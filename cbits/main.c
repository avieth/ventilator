/**
 * This is a very simple demo of how the copilot Haskell EDSL can be used to
 * build a C program suitable for an embedded device, in the context of this
 * mechanical ventilator project.
 *
 * This program demonstrates the basic functionality by simulating "sensor data"
 * according to a very simple model, stepping the system, and displaying plots
 * of volume, flow, and pressure using a crude horrible ncurses interface.
 *
 * In a real embedded application, the loop would look much the same, except
 * that the input would actually be sampled from sensors and the callbacks for
 * set_flow and alarm would actually do what you expect.
 *
 * Follow through comments inline.
 */

#include <unistd.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <ncurses.h>
#include <math.h>

/**
 * This is the header file generated by copilot.
 */
#include "ventilator.h"

/**
 * For simulation, we advance by 1ms per step.
 */
#define TIME_DELTA_MS 1

/**
 * The following block of declarations are each declared extern in ventilator.h,
 * determined by the Haskell copilot program.
 *
 * Here we give initial values for each of them, and we update them according
 * to simulation in set_flow (update_model).
 *
 * Of course, in a real deployment they would be updated by the input routine
 * whenever sensors are sampled.
 */

// Controls
uint8_t c_bpm = 12;
uint16_t c_ie_ratio = 0x0102;
uint32_t t_delta_ms = 1;
uint32_t t_delta_us = 1000;
bool c_cmv_mode = true;
uint32_t c_volume_limit = 1000000;
uint32_t c_pressure_limit = 5000;
uint32_t c_cmv_volume_goal = 500000;
uint32_t c_cmv_pressure_goal = 3000;
uint32_t c_peep = 200;

// Observed things (computed, since this is a simulation)

// uL/ms
int32_t s_flow = 0;
int32_t s_internal_pressure_1 = 0;
int32_t s_internal_pressure_2 = 0;
int32_t s_pressure = 0;
// uL
int32_t s_volume = 0;

bool s_piston_low = true;
bool s_piston_high = false;

int32_t s_encoder_position = 0;

/**
 * Some constants and variables for simulation.
 *
 * Given the motor velocity at an instant (int8_t) the simulation will update
 * the flow, pressure, and volume sensor values, as well as the motor high and
 * low sensors.
 *
 * It's a simple/naive simulation:
 *
 * - flow is a linear function of velocity
 * - there is no loss of volume/pressure: change in volume = flow
 * - change in pressure is a function of flow by way of constant lung compliance
 *
 * The simulation is computed with floating point arithmetic because it's just
 * easier to express that way.
 */

/**
 * Length of the cylinder in which the piston moves. 15cm (in um).
 */
const int32_t cylinder_length_um = 150000;

/**
 * The simulation computes the position of the piston in um from the origin
 * (fully retracted). Same units as cylinder_length of course.
 */
int32_t piston_position_um = 0.0;

// TODO to make the simulation viable, the flow needs to be somewhat continuous.
// Currently it's not because acceleration of the piston is instantaneous.
// Quick and dirty way to do that? Have an int32_t which begins at 0 and
// ramps up/down to the desired motor velocity at a given rate, then compute
// the change in position from this!

/**
 * Displacement of air per change in piston position (uL/um).
 * Set to 7 so that the piston can displace 1.05L in a full cycle from 0 to
 * cylinder_length.
 */
const int32_t displacement_ul_um = 7;

/**
 * How fast the piston moves in um/ms at full duty cycle (magnitude 127 motor
 * velocity signal). Speeds at lower magnitudes are linearly interpolated.
 *
 * It's defined such that, at full power, the piston can traverse the cylinder
 * in 500ms.
 */
const int32_t piston_speed_um_ms = 300;

/**
 * Lung compliance, for a simulated model of pressure.
 *
 * This is the change in volume over the change in pressure.
 * Unit is microlitres over pascals.
 *
 * Apparently anywhere from 100mL/cmH2O to 400mL/cmH2O is a reasonable lung
 * compliance. We'll take the midpoint 250mL/cmH2O.
 * But that's not consistent with what I've seen/read about ventilators:
 * apparently 20-30 cmH2O is a common desirable pressure, and 500mL a
 * desirable tidal volume, but to get 20-30cmH2O you would apparently need
 * way more volume than that...
 *
 *   ~ 2.55mL/Pa
 *
 *
 * i.e. for every approximately 2.5 mL you put in the lung, you get a ~100 Pa
 * increase in pressure.... that can't be right though... we expect pressure
 * to be in the 10-30 cmH2O range on inhale, but that's around 1000 to 3000
 * Pa. Something is all wrong here...
 *
 * TODO surely compliance decreases as volume increases. May be good to include
 * this in the simulation?
 */
//const double compliance = 2549.290532;
const int32_t compliance_ul_pa = 150;

/**
 * Intrinsic positive end-expiratory pressure in cmH2O
 */
const int32_t intrinsic_peep_cmh2o = 0;

/**
 * Updates the simulation model according to the flow value at an instant.
 *
 * In practice, we'll take the data from sensors. But for simulation we assume
 * the desired flow (computed by the copilot program) is identical to the
 * observed flow, and we integrate to get the volume.
 *
 * Pressure is computed according to the compliance.
 *
 * O2 concentration is never changed.
 *
 * TODO change this so that flow is computed from motor velocity.
 * Define some constant of how much air it displaces per mm moved, and how
 * far it moves according to the velocity magnitude in [0,127].
 * Would also need the size of the cylinder so we can simulate the high/low
 * sensors.
 *
 * One problem with having the flow change instantaneously: 
 */
void update_model(bool in_motor_pulse, bool in_motor_direction) {
  /*
  static int8_t actual_motor_velocity = 0;
  if (actual_motor_velocity > in_motor_velocity) {
    actual_motor_velocity--;
  } else if (actual_motor_velocity < in_motor_velocity) {
    actual_motor_velocity++;
  }

  // Compute the change in piston position. We have the speed in um/ms at full
  // duty cycle (in_motor_velocity is magnitude 127), and the time delta in ms,
  // so we linearly interpolate the actual speed and multiply by the time
  // delta.
  int32_t motor_velocity_32 = (int32_t) in_motor_velocity;
  int32_t motor_velocity_interp = (motor_velocity_32 * 1000) / 127;
  int32_t d_position_um_ms = (motor_velocity_interp * piston_speed_um_ms) / 1000;
  int32_t d_position_um = t_delta_ms * d_position_um_ms;

  piston_position_um += d_position_um;
  s_piston_low = false;
  s_piston_high = false;
  if (piston_position_um >= cylinder_length_um) {
    s_piston_high = true;
    d_position_um = d_position_um - (piston_position_um - cylinder_length_um);
    piston_position_um = cylinder_length_um;
  }
  if (piston_position_um <= 0) {
    s_piston_low = true;
    // Written like this for clarity and symmetry with the previous if
    // block.
    d_position_um = d_position_um + (0 - piston_position_um);
    piston_position_um = 0;
  }

  // Flow is supposed to be uL/ms. We have the change in position and the
  // displacement, giving uL, so we must divide by the t_delta_ms.
  // Not a good idea in general but here it's 1 so whatever.
  s_flow = (displacement_ul_um * d_position_um) / ((int32_t) t_delta_ms);
  // Volume is uL.
  s_volume += (((int32_t) t_delta_ms) * s_flow);
  s_pressure += ((((int32_t) t_delta_ms) * s_flow) / compliance_ul_pa);
  s_internal_pressure_1 = s_pressure;
  s_internal_pressure_2 = s_pressure;
  */
}

/**
 * Also declared in ventilator.h, this routine shall be called with the motor
 * velocity which the spec has decided upon. This routine would be required to
 * write that value to hardware so as to drive the motor.
 */
void control_motor(bool in_motor_pulse, bool in_motor_direction, double in_volume) {
  update_model(in_motor_pulse, in_motor_direction);
}

void raise_alarm(void) {
  // TODO set an alarm global and check it as part of the UI.
  // For now, kill the ncurses app, print a message and exit.
  endwin();
  printf("Alarm raised. Bye.");
  exit(1);
}

#define PLOT_TIME_STEP 200000

/**
 * We'll display the total elapsed time (microseconds).
 * No bother about overflow, this is just a demo.
 */
uint32_t time_us = 0;


void input(void) {
  int ch = getch();
  if (ch == ERR) {
    return;
  }
  uint16_t i_bits = 0x0000;
  uint16_t e_bits = 0x0000;
  // TODO these checks not needed. The logic does limits. We can modify these
  // but read out from derived signals given to update_ui.
  switch (ch) {
    case 's':
      c_bpm = (c_bpm <= 6) ? 6 : c_bpm - 1;
      break;
    case 'w':
      c_bpm = (c_bpm >= 255) ? 255 : c_bpm + 1;
      break;
    case 'd':
      c_cmv_volume_goal = (c_cmv_volume_goal <= 1000) ? 1000 : c_cmv_volume_goal - 1000;
      break;
    case 'e':
      c_cmv_volume_goal = (c_cmv_volume_goal >= 5000000) ? 5000000 : c_cmv_volume_goal + 1000;
      break;
    case 'f':
      c_cmv_pressure_goal = (c_cmv_pressure_goal <= 1000) ? 100 : c_cmv_pressure_goal - 100;
      break;
    case 'r':
      c_cmv_pressure_goal = (c_cmv_pressure_goal >= 5000000) ? 5000000 : c_cmv_pressure_goal + 100;
      break;

    case 'g':
      i_bits = c_ie_ratio >> 8;
      i_bits = (i_bits == 1) ? i_bits : (i_bits - 1);
      i_bits = i_bits << 8;
      c_ie_ratio = (c_ie_ratio & 0x00FF) | i_bits;
      break;
    case 't':
      i_bits = c_ie_ratio >> 8;
      i_bits = (i_bits == 255) ? i_bits : (i_bits + 1);
      i_bits = i_bits << 8;
      c_ie_ratio = (c_ie_ratio & 0x00FF) | i_bits;
      break;
    case 'h':
      e_bits = c_ie_ratio & 0x00FF;
      e_bits = (e_bits == 1) ? e_bits : (e_bits - 1);
      c_ie_ratio = (c_ie_ratio & 0xFF00) | e_bits;
      break;
    case 'y':
      e_bits = c_ie_ratio & 0x00FF;
      e_bits = (e_bits == 255) ? e_bits : (e_bits + 1);
      c_ie_ratio = (c_ie_ratio & 0xFF00) | e_bits;
      break;

    case 'm':
      c_cmv_mode = (c_cmv_mode == true) ? false : true;
      break;
    case 'Q':
    case 'q':
      endwin();
      exit(1);
      break;
  }
}

/**
 * This is just a big messy rounte to display plots using ncurses.
 *
 * Prints 3 plots stacked vertically: volume, flow, pressure.
 *
 * Since a terminal is relatively low-res and in fact not at all suitable
 * for plotting, we'll only plot a point once every 200ms. Plotting at a faster
 * rate means the curve appears flatter and that an entire breath cycle
 * probably won't be visible. With 80 columns, plotting once every 10ms gives
 * only 800ms, but every 200ms gives 16 seconds which is probably enough to see
 * an entire breath cycle.
 */
void update_ui(
    double in_desired_flow
  , bool in_motor_pulse
  , bool in_motor_direction
  , double in_volume
  , bool in_piston_high
  , bool in_piston_low
  , double piston_position
  , uint8_t in_bpm
  , uint32_t in_volume_limit
  , uint32_t in_pressure_limit
  , uint8_t in_ie_inhale
  , uint8_t in_ie_exhale
  , bool in_cmv_mode
  , uint32_t in_cmv_volume_goal
  , uint32_t in_cmv_pressure_goal
  , uint32_t in_global_volume_max
  , uint32_t in_global_volume_min
  , uint32_t in_global_pressure_max
  , uint32_t in_global_pressure_min
  ) {

  int height, width;
  getmaxyx(stdscr, height, width);
  int row = 0;

  // We need at least 10 rows per-plot, plus 3 rows for the plot headings, 2
  // rows for the main heading, and 2 rows for the footer (not implemented).
  if (height < 37) {
    mvprintw(row, 0, "Window too small");
    refresh();
    return;
  };

  move(row, 0);
  clrtoeol();
  if (in_cmv_mode == true) {
    mvprintw(row, 0, "Volume control target %d.%03d mL", in_cmv_volume_goal / 1000, abs(in_cmv_volume_goal % 1000));
  } else {
    mvprintw(row, 0, "Pressure control target %d.%03d cm H2O", (in_cmv_pressure_goal * 1000) / 98000, abs(((in_cmv_pressure_goal * 1000) / 98) % 1000));
  }

  row += 1;
  move(row, 0);
  clrtoeol();
  mvprintw(row, 0,
      "BPM: %i, I:E set %i:%i, I:E actual %i:%i, time: %i ms, desired flow: %03i, piston position: %e",
      in_bpm,
      (c_ie_ratio >> 8),
      (c_ie_ratio & 0xFF),
      in_ie_inhale,
      in_ie_exhale,
      time_us / 1000,
      in_desired_flow,
      piston_position_um
  );
  row += 1;

  int plot_height = (height - 7) / 3;
  int plot_width = width - 5;

  // Print headings for each plot.
  // We choose units that are apparently common on real ventilators:
  //   Volume in mL
  //   Flow in L/min
  //   Pressure in cm H2O
  int volume_header_row = row;
  int flow_header_row = volume_header_row + plot_height + row;
  int pressure_header_row = flow_header_row + plot_height + row;
  move(volume_header_row, 0);
  clrtoeol();
  // Volume is in uL so getting mL with 4 decimal points is simple.
  mvprintw(volume_header_row, 0, "Volume: %d.%03d mL", s_volume / 1000, abs(s_volume % 1000));
  move(flow_header_row, 0);
  clrtoeol();
  // Flow is in uL/ms, or equivalently mL/s.
  int32_t flow_ml_min = s_flow * 60;
  mvprintw(flow_header_row, 0, "Flow: %d.%03d L/min", flow_ml_min / 1000, abs(flow_ml_min % 1000));
  move(pressure_header_row, 0);
  clrtoeol();
  // Pressure is in Pa, and there are approx. 98 per cm H2O. We'll first multiply
  // by 1000 so we can get the decimal places.
  mvprintw(pressure_header_row, 0, "Pressure: %d.%03d cm H2O", (s_pressure * 1000) / 98000, abs(((s_pressure * 1000) / 98) % 1000));

  // For each plot we have a top row. The plot's row space is this up to
  // this plus plot_height.
  int volume_plot_y = volume_header_row + 1;
  int flow_plot_y = flow_header_row + 1;
  int pressure_plot_y = pressure_header_row + 1;

  // The column for the current frame is special.
  // The value at `sample` goes at this column.
  int now_column = (time_us / PLOT_TIME_STEP) % plot_width;
  row = 0;

  int volume_step = in_global_volume_max / plot_height;
  for (int i = 0; i < plot_height; ++i) {
    row = volume_plot_y + plot_height - i;
    if (s_volume >= (i * volume_step) && s_volume < ((i + 1) * volume_step)) {
      mvprintw(row, now_column, "+");
    } else {
      mvprintw(row, now_column, " ");
    }
    mvprintw(row, (now_column + 1) % plot_width, "|");
  }

  // TODO have an input max flow given
  int flow_step = 1024 / plot_height;
  int min_flow = -512;
  for (int i = 0; i < plot_height; ++i) {
    row = flow_plot_y + plot_height - i;
    if (s_flow >= (i * flow_step + min_flow) && s_flow < ((i + 1) * flow_step + min_flow)) {
      mvprintw(row, now_column, "+");
    } else {
      mvprintw(row, now_column, " ");
    }
    mvprintw(row, (now_column + 1) % plot_width, "|");
  }

  int pressure_step = in_global_pressure_max / plot_height;
  int min_pressure = in_global_pressure_min;
  for (int i = 0; i < plot_height; ++i) {
    row = pressure_plot_y + plot_height - i;
    if (s_pressure >= (i * pressure_step + min_pressure) && s_pressure < ((i + 1) * pressure_step + min_pressure)) {
      mvprintw(row, now_column, "+");
    } else {
      mvprintw(row, now_column, " ");
    }
    mvprintw(row, (now_column + 1) % plot_width, "|");
  }

  // Print a visualization of the motor velocity and piston at the right hand
  // side.
  // We can use all of the space on the right margin except for the top
  //
  // 2 rows of header.
  int rhs_height = height - 2;

  int col = width - 2;
  int midpoint = rhs_height / 2;
  /*int v_step = 256 / rhs_height;
  int v_motor = (int) in_motor;
  for (int i = 0; i < midpoint; ++i) {
    if (v_motor >= 0) {
      row = 2 + midpoint - i;
      if (v_motor >= (i * v_step) && (v_motor <= (i + 1) * v_step || i == midpoint - 1)) {
        mvprintw(row, col, "^");
      } else if (v_motor >= (i * v_step)) {
        mvprintw(row, col, "|");
      } else {
        mvprintw(row, col, " ");
      }
      row = 2 + midpoint + i;
      mvprintw(row, col, " ");
    } else {
      row = 2 + midpoint + i;
      if (v_motor <= (i * -v_step) && (v_motor >= (i + 1) * -v_step || i == midpoint - 1)) {
        mvprintw(row, col, "v");
      } else if (v_motor <= (i * -v_step)) {
        mvprintw(row, col, "|");
      } else {
        mvprintw(row, col, " ");
      }
      row = 2 + midpoint - i;
      mvprintw(row, col, " ");
    }
  }*/

  col = width - 1;
  int piston_step = cylinder_length_um / height;
  int piston_position_i = piston_position_um;
  for (int i = 0; i < rhs_height; ++i) {
    row = rhs_height - i;
    if (piston_position_um >= (i * piston_step)) {
      mvprintw(row, col, "#");
    } else {
      mvprintw(row, col, " ");
    }
  }

  refresh();

}

void main() {

  // Set up ncurses
  initscr();
  // Raw character input ...
  raw();
  // ... and respect CTRL+C
  cbreak();
  // No terminal cursor
  curs_set(0);
  // No delay for input
  nodelay(stdscr, true);

  // Main loop: run the simulation and display the current state.
  while (1) {
    // TODO input could come from the step as well, so long as it's nonblocking.
    input();
    step();
    time_us += (1000 * t_delta_ms);
    if (usleep(t_delta_ms * 1000) != 0) { break; }
  }

  // Tear down ncurses and exit.
  endwin();
  exit(0);
}
