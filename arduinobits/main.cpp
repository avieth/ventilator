#include <Arduino.h>
#include <Encoder.h>
#include <math.h>
#include "pins.h"
#include "sensor_control.h"
#include "ventilator.h"
#include "display.h"
#include "display_modes.h"
#include "input.h"

Encoder encoder(PIN_ENC_0, PIN_ENC_1);

int32_t s_encoder_position = 0;
bool s_limit_low = false;
bool s_limit_high = false;

/**
 * Operator controls (c_ prefix).
 * These are read by the ventilator system logic.
 */
//uint8_t c_mode = 0; // CMV
uint8_t c_mode = 1; // SIMV
bool c_button_start = false;
bool c_button_stop = false;
uint8_t c_bpm = 12;
uint8_t c_ie_inhale = 0x01;
uint8_t c_ie_exhale = 0x02;
bool c_cmv_mode = false; // CMV volume control
uint32_t c_volume_limit = 1000;
uint32_t c_pressure_limit = 5000;
uint32_t c_cmv_volume_goal = 500; //mL
uint32_t c_cmv_pressure_goal = 2000; //Pa
uint32_t c_peep = 200;

/**
 * Globals for sensor data.
 * See update_sensors();
 * TODO must these use floating points really?
 * TODO units?
 */
int32_t s_insp_pressure_1 = 0;
int32_t s_insp_pressure_2 = 0;
int32_t s_insp_flow_1 = 0;
int32_t s_insp_flow_2 = 0;
int32_t s_exp_flow_1 = 0;
int32_t s_exp_flow_2 = 0;
int32_t s_air_in_flow_1 = 0;
int32_t s_air_in_flow_2 = 0;

/**
 * The time leapsed (in microseconds) since the last loop() call.
 * It is set by update_time(), which is the first thing to be called in loop().
 * It is also read by the system logic in step().
 */
uint32_t t_delta_us = 0;

/**
 * Sets t_delta_us.
 */
void update_time(uint32_t now_us) {
  static long last_us = 0;
  t_delta_us = now_us - last_us;
  last_us = now_us;
}

/**
 * TODO move all of the UI stuff to separate translation units.
 */

/**
 * Will be updated by input_poll through the encoderPosition callback.
 */
int32_t ui_encoder_position = 0;

/**
 * Called not by an ISR but by polling (encoder library doesn't offer an
 * interrupt interface to users?).
 */
void uiEncoderPosition(int32_t pos) {
  ui_encoder_position = pos;
};

/**
 * Set on setup() and by ui_loop.
 */
displayDataInput* ui_focus;

/**
 * Set by button interrupts.
 */
bool ui_editing = false;

displayData display_data = {
  .state = 0,
  .mode = c_mode,
  .bpm = c_bpm,
  .ieInhale = c_ie_inhale,
  .ieExhale = c_ie_exhale,
  .tidalVolume = 0,
  .pressurePeak = 0,
  .peep = 200,
  .oxygen = 100
};

/**
 * References to which the UI will write.
 */
displayWriteData write_data = {
  .mode = &c_mode,
  .bpm = &c_bpm,
  .ieInhale = &c_ie_inhale,
  .ieExhale = &c_ie_exhale,
  .volume = &c_cmv_volume_goal,
  .pressure = &c_cmv_pressure_goal
};

#define BUTTON_DEBOUNCE 250000

/**
 */
void buttonLeft(bool pressed) {
  if (pressed) {
    c_button_stop = true;
  } else {
    c_button_stop = false;
  }
  if (ui_editing) {
    ui_focus->deselect(false, &write_data);
    ui_editing = false;
  }
};

/**
 */
void buttonRight(bool pressed) {
  if (pressed) {
    c_button_start = true;
  } else {
    c_button_start = false;
  }
  if (ui_editing) {
    ui_focus->deselect(false, &write_data);
    ui_editing = false;
  }
};



/**
 * The main button is used to toggle editing of fields.
 */
void buttonMain(bool pressed) {
  static uint32_t last_us = 0;
  uint32_t now_us = micros();

  if ((now_us - last_us) < BUTTON_DEBOUNCE) {
    return;
  } else {
    last_us = now_us;
  }
  if (pressed) {
    if (ui_editing) {
      ui_focus->deselect(true, &write_data);
      ui_editing = false;
    } else {
      ui_focus->select(&display_data, &write_data);
      ui_editing = true;
    }
  }
};

/**
 * Limits how often ui_loop runs.
 * Go for at most ~60Hz.
 *
 * NB: this is not the _draw_ limit. Drawing is done asynchronously and in
 * a staggered way, since it's so slow.
 *
 * NB also: the rate of this loop affects input responsiveness, the encoder
 * in particular, since we don't get interrupts when it changes (even though
 * interrupts are used to keep its value up-to-date).
 */
#define UI_LOOP_LIMIT 16666

/**
 * Limit on how often we check the encoder. It's really fickle so this needs
 * TO be slower than the UI_LOOP_LIMIT
 */
#define UI_LOOP_ENCODER_LIMIT 150000

void ui_loop(uint32_t now_us) {

  /**
   * First, rate limiting.
   */
  static uint32_t accumulator_us = 0;
  static uint32_t accumulator_encoder_us = 0;
  static uint32_t last_us = 0;
  uint32_t delta_us = now_us - last_us;
  accumulator_us += delta_us;
  accumulator_encoder_us += delta_us;
  last_us = now_us;
  bool do_loop = accumulator_us >= UI_LOOP_LIMIT;
  bool do_encoder = accumulator_encoder_us >= UI_LOOP_ENCODER_LIMIT;
  if (!do_loop) {
    return;
  } else {
    accumulator_us = 0;
  }

  /**
   * Speaker and LEDs
   *
   * Our decision here depends only on the status in the display state
   * (ultimately determined by the ventilator spec logic passed through
   * update_ui calls).
   */
  if (display_data.state == DISPLAY_STATE_STOPPED || display_data.state == DISPLAY_STATE_ERROR) {
    display_speaker(true);
    display_led_red(true);
  } else if (display_data.state == DISPLAY_STATE_CALIBRATING) {
    display_led_green_flashing(now_us, 250000);
  } else if (display_data.state == DISPLAY_STATE_READY) {
    display_led_green_flashing(now_us, 1000000);
  } else if (display_data.state == DISPLAY_STATE_RUNNING) {
    display_led_green(true);
  }

  /**
   * LCD display
   *
   * This is determined not only by the ventilator spec logic, which gives
   * the true values (update_ui), but also depends upon user input. The
   * simple rule is: user input only has an effect when in the ready or
   * running states. User input state is reset if the state changes to not
   * one of these.
   */

  /**
   * We _always_ start by drawing the display data. We will overlay things if
   * necessary.
   */
  display_format_running(&display_data);

  /**
   * For all states other than ready or running, the display is completely
   * determined by the display data (not user input interactions).
   *
   */
  if ((display_data.state != DISPLAY_STATE_READY) && (display_data.state != DISPLAY_STATE_RUNNING)) {
    return;
  }

  ui_focus->highlight(ui_editing);

  // Must always show the overlay for editing things.
  if (ui_editing) {
    ui_focus->overlay();
  }

  /**
   * Here we drop out early so as to ignore the bad UX of the encoder flopping
   * and flailing around.
   * Everything past this point is solely response to the encoder position
   * change.
   */
  if (!do_encoder) {
    return;
  } else {
    accumulator_encoder_us = 0;
  }

  /**
   * Compute changes in the encoder position always.
   * TODO FIXME better way to deal with this? Is it even appropriate to do it
   * in this routine?
   * NB: by doing it here at the top, it means that any changes to the encoder
   * position which happen in a non-input-accepting state (like error) do not
   * carry forward to normal states, which is definitely a good thing.
   */
  static int32_t prev_encoder_position = 0;
  int32_t encoder_delta = ui_encoder_position - prev_encoder_position;
  prev_encoder_position = ui_encoder_position;

  /**
   * Any encoder movement causes a reaction:
   * - If we are editing something, change it according to its own peculiarities
   * - Otherwise the encoder changes the focus. The size of the delta does not
   *   matter, you never jump over more than one.
   */
  if (encoder_delta != 0) {
    if (ui_editing) {
      ui_focus->change(encoder_delta);
    } else {
      ui_focus = next_display_data(encoder_delta > 0);
    }
  }

}



void setup() {
  SerialUSB.begin(115200);

  display_setup();
  inputCallbacks cbs = inputCallbacks {
    .buttonLeft = buttonLeft,
    .buttonRight = buttonRight,
    .buttonMain = buttonMain,
    .encoderPosition = uiEncoderPosition
  };
  input_setup(cbs);
  ui_focus = get_display_data(0);

  /**
   * TODO encapsulate these all together in initializeSensors?
   */
  pinMode(PIN_MOTOR_STEP, OUTPUT);
  pinMode(PIN_MOTOR_DIRECTION, OUTPUT);
  digitalWrite(PIN_MOTOR_DIRECTION, HIGH);
  pinMode(PIN_LIMIT_SWITCH_LOWER, INPUT_PULLUP);
  pinMode(PIN_LIMIT_SWITCH_UPPER, INPUT_PULLUP);
  encoder.write(0);
  s_encoder_position = 0;
  initializeSensors();
}

/**
 * Desired motor direction. true for HIGH false for LOW on the direction pin.
 * true/HIGH means the encoder value will increase.
 */
bool motor_direction = true;

/**
 * How many microseconds between motor pulses.
 * Set to 0 to mean no movement.
 * This, along with the pulses per revolution of the particular hardware,
 * determines the speed of revolution. It is also limited by hardware/physics.
 * Anything lower than 400 is probably not feasible.
 */
unsigned long pulse_us = 0;

/**
 * Set the motor pulse length and direction.
 * Encoding is simple: magnitude give the us per pulse, sign gives the direction.
 */
void control_motor(long in_us_per_pulse) {
  if (in_us_per_pulse >= 0) {
    pulse_us = in_us_per_pulse;
    motor_direction = true;
  } else {
    pulse_us = -in_us_per_pulse;
    motor_direction = false;
  }
}

/**
 * Pulses the motor according to the elapsed time and the interval between pulses.
 * This is called at every step. It's essential that it is called as often as possible,
 * in order to give good motor resolution. Must not be called until update_time() has been
 * called, as it uses the t_delta_us global.
 */
void step_motor(uint32_t now_us) {
  /**
   * Accumulator in microseconds. We add the time elapsed since the last pulse to this,
   * and whenever it exceeds the pulse_us we step the motor. Leftovers are carried
   * forward, so that we can best approximate the desired pulse rate given that we don't
   * have a hard realtime scheduling guarantee (we share this processor with the
   * ventilator logic).
   */
  static unsigned long accumulator_us = 0;
  /**
   * Time in microseconds of the last call to this function.
   * Defined so that we can compute the time delta between calls.
   */
  static unsigned long last_call_us = 0;

  unsigned long delta_us = now_us - last_call_us;
  last_call_us = now_us;

  /**
   * Always check for changes in direction.
   */
  static bool current_direction = true;
  if (motor_direction != current_direction) {
    current_direction = motor_direction;
    if (current_direction) {
      digitalWrite(PIN_MOTOR_DIRECTION, HIGH);
    } else {
      digitalWrite(PIN_MOTOR_DIRECTION, LOW);
    }
  }
  
  if (pulse_us == 0) {
    // We never accrue any debt if the motor is meant to be stationary.
    accumulator_us = 0;
  } else {
    // If the motor is meant to move, we bump up the accumulated debt and pulse
    // whenever it exceeds. Debt carries forward so that a late pulse might be
    // made up for in subsequent calls. This also allows us to identify underruns.
    // TODO FIXME deal with microsecond unsigned long overflow, else we'll get false
    // underrun positives.
    accumulator_us = accumulator_us + delta_us;
    if (accumulator_us >= pulse_us) {
      digitalWrite(PIN_MOTOR_STEP, LOW);
      digitalWrite(PIN_MOTOR_STEP, HIGH);
      accumulator_us = accumulator_us - pulse_us;
      unsigned long missed = accumulator_us / pulse_us;
      if (missed > 0) {
        SerialUSB.print("motor underrun: missed ");
        SerialUSB.print(missed);
        SerialUSB.println(" steps(s)");
        // The serial print takes a lot of time. In this case let's reset the
        // last call time so that one underrun doesn't artifically cause further
        // underruns due to the serial print.
        accumulator_us = 0;
        last_call_us = micros();
      }
    }
  }
}

/**
 * To be called as often as, and immediately before, the sytsem logic is stepped
 * (by a call to step_system).
 */
void update_sensors() {
  // TODO FIXME
  // Am I doing direction wrong? I find that forward (positive) direction makes the encoder
  // decrease.
  s_encoder_position = -encoder.read();
  s_limit_low  = digitalRead(PIN_LIMIT_SWITCH_LOWER) == LOW;
  s_limit_high = digitalRead(PIN_LIMIT_SWITCH_UPPER) == LOW;

  /** 
   *  Sample all of the sensors for this frame.
   *  No redundant sensor hardware at the moment so we just copy the values.
   */
  s_insp_pressure_1 = get_insp_pressure_i();
  s_insp_pressure_2 = s_insp_pressure_2;
  s_insp_flow_1 = get_insp_flow_i();
  s_insp_flow_2 = s_insp_flow_1;
  s_exp_flow_1 = get_exp_flow_i();
  s_exp_flow_2 = s_exp_flow_1;
  s_air_in_flow_1 = get_air_in_flow_i();
  s_air_in_flow_2 = s_air_in_flow_1;
}

/**
 * Updating the system is resource-intensive.
 * 
 * With such a pathetically slow board as the arduino uno, we must not do this too often, as it
 * can upset the motor control, which relies upon being called sufficiently often in order to give
 * accurate movement.
 */
#define STEP_SYSTEM_INTERVAL_US 1000

/**
 * Call into the ventilator system logic by way of step(), at most one every STEP_SYSTEM_INTERVAL_US
 * microseconds. This also updates the sensors immediately before that call.
 */
void step_system() {
  static unsigned long time_since_last_update_us = 0;
  time_since_last_update_us += t_delta_us;
  if (time_since_last_update_us >= STEP_SYSTEM_INTERVAL_US) {
    update_sensors();
    unsigned long hold = t_delta_us;
    // Must set this because the system step() uses it.
    t_delta_us = time_since_last_update_us;
    step();
    t_delta_us = hold;
    time_since_last_update_us = 0;
  }
}

#define DEBUG_INTERVAL 100000

void debug(uint32_t now_us) {
  static unsigned long debug_elapsed_us = 0;
  debug_elapsed_us += t_delta_us;
  if (debug_elapsed_us >= DEBUG_INTERVAL) {
    debug_elapsed_us = 0;
  }
}

/**
 * Values to be displayed are determined _solely_ by this function (its parameters).
 * The UI can of course indicate changes to these values, but the actual values must be
 * determined by the ventilator program.
 * 
 * The operator control variables (globals, c_ prefix) are modified by the UI input
 * part. It's these values which the display state tracks. However, these are not the
 * values which are necessarily displayed. They are always displayed when editing that
 * value, but if it's not the value that the ventilator logic decides to use (maybe it's
 * nonsense) then it will not be displayed.
 */
void update_ui(uint8_t in_state, uint8_t in_mode, long in_flow, long in_volume, long in_pressure, unsigned char in_bpm, unsigned char in_inhale_ratio, unsigned char in_exhale_ratio, bool in_cmv_mode, unsigned long in_cmv_volume_goal, unsigned long in_cmv_pressure_goal) {
  display_data.state = in_state;
  display_data.mode = in_mode;
  display_data.bpm = in_bpm;
  display_data.ieInhale = in_inhale_ratio;
  display_data.ieExhale = in_exhale_ratio;
  display_data.tidalVolume = in_volume;
  display_data.volumeLimit = in_cmv_volume_goal;
  display_data.pressurePeak = in_pressure;
  display_data.pressureLimit = in_cmv_pressure_goal;
  display_data.peep = 200;
  display_data.oxygen = 100;
}

/**
 * Main loop: track time, update the system, and crucially: call into step_motor as often as possible.
 */
void loop() {
  uint32_t now_us = micros();
  update_time(now_us);
  step_system();
  step_motor(now_us);
  input_poll(now_us);
  ui_loop(now_us);
  display_draw(now_us);
  debug(now_us);
}
