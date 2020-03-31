#include <Encoder.h>
#include "pins.h"
#include "sensor_control.h"
#include "display_control.h"
#include "input_control.h"
#include "ventilator.c"

Encoder encoder(PIN_ENC_0, PIN_ENC_1);

int32_t s_encoder_position = 0;
bool s_limit_low = false;
bool s_limit_high = false;

/**
 * Operator controls (c_ prefix).
 * These are read by the ventilator system logic.
 */
uint8_t c_bpm = 12;
uint8_t c_ie_inhale = 0x01;
uint8_t c_ie_exhale = 0x02;
bool c_cmv_mode = true;
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
void update_time() {
  static long last_micros = 0;
  static long current_micros = 0;
  current_micros = micros();
  t_delta_us = current_micros - last_micros;
  last_micros = current_micros;
}

void setup() {
  Serial.begin(115200);
  pinMode(PIN_MOTOR_STEP, OUTPUT);
  pinMode(PIN_MOTOR_DIRECTION, OUTPUT);
  digitalWrite(PIN_MOTOR_DIRECTION, HIGH);
  pinMode(PIN_LIMIT_SWITCH_LOWER, INPUT_PULLUP);
  pinMode(PIN_LIMIT_SWITCH_UPPER, INPUT_PULLUP);
  initializeSensors();
  // Give the display state the relevant pointers, so it can edit them.
  display_state *displayState = setup_display(c_bpm, c_cmv_volume_goal, c_cmv_pressure_goal, c_ie_inhale, c_ie_exhale);
  // TODO FIXME should not need to set up inputs and recordings explicitly
  // here; should be encapsulated in ui_setup or something.
  inputs_setup(displayState, &c_bpm, &c_cmv_volume_goal, &c_cmv_pressure_goal, &c_ie_inhale, &c_ie_exhale);
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
void step_motor() {
  static unsigned long time_since_last_pulse_us = 0;
  static bool current_direction = true;

  /**
   * Some debugging code: it's good to know if the motor stepper is "underrunning", i.e. not
   * being called often enough, for instance because the system step function takes too long
   * (floating point math p hard).
   */
  static unsigned long time_at_last_step_motor_us = 0;
  static unsigned long now_us = micros();
  if ((pulse_us != 0) && ((now_us - time_at_last_step_motor_us) > pulse_us)) {
    Serial.println("motor control underrun");
    now_us = micros();
  }
  time_at_last_step_motor_us = now_us;
  
  if (motor_direction != current_direction) {
    current_direction = motor_direction;
    if (current_direction) {
      digitalWrite(PIN_MOTOR_DIRECTION, HIGH);
    } else {
      digitalWrite(PIN_MOTOR_DIRECTION, LOW);
    }
  }
  time_since_last_pulse_us += t_delta_us;
  // 0 means "do not move"; it does not mean "move infinitely fast".
  if (pulse_us == 0) {
    return;
  } else if (time_since_last_pulse_us >= pulse_us) {
    digitalWrite(PIN_MOTOR_STEP, LOW);
    digitalWrite(PIN_MOTOR_STEP, HIGH);
    time_since_last_pulse_us = 0;
  }
}

/**
 * To be called as often as, and immediately before, the sytsem logic is stepped.
 */
void update_sensors() {
  // TODO FIXME
  // Am I doing direction wrong? I find that forward (positive) direction makes the encoder
  // decrease.
  s_encoder_position = -encoder.read();
  s_limit_low = digitalRead(PIN_LIMIT_SWITCH_LOWER) == LOW;
  s_limit_high = digitalRead(PIN_LIMIT_SWITCH_UPPER) == LOW;

  /** 
   *  Sample the sensors and round them to integers.
   *  We give 1000 times the values before rounding.
   *  
   *  No redundant sensors so we just copy the values.
   */
  s_insp_pressure_1 = lroundf(get_insp_pressure());
  s_insp_pressure_2 = s_insp_pressure_2;
  if (s_insp_pressure_1 > 0) {
    Serial.println(s_insp_pressure_1);
  }
  s_insp_flow_1 = lroundf(get_insp_flow());
  s_insp_flow_2 = s_insp_flow_1;
  s_exp_flow_1 = lroundf(get_exp_flow());
  s_exp_flow_2 = s_exp_flow_1;
  s_air_in_flow_1 = lroundf(get_air_in_flow());
  s_air_in_flow_2 = s_air_in_flow_1;
}

/**
 * To be triggered whenever the motor low switch is on (is LOW).
 */
void zero_encoder() {
  encoder.write(0);
  s_encoder_position = 0;
}

void calibration_change(unsigned char in_c_phase, double in_theta, double in_volume) {
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

void debug() {
  static unsigned long debug_elapsed_us = 0;
  debug_elapsed_us += t_delta_us;
  if (debug_elapsed_us >= DEBUG_INTERVAL) {
    // Put periodic debug code here if you desire.
    debug_elapsed_us = 0;
  }
}

/**
 * Values to be displayed are determines _solely_ by this function (its parameters).
 * The UI can of course indicate changes to these values, but the actual values must be
 * determined by the ventilator program.
 * 
 * The operator control variables (globals, c_ prefix) are modified by the UI input
 * part. It's these values which the display state tracks. However, these are not the
 * values which are necessarily displayed. They are always displayed when editing that
 * value, but if it's not the value that the ventilator logic decides to use (maybe it's
 * nonsense) then it will not be displayed.
 */
void update_ui(long in_flow, long in_volume, long in_pressure, unsigned char in_bpm, unsigned char in_inhale_ratio, unsigned char in_exhale_ratio, bool in_cmv_mode, unsigned long in_cmv_volume_goal, unsigned long in_cmv_pressure_goal) {
  lcd_control(in_bpm, in_volume, in_pressure, in_inhale_ratio, in_exhale_ratio);
}

/**
 * Main loop: track time, update the system, and crucially: call into step_motor as often as possible.
 */
void loop() {
  update_time();
  step_system();
  step_motor();
  input_control(micros());
  debug();
}
