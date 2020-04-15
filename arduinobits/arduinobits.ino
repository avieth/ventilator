#include <Encoder.h>
#include <math.h>
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
  encoder.write(0);
  s_encoder_position = 0;
  initializeSensors();
  // Give the display state the relevant pointers, so it can edit them.
  display_state *displayState = setup_display(c_bpm, c_cmv_volume_goal, c_cmv_pressure_goal, c_ie_inhale, c_ie_exhale);
  // TODO FIXME should not need to set up inputs and recordings explicitly
  // here; should be encapsulated in ui_setup or something.
  inputs_setup(displayState, &c_mode, &c_button_start, &c_button_stop, &c_bpm, &c_cmv_volume_goal, &c_cmv_pressure_goal, &c_ie_inhale, &c_ie_exhale);
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

  unsigned long now_us = micros();
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
        // LCD display makes it underrun all the time.
        // TODO make the LCD library non-blocking plz.
        //Serial.print("motor underrun: missed ");
        //Serial.print(missed);
        //Serial.println(" steps(s)");
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

  /*int16_t irawData = read_sensor_with_offset(INSPFLOW);
  int32_t ir = flow_rate(irawData);
  int16_t erawData = read_sensor_with_offset(EXPFLOW);
  int32_t er = flow_rate(erawData);
  int16_t prawData = read_sensor_with_offset(INSPPRESSURE);
  int32_t pr = pressure_difference_i(prawData);
  Serial.print(irawData);
  Serial.print(" = ");
  Serial.print(ir);
  Serial.print(" : ");
  Serial.print(erawData);
  Serial.print(" = ");
  Serial.println(er);*/
  //Serial.print(" : ");
  //Serial.println(prawData);
  //Serial.print(" : ");
  //Serial.println(pr);
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

void debug(int32_t in_enc, int32_t in_enc_low, int32_t in_enc_high) {
  static unsigned long debug_elapsed_us = 0;
  debug_elapsed_us += t_delta_us;
  if (debug_elapsed_us >= DEBUG_INTERVAL) {
    /*Serial.print(in_enc_low);
    Serial.print(" : ");
    Serial.print(in_enc);
    Serial.print(" : ");
    Serial.println(in_enc_high);*/
    // Put periodic debug code here if you desire.
    //Serial.println("DEBUG");
    //Serial.print(" insp pressure in Pa: ");
    //Serial.println(s_insp_pressure_1);
    debug_elapsed_us = 0;
    //Serial.println(pulse_us);
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
  // Updates the LCD memory but does not write to the device; that's done in a separate loop because it's very
  // very very slow.
  lcd_control(in_state, in_mode, in_bpm, in_volume, ((int32_t) max(0, in_pressure)), in_inhale_ratio, in_exhale_ratio);
}

/**
 * Main loop: track time, update the system, and crucially: call into step_motor as often as possible.
 */
void loop() {
  update_time();
  step_system();
  step_motor();
  lcd_display();
  led_display();
  input_control(micros());
  //debug();
}
