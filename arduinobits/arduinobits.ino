#include <Encoder.h>
#include "ventilator.c"

/** 
 * Pins for the position encoder.
 * These are chosen because they are interrupt pins.
 */
#define ENC_PIN0 2
#define ENC_PIN1 3

Encoder encoder(ENC_PIN0, ENC_PIN1);

int32_t s_encoder_position = 0;

/**
 * Motor control: direction and step.
 */
#define DIRECTION_PIN 6
#define STEP_PIN 7

/**
 * A switch to indicate 0 for the encoder.
 */
#define LIMITSWITCH_PIN 12

bool s_limit_low = false;

/**
 * Operator controls (c_ prefix).
 * These are read by the ventilator system logic.
 */
uint8_t c_bpm = 12;
uint16_t c_ie_ratio = 0x0102;
bool c_cmv_mode = true;
uint32_t c_volume_limit = 1000;
uint32_t c_pressure_limit = 5000;
uint32_t c_cmv_volume_goal = 1000; //mL
uint32_t c_cmv_pressure_goal = 3000;
uint32_t c_peep = 200;

int32_t s_internal_pressure_1 = 0;

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
  pinMode(STEP_PIN, OUTPUT);
  pinMode(DIRECTION_PIN, OUTPUT);  
  digitalWrite(DIRECTION_PIN, HIGH);
  pinMode(LIMITSWITCH_PIN, INPUT_PULLUP);
  Serial.begin(9600);
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
unsigned long pulse_us = 1250;

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
  if ((now_us - time_at_last_step_motor_us) > pulse_us) {
    //Serial.println("xrun");
    now_us = micros();
  }
  time_at_last_step_motor_us = now_us;
  
  if (motor_direction != current_direction) {
    current_direction = motor_direction;
    if (current_direction) {
      digitalWrite(DIRECTION_PIN, HIGH);
    } else {
      digitalWrite(DIRECTION_PIN, LOW);
    }
  }
  time_since_last_pulse_us += t_delta_us;
  // 0 means "do not move"; it does not mean "move infinitely fast".
  if (pulse_us == 0) {
    return;
  } else if (time_since_last_pulse_us >= pulse_us) {
    digitalWrite(STEP_PIN, LOW);
    digitalWrite(STEP_PIN, HIGH);
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
  s_limit_low = digitalRead(LIMITSWITCH_PIN) == LOW;
  //if (s_limit_low) { Serial.println("low"); }
}

/**
 * To be triggered whenever the motor low switch is on (is LOW).
 */
void zero_encoder() {
  encoder.write(0);
  s_encoder_position = 0;
}

void calibration_change(unsigned char in_c_phase, double in_theta, double in_volume) {
  /*Serial.print(in_c_phase);
  Serial.print(" : ");
  Serial.print(encoder.read());
  Serial.print(" : ");
  Serial.print(in_theta);
  Serial.print(" : ");
  Serial.println(in_volume);*/
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
    //Serial.println(s_encoder_position);
    debug_elapsed_us = 0;
  }
}

void update_ui(double in_flow, double in_volume, long in_pressure, unsigned char in_bpm, unsigned char in_inhale_ratio, unsigned char in_exhale_ratio, bool in_cmv_mode, unsigned long in_cmv_volume_goal, unsigned long in_cmv_pressure_goal) {
  //Serial.println(in_flow);
}

/**
 * Main loop: track time, update the system, and crucially: call into step_motor as often as possible.
 */
void loop() {
  update_time();
  step_system();
  step_motor();
  debug();
}
