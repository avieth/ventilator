/**
 * Preprocessor definitions relating to the pin identifiers for motor
 * control, sensors, etc.
 */

/**
 * Sensors (analog pins).
 */
#define PIN_INSP_PRESSURE 8
#define PIN_EXP_PRESSURE 9
#define PIN_INSP_FLOW 10
#define PIN_EXP_FLOW 11
#define PIN_AIR_IN_FLOW 12

/** 
 * Rotary encoder.
 * These are chosen because they are interrupt pins.
 */
#define PIN_ENC_0 2
#define PIN_ENC_1 3

/**
 * Motor control: direction and step.
 */
#define PIN_MOTOR_DIRECTION 6
#define PIN_MOTOR_STEP 7

/**
 * A switch to indicate 0 for the encoder.
 */
#define PIN_MOTOR_LIMIT_SWITCH 13
