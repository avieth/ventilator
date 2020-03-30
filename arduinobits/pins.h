/**
 * Preprocessor definitions relating to the pin identifiers for motor
 * control, sensors, etc.
 */

/**
 * Sensors (analog pins).
 */
#define PIN_INSP_PRESSURE A0
#define PIN_EXP_PRESSURE A1
#define PIN_INSP_FLOW A2
#define PIN_EXP_FLOW A3
#define PIN_AIR_IN_FLOW A4

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
