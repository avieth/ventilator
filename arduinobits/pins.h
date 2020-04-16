#include <Arduino.h>

/**
 * Preprocessor definitions relating to the pin identifiers for motor
 * control, sensors, etc.
 *
 * Display and input pins are not defined here.
 */

/**
 * Sensors (analog pins).
 */
#define PIN_INSP_PRESSURE A0
#define PIN_INSP_FLOW A2
#define PIN_EXP_FLOW A3
#define PIN_AIR_IN_FLOW A1

/** 
 * Rotary encoder.
 */
#define PIN_ENC_0 20
#define PIN_ENC_1 21

/**
 * Motor control: direction and step.
 */
#define PIN_MOTOR_DIRECTION 6
#define PIN_MOTOR_STEP 7

/**
 * Switches to indicate extermes of the piston.
 * 
 * LOWER means fully retracted (patient has exhaled)
 * UPPER means the opposite: no more air can be pushed.
 *
 * TODO proper pins. dunno what they will be on the due.
 */
#define PIN_LIMIT_SWITCH_LOWER 19
#define PIN_LIMIT_SWITCH_UPPER 20
