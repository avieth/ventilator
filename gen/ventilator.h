/**
 * Change in time in microseconds since the last step.
 */
extern uint32_t t_delta_us;
/**
 * c_ prefix indicates user (operator) control. Set these whenever operator
 * settings change.
 */
extern uint8_t c_bpm;
extern uint16_t c_ie_ratio;
extern bool c_cmv_mode;
extern uint32_t c_cmv_volume_goal;
extern uint32_t c_volume_limit;
extern uint32_t c_cmv_pressure_goal;
extern uint32_t c_pressure_limit;
/**
 * s_ prefix indicates a sensor value or derived from a sensor value (flow,
 * volume). Many declared sensors in the spec do not appear here because they
 * aren't yet used.
 *
 * Must set these before each call to step.
 */
extern int32_t s_flow;
extern int32_t s_volume;
extern int32_t s_internal_pressure_1;
extern bool s_piston_high;
extern bool s_piston_low;
/**
 * Routines which must be implemented by the C driver.
 */
void raise_alarm(void);
void update_ui(int32_t desired_flow, int8_t motor_velocity, bool s_piston_high, bool s_piston_low, uint8_t bpm_limited, uint32_t volume_limit, uint32_t pressure_limit, uint8_t ie_inhale, uint8_t ie_exhale, bool cmv_mode, uint32_t cmv_volume_goal, uint32_t cmv_pressure_goal, uint32_t global_volume_max, uint32_t global_volume_min, uint32_t global_pressure_max, uint32_t global_pressure_min);
void control_motor(int8_t motor_velocity);
/**
 * Call this at every step of the program (after updating time delta, sensor,
 * and control globals). It will call into the above routines whenever
 * necessary.
 */
void step(void);
