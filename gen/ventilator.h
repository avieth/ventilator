/**
 * These values must be set at each step of the program (or just whenever they
 * change).
 *
 * c_ prefix means (operator/user) control
 * s_ prefix means sensor input (or derived from sensor input)
 * t_ is time-related
 */
extern uint32_t t_delta_us;
extern uint8_t c_bpm;
extern uint16_t c_ie_ratio;
extern bool c_cmv_mode;
extern int32_t s_volume;
extern uint32_t c_cmv_volume_goal;
extern uint32_t c_volume_limit;
extern int32_t s_internal_pressure_1;
extern uint32_t c_cmv_pressure_goal;
extern uint32_t c_pressure_limit;
extern int32_t s_flow;
extern bool s_piston_high;
extern bool s_piston_low;
/**
 * These functions must be implemented according to the hardware. They are
 * called by step() according to the high-level spec.
 */
void raise_alarm(void);
void update_ui(int32_t update_ui_arg0, int8_t update_ui_arg1, bool update_ui_arg2, bool update_ui_arg3, uint8_t update_ui_arg4, uint32_t update_ui_arg5, uint32_t update_ui_arg6, uint8_t update_ui_arg7, uint8_t update_ui_arg8, bool update_ui_arg9, uint32_t update_ui_arg10, uint32_t update_ui_arg11, uint32_t update_ui_arg12, uint32_t update_ui_arg13, uint32_t update_ui_arg14, uint32_t update_ui_arg15);
void control_motor(int8_t control_motor_arg0);
/**
 * This is defined in ventilator.c
 */
void step(void);
