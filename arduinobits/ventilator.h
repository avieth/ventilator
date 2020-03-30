extern uint32_t t_delta_us;
extern bool s_limit_low;
extern int32_t s_encoder_position;
extern uint8_t c_bpm;
extern uint16_t c_ie_ratio;
extern uint32_t c_cmv_volume_goal;
extern bool c_cmv_mode;
extern uint32_t c_cmv_pressure_goal;
void raise_alarm(void);
void update_ui(double flow, double volume, int32_t pressure, uint8_t bpm_limited, uint8_t ie_inhale, uint8_t ie_exhale, bool cmv_mode, uint32_t cmv_volume_goal, uint32_t cmv_pressure_goal);
void control_motor(int32_t us_per_pulse);
void calibration_change(uint8_t calibration_change_arg0, double calibration_change_arg1, double calibration_change_arg2);
void zero_encoder(void);
void step(void);

