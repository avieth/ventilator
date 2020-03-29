extern uint8_t c_bpm;
extern uint16_t c_ie_ratio;
extern uint32_t t_delta_us;
extern bool s_limit_low;
extern uint32_t c_cmv_volume_goal;
extern int32_t s_encoder_position;
void raise_alarm(void);
void control_motor(uint32_t us_per_pulse, bool motor_direction);
void zero_encoder(void);
void step(void);

