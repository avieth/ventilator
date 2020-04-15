#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>

#include "ventilator.h"

static uint32_t t_delta_us_cpy;
static bool s_limit_low_cpy;
static bool s_limit_high_cpy;
static bool c_button_start_cpy;
static bool c_button_stop_cpy;
static int32_t s_encoder_position_cpy;
static uint8_t c_bpm_cpy;
static uint8_t c_ie_exhale_cpy;
static uint8_t c_ie_inhale_cpy;
static int32_t s_insp_flow_1_cpy;
static uint8_t c_mode_cpy;
static bool c_cmv_mode_cpy;
static uint32_t c_cmv_volume_goal_cpy;
static uint32_t c_cmv_pressure_goal_cpy;
static unsigned int s1_size = (2);
static uint32_t s1[(2)] = {((uint32_t)(0)), (0)};
static unsigned int s0_size = (2);
static uint32_t s0[(2)] = {((uint32_t)(0)), (0)};
static unsigned int s4_size = (3);
static bool s4[(3)] = {(false), (false), (0)};
static unsigned int s5_size = (3);
static bool s5[(3)] = {(false), (false), (0)};
static unsigned int s3_size = (2);
static uint8_t s3[(2)] = {((uint8_t)(0)), (0)};
static unsigned int s6_size = (2);
static bool s6[(2)] = {(false), (0)};
static unsigned int s7_size = (2);
static bool s7[(2)] = {(false), (0)};
static unsigned int s2_size = (2);
static uint8_t s2[(2)] = {((uint8_t)(0)), (0)};
static unsigned int s8_size = (2);
static int32_t s8[(2)] = {((int32_t)(0)), (0)};
static unsigned int s11_size = (2);
static bool s11[(2)] = {(false), (0)};
static unsigned int s10_size = (2);
static int32_t s10[(2)] = {((int32_t)(0)), (0)};
static unsigned int s13_size = (2);
static int32_t s13[(2)] = {((int32_t)(0)), (0)};
static unsigned int s22_size = (2);
static bool s22[(2)] = {(false), (0)};
static unsigned int s24_size = (2);
static bool s24[(2)] = {(false), (0)};
static unsigned int s23_size = (2);
static int32_t s23[(2)] = {((int32_t)(0)), (0)};
static unsigned int s26_size = (2);
static bool s26[(2)] = {(false), (0)};
static unsigned int s25_size = (2);
static int32_t s25[(2)] = {((int32_t)(0)), (0)};
static unsigned int s21_size = (2);
static uint8_t s21[(2)] = {((uint8_t)(0)), (0)};
static unsigned int s29_size = (2);
static bool s29[(2)] = {(false), (0)};
static unsigned int s28_size = (2);
static int32_t s28[(2)] = {((int32_t)(0)), (0)};
static size_t s1_idx = (0);
static size_t s0_idx = (0);
static size_t s4_idx = (0);
static size_t s5_idx = (0);
static size_t s3_idx = (0);
static size_t s6_idx = (0);
static size_t s7_idx = (0);
static size_t s2_idx = (0);
static size_t s8_idx = (0);
static size_t s11_idx = (0);
static size_t s10_idx = (0);
static size_t s13_idx = (0);
static size_t s22_idx = (0);
static size_t s24_idx = (0);
static size_t s23_idx = (0);
static size_t s26_idx = (0);
static size_t s25_idx = (0);
static size_t s21_idx = (0);
static size_t s29_idx = (0);
static size_t s28_idx = (0);

uint32_t s1_gen(void) {
  return t_delta_us_cpy;
}

uint32_t s0_gen(void) {
  return (((s0)[s0_idx]) >= ((uint32_t)(200000))) ? (uint32_t)(0) : (((s0)[s0_idx]) + ((s1)[s1_idx]));
}

bool s4_gen(void) {
  return s_limit_low_cpy;
}

bool s5_gen(void) {
  return s_limit_high_cpy;
}

uint8_t s3_gen(void) {
  return ((((s3)[s3_idx]) != ((uint8_t)(0))) && (((s2)[s2_idx]) == ((uint8_t)(4)))) ? (uint8_t)(0) : (((((s3)[s3_idx]) == ((uint8_t)(0))) && (((s4)[s4_idx]) && ((s4)[((s4_idx) + (1)) % (s4_size)]))) ? (uint8_t)(1) : (((((s3)[s3_idx]) == ((uint8_t)(1))) && (((s5)[s5_idx]) && ((s5)[((s5_idx) + (1)) % (s5_size)]))) ? (uint8_t)(2) : (((((s3)[s3_idx]) == ((uint8_t)(2))) && (((s4)[s4_idx]) && ((s4)[((s4_idx) + (1)) % (s4_size)]))) ? (uint8_t)(3) : ((s3)[s3_idx]))));
}

bool s6_gen(void) {
  return c_button_start_cpy;
}

bool s7_gen(void) {
  return c_button_stop_cpy;
}

uint8_t s2_gen(void) {
  return (((s2)[s2_idx]) == ((uint8_t)(0))) ? (((s3)[s3_idx]) == ((uint8_t)(3))) ? (uint8_t)(1) : ((uint8_t)(0)) : ((((s2)[s2_idx]) == ((uint8_t)(1))) ? ((c_button_start_cpy) && (!((s6)[s6_idx]))) ? (uint8_t)(2) : ((uint8_t)(1)) : ((((s2)[s2_idx]) == ((uint8_t)(2))) ? ((c_button_stop_cpy) && (!((s7)[s7_idx]))) ? (uint8_t)(3) : ((uint8_t)(2)) : ((((s2)[s2_idx]) == ((uint8_t)(3))) ? ((c_button_start_cpy) && (!((s6)[s6_idx]))) ? (uint8_t)(4) : (((c_button_stop_cpy) && (!((s7)[s7_idx]))) ? (uint8_t)(0) : ((uint8_t)(3))) : ((((s2)[s2_idx]) == ((uint8_t)(4))) ? (((s4)[s4_idx]) && ((s4)[((s4_idx) + (1)) % (s4_size)])) ? (uint8_t)(1) : ((uint8_t)(4)) : ((s2)[s2_idx])))));
}

int32_t s8_gen(void) {
  return (((s4)[s4_idx]) && ((s4)[((s4_idx) + (1)) % (s4_size)])) ? s_encoder_position_cpy : ((s8)[s8_idx]);
}

bool s11_gen(void) {
  return ((s2)[s2_idx]) == ((uint8_t)(2));
}

int32_t s10_gen(void) {
  return (!(((s2)[s2_idx]) == ((uint8_t)(2)))) ? (s10)[s10_idx] : (((((s2)[s2_idx]) == ((uint8_t)(2))) && (!((s11)[s11_idx]))) ? (int32_t)(((((((uint32_t)(60000000)) / ((uint32_t)(((c_bpm_cpy) <= ((uint8_t)(6))) ? (uint8_t)(6) : (((c_bpm_cpy) >= ((uint8_t)(40))) ? (uint8_t)(40) : (c_bpm_cpy))))) / ((uint32_t)((((((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) <= (((uint8_t)(4)) * (c_ie_inhale_cpy))) ? c_ie_inhale_cpy : ((((((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) % ((uint8_t)(4))) == ((uint8_t)(0))) ? (((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) / ((uint8_t)(4)) : (((((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) / ((uint8_t)(4))) + ((uint8_t)(1))))) + (((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy))))) == ((uint32_t)(0))) ? (uint32_t)(1) : ((((uint32_t)(60000000)) / ((uint32_t)(((c_bpm_cpy) <= ((uint8_t)(6))) ? (uint8_t)(6) : (((c_bpm_cpy) >= ((uint8_t)(40))) ? (uint8_t)(40) : (c_bpm_cpy))))) / ((uint32_t)((((((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) <= (((uint8_t)(4)) * (c_ie_inhale_cpy))) ? c_ie_inhale_cpy : ((((((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) % ((uint8_t)(4))) == ((uint8_t)(0))) ? (((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) / ((uint8_t)(4)) : (((((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) / ((uint8_t)(4))) + ((uint8_t)(1))))) + (((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)))))) * ((uint32_t)(((((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) <= (((uint8_t)(4)) * (c_ie_inhale_cpy))) ? c_ie_inhale_cpy : ((((((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) % ((uint8_t)(4))) == ((uint8_t)(0))) ? (((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) / ((uint8_t)(4)) : (((((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) / ((uint8_t)(4))) + ((uint8_t)(1))))))) : ((((s10)[s10_idx]) > ((int32_t)(0))) ? (((s10)[s10_idx]) <= ((int32_t)((s1)[s1_idx]))) ? ((int32_t)(0)) - ((int32_t)((((uint32_t)(60000000)) / ((uint32_t)(((c_bpm_cpy) <= ((uint8_t)(6))) ? (uint8_t)(6) : (((c_bpm_cpy) >= ((uint8_t)(40))) ? (uint8_t)(40) : (c_bpm_cpy))))) - (((((((uint32_t)(60000000)) / ((uint32_t)(((c_bpm_cpy) <= ((uint8_t)(6))) ? (uint8_t)(6) : (((c_bpm_cpy) >= ((uint8_t)(40))) ? (uint8_t)(40) : (c_bpm_cpy))))) / ((uint32_t)((((((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) <= (((uint8_t)(4)) * (c_ie_inhale_cpy))) ? c_ie_inhale_cpy : ((((((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) % ((uint8_t)(4))) == ((uint8_t)(0))) ? (((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) / ((uint8_t)(4)) : (((((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) / ((uint8_t)(4))) + ((uint8_t)(1))))) + (((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy))))) == ((uint32_t)(0))) ? (uint32_t)(1) : ((((uint32_t)(60000000)) / ((uint32_t)(((c_bpm_cpy) <= ((uint8_t)(6))) ? (uint8_t)(6) : (((c_bpm_cpy) >= ((uint8_t)(40))) ? (uint8_t)(40) : (c_bpm_cpy))))) / ((uint32_t)((((((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) <= (((uint8_t)(4)) * (c_ie_inhale_cpy))) ? c_ie_inhale_cpy : ((((((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) % ((uint8_t)(4))) == ((uint8_t)(0))) ? (((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) / ((uint8_t)(4)) : (((((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) / ((uint8_t)(4))) + ((uint8_t)(1))))) + (((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)))))) * ((uint32_t)(((((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) <= (((uint8_t)(4)) * (c_ie_inhale_cpy))) ? c_ie_inhale_cpy : ((((((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) % ((uint8_t)(4))) == ((uint8_t)(0))) ? (((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) / ((uint8_t)(4)) : (((((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) / ((uint8_t)(4))) + ((uint8_t)(1))))))))) : (((s10)[s10_idx]) - ((int32_t)((s1)[s1_idx]))) : ((((s10)[s10_idx]) < ((int32_t)(0))) ? (((s10)[s10_idx]) >= (((int32_t)(0)) - ((int32_t)((s1)[s1_idx])))) ? (int32_t)(((((((uint32_t)(60000000)) / ((uint32_t)(((c_bpm_cpy) <= ((uint8_t)(6))) ? (uint8_t)(6) : (((c_bpm_cpy) >= ((uint8_t)(40))) ? (uint8_t)(40) : (c_bpm_cpy))))) / ((uint32_t)((((((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) <= (((uint8_t)(4)) * (c_ie_inhale_cpy))) ? c_ie_inhale_cpy : ((((((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) % ((uint8_t)(4))) == ((uint8_t)(0))) ? (((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) / ((uint8_t)(4)) : (((((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) / ((uint8_t)(4))) + ((uint8_t)(1))))) + (((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy))))) == ((uint32_t)(0))) ? (uint32_t)(1) : ((((uint32_t)(60000000)) / ((uint32_t)(((c_bpm_cpy) <= ((uint8_t)(6))) ? (uint8_t)(6) : (((c_bpm_cpy) >= ((uint8_t)(40))) ? (uint8_t)(40) : (c_bpm_cpy))))) / ((uint32_t)((((((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) <= (((uint8_t)(4)) * (c_ie_inhale_cpy))) ? c_ie_inhale_cpy : ((((((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) % ((uint8_t)(4))) == ((uint8_t)(0))) ? (((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) / ((uint8_t)(4)) : (((((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) / ((uint8_t)(4))) + ((uint8_t)(1))))) + (((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)))))) * ((uint32_t)(((((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) <= (((uint8_t)(4)) * (c_ie_inhale_cpy))) ? c_ie_inhale_cpy : ((((((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) % ((uint8_t)(4))) == ((uint8_t)(0))) ? (((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) / ((uint8_t)(4)) : (((((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) / ((uint8_t)(4))) + ((uint8_t)(1))))))) : (((s10)[s10_idx]) + ((int32_t)((s1)[s1_idx]))) : ((int32_t)(0)))));
}

int32_t s13_gen(void) {
  return (((s5)[s5_idx]) && ((s5)[((s5_idx) + (1)) % (s5_size)])) ? s_encoder_position_cpy : ((s13)[s13_idx]);
}

bool s22_gen(void) {
  return ((s2)[s2_idx]) == ((uint8_t)(2));
}

bool s24_gen(void) {
  return ((s2)[s2_idx]) == ((uint8_t)(2));
}

int32_t s23_gen(void) {
  return (!(((s2)[s2_idx]) == ((uint8_t)(2)))) ? (s23)[s23_idx] : (((((s2)[s2_idx]) == ((uint8_t)(2))) && (!((s24)[s24_idx]))) ? (int32_t)(((((((uint32_t)(60000000)) / ((uint32_t)(((c_bpm_cpy) <= ((uint8_t)(6))) ? (uint8_t)(6) : (((c_bpm_cpy) >= ((uint8_t)(40))) ? (uint8_t)(40) : (c_bpm_cpy))))) / ((uint32_t)((((((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) <= (((uint8_t)(4)) * (c_ie_inhale_cpy))) ? c_ie_inhale_cpy : ((((((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) % ((uint8_t)(4))) == ((uint8_t)(0))) ? (((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) / ((uint8_t)(4)) : (((((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) / ((uint8_t)(4))) + ((uint8_t)(1))))) + (((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy))))) == ((uint32_t)(0))) ? (uint32_t)(1) : ((((uint32_t)(60000000)) / ((uint32_t)(((c_bpm_cpy) <= ((uint8_t)(6))) ? (uint8_t)(6) : (((c_bpm_cpy) >= ((uint8_t)(40))) ? (uint8_t)(40) : (c_bpm_cpy))))) / ((uint32_t)((((((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) <= (((uint8_t)(4)) * (c_ie_inhale_cpy))) ? c_ie_inhale_cpy : ((((((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) % ((uint8_t)(4))) == ((uint8_t)(0))) ? (((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) / ((uint8_t)(4)) : (((((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) / ((uint8_t)(4))) + ((uint8_t)(1))))) + (((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)))))) * ((uint32_t)(((((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) <= (((uint8_t)(4)) * (c_ie_inhale_cpy))) ? c_ie_inhale_cpy : ((((((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) % ((uint8_t)(4))) == ((uint8_t)(0))) ? (((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) / ((uint8_t)(4)) : (((((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) / ((uint8_t)(4))) + ((uint8_t)(1))))))) : ((((s23)[s23_idx]) > ((int32_t)(0))) ? (((s23)[s23_idx]) <= ((int32_t)((s1)[s1_idx]))) ? ((int32_t)(0)) - ((int32_t)((((uint32_t)(60000000)) / ((uint32_t)(((c_bpm_cpy) <= ((uint8_t)(6))) ? (uint8_t)(6) : (((c_bpm_cpy) >= ((uint8_t)(40))) ? (uint8_t)(40) : (c_bpm_cpy))))) - (((((((uint32_t)(60000000)) / ((uint32_t)(((c_bpm_cpy) <= ((uint8_t)(6))) ? (uint8_t)(6) : (((c_bpm_cpy) >= ((uint8_t)(40))) ? (uint8_t)(40) : (c_bpm_cpy))))) / ((uint32_t)((((((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) <= (((uint8_t)(4)) * (c_ie_inhale_cpy))) ? c_ie_inhale_cpy : ((((((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) % ((uint8_t)(4))) == ((uint8_t)(0))) ? (((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) / ((uint8_t)(4)) : (((((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) / ((uint8_t)(4))) + ((uint8_t)(1))))) + (((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy))))) == ((uint32_t)(0))) ? (uint32_t)(1) : ((((uint32_t)(60000000)) / ((uint32_t)(((c_bpm_cpy) <= ((uint8_t)(6))) ? (uint8_t)(6) : (((c_bpm_cpy) >= ((uint8_t)(40))) ? (uint8_t)(40) : (c_bpm_cpy))))) / ((uint32_t)((((((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) <= (((uint8_t)(4)) * (c_ie_inhale_cpy))) ? c_ie_inhale_cpy : ((((((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) % ((uint8_t)(4))) == ((uint8_t)(0))) ? (((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) / ((uint8_t)(4)) : (((((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) / ((uint8_t)(4))) + ((uint8_t)(1))))) + (((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)))))) * ((uint32_t)(((((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) <= (((uint8_t)(4)) * (c_ie_inhale_cpy))) ? c_ie_inhale_cpy : ((((((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) % ((uint8_t)(4))) == ((uint8_t)(0))) ? (((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) / ((uint8_t)(4)) : (((((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) / ((uint8_t)(4))) + ((uint8_t)(1))))))))) : (((s23)[s23_idx]) - ((int32_t)((s1)[s1_idx]))) : ((((s23)[s23_idx]) < ((int32_t)(0))) ? (((s23)[s23_idx]) >= (((int32_t)(0)) - ((int32_t)((s1)[s1_idx])))) ? (int32_t)(((((((uint32_t)(60000000)) / ((uint32_t)(((c_bpm_cpy) <= ((uint8_t)(6))) ? (uint8_t)(6) : (((c_bpm_cpy) >= ((uint8_t)(40))) ? (uint8_t)(40) : (c_bpm_cpy))))) / ((uint32_t)((((((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) <= (((uint8_t)(4)) * (c_ie_inhale_cpy))) ? c_ie_inhale_cpy : ((((((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) % ((uint8_t)(4))) == ((uint8_t)(0))) ? (((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) / ((uint8_t)(4)) : (((((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) / ((uint8_t)(4))) + ((uint8_t)(1))))) + (((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy))))) == ((uint32_t)(0))) ? (uint32_t)(1) : ((((uint32_t)(60000000)) / ((uint32_t)(((c_bpm_cpy) <= ((uint8_t)(6))) ? (uint8_t)(6) : (((c_bpm_cpy) >= ((uint8_t)(40))) ? (uint8_t)(40) : (c_bpm_cpy))))) / ((uint32_t)((((((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) <= (((uint8_t)(4)) * (c_ie_inhale_cpy))) ? c_ie_inhale_cpy : ((((((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) % ((uint8_t)(4))) == ((uint8_t)(0))) ? (((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) / ((uint8_t)(4)) : (((((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) / ((uint8_t)(4))) + ((uint8_t)(1))))) + (((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)))))) * ((uint32_t)(((((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) <= (((uint8_t)(4)) * (c_ie_inhale_cpy))) ? c_ie_inhale_cpy : ((((((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) % ((uint8_t)(4))) == ((uint8_t)(0))) ? (((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) / ((uint8_t)(4)) : (((((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) / ((uint8_t)(4))) + ((uint8_t)(1))))))) : (((s23)[s23_idx]) + ((int32_t)((s1)[s1_idx]))) : ((int32_t)(0)))));
}

bool s26_gen(void) {
  return ((s2)[s2_idx]) == ((uint8_t)(2));
}

int32_t s25_gen(void) {
  return (!(((s2)[s2_idx]) == ((uint8_t)(2)))) ? (s25)[s25_idx] : (((((s2)[s2_idx]) == ((uint8_t)(2))) && (!((s26)[s26_idx]))) ? (int32_t)(((((((uint32_t)(60000000)) / ((uint32_t)(((c_bpm_cpy) <= ((uint8_t)(6))) ? (uint8_t)(6) : (((c_bpm_cpy) >= ((uint8_t)(40))) ? (uint8_t)(40) : (c_bpm_cpy))))) / ((uint32_t)((((((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) <= (((uint8_t)(4)) * (c_ie_inhale_cpy))) ? c_ie_inhale_cpy : ((((((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) % ((uint8_t)(4))) == ((uint8_t)(0))) ? (((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) / ((uint8_t)(4)) : (((((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) / ((uint8_t)(4))) + ((uint8_t)(1))))) + (((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy))))) == ((uint32_t)(0))) ? (uint32_t)(1) : ((((uint32_t)(60000000)) / ((uint32_t)(((c_bpm_cpy) <= ((uint8_t)(6))) ? (uint8_t)(6) : (((c_bpm_cpy) >= ((uint8_t)(40))) ? (uint8_t)(40) : (c_bpm_cpy))))) / ((uint32_t)((((((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) <= (((uint8_t)(4)) * (c_ie_inhale_cpy))) ? c_ie_inhale_cpy : ((((((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) % ((uint8_t)(4))) == ((uint8_t)(0))) ? (((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) / ((uint8_t)(4)) : (((((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) / ((uint8_t)(4))) + ((uint8_t)(1))))) + (((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)))))) * ((uint32_t)(((((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) <= (((uint8_t)(4)) * (c_ie_inhale_cpy))) ? c_ie_inhale_cpy : ((((((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) % ((uint8_t)(4))) == ((uint8_t)(0))) ? (((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) / ((uint8_t)(4)) : (((((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) / ((uint8_t)(4))) + ((uint8_t)(1))))))) : ((((s25)[s25_idx]) > ((int32_t)(0))) ? (((s25)[s25_idx]) <= ((int32_t)((s1)[s1_idx]))) ? ((int32_t)(0)) - ((int32_t)((((uint32_t)(60000000)) / ((uint32_t)(((c_bpm_cpy) <= ((uint8_t)(6))) ? (uint8_t)(6) : (((c_bpm_cpy) >= ((uint8_t)(40))) ? (uint8_t)(40) : (c_bpm_cpy))))) - (((((((uint32_t)(60000000)) / ((uint32_t)(((c_bpm_cpy) <= ((uint8_t)(6))) ? (uint8_t)(6) : (((c_bpm_cpy) >= ((uint8_t)(40))) ? (uint8_t)(40) : (c_bpm_cpy))))) / ((uint32_t)((((((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) <= (((uint8_t)(4)) * (c_ie_inhale_cpy))) ? c_ie_inhale_cpy : ((((((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) % ((uint8_t)(4))) == ((uint8_t)(0))) ? (((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) / ((uint8_t)(4)) : (((((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) / ((uint8_t)(4))) + ((uint8_t)(1))))) + (((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy))))) == ((uint32_t)(0))) ? (uint32_t)(1) : ((((uint32_t)(60000000)) / ((uint32_t)(((c_bpm_cpy) <= ((uint8_t)(6))) ? (uint8_t)(6) : (((c_bpm_cpy) >= ((uint8_t)(40))) ? (uint8_t)(40) : (c_bpm_cpy))))) / ((uint32_t)((((((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) <= (((uint8_t)(4)) * (c_ie_inhale_cpy))) ? c_ie_inhale_cpy : ((((((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) % ((uint8_t)(4))) == ((uint8_t)(0))) ? (((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) / ((uint8_t)(4)) : (((((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) / ((uint8_t)(4))) + ((uint8_t)(1))))) + (((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)))))) * ((uint32_t)(((((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) <= (((uint8_t)(4)) * (c_ie_inhale_cpy))) ? c_ie_inhale_cpy : ((((((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) % ((uint8_t)(4))) == ((uint8_t)(0))) ? (((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) / ((uint8_t)(4)) : (((((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) / ((uint8_t)(4))) + ((uint8_t)(1))))))))) : (((s25)[s25_idx]) - ((int32_t)((s1)[s1_idx]))) : ((((s25)[s25_idx]) < ((int32_t)(0))) ? (((s25)[s25_idx]) >= (((int32_t)(0)) - ((int32_t)((s1)[s1_idx])))) ? (int32_t)(((((((uint32_t)(60000000)) / ((uint32_t)(((c_bpm_cpy) <= ((uint8_t)(6))) ? (uint8_t)(6) : (((c_bpm_cpy) >= ((uint8_t)(40))) ? (uint8_t)(40) : (c_bpm_cpy))))) / ((uint32_t)((((((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) <= (((uint8_t)(4)) * (c_ie_inhale_cpy))) ? c_ie_inhale_cpy : ((((((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) % ((uint8_t)(4))) == ((uint8_t)(0))) ? (((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) / ((uint8_t)(4)) : (((((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) / ((uint8_t)(4))) + ((uint8_t)(1))))) + (((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy))))) == ((uint32_t)(0))) ? (uint32_t)(1) : ((((uint32_t)(60000000)) / ((uint32_t)(((c_bpm_cpy) <= ((uint8_t)(6))) ? (uint8_t)(6) : (((c_bpm_cpy) >= ((uint8_t)(40))) ? (uint8_t)(40) : (c_bpm_cpy))))) / ((uint32_t)((((((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) <= (((uint8_t)(4)) * (c_ie_inhale_cpy))) ? c_ie_inhale_cpy : ((((((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) % ((uint8_t)(4))) == ((uint8_t)(0))) ? (((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) / ((uint8_t)(4)) : (((((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) / ((uint8_t)(4))) + ((uint8_t)(1))))) + (((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)))))) * ((uint32_t)(((((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) <= (((uint8_t)(4)) * (c_ie_inhale_cpy))) ? c_ie_inhale_cpy : ((((((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) % ((uint8_t)(4))) == ((uint8_t)(0))) ? (((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) / ((uint8_t)(4)) : (((((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) / ((uint8_t)(4))) + ((uint8_t)(1))))))) : (((s25)[s25_idx]) + ((int32_t)((s1)[s1_idx]))) : ((int32_t)(0)))));
}

uint8_t s21_gen(void) {
  return ((((s2)[s2_idx]) == ((uint8_t)(2))) && (!((s22)[s22_idx]))) ? (uint8_t)(0) : ((((s21)[s21_idx]) == ((uint8_t)(0))) ? (((s23)[s23_idx]) <= ((int32_t)(0))) ? (uint8_t)(1) : ((uint8_t)(0)) : ((((s21)[s21_idx]) == ((uint8_t)(1))) ? ((s_insp_flow_1_cpy) > ((int32_t)(10))) ? (uint8_t)(2) : ((((s25)[s25_idx]) >= ((int32_t)(0))) ? (uint8_t)(0) : ((uint8_t)(1))) : ((((s21)[s21_idx]) == ((uint8_t)(2))) ? ((s_insp_flow_1_cpy) < ((int32_t)(10))) ? (uint8_t)(1) : ((uint8_t)(2)) : ((s21)[s21_idx]))));
}

bool s29_gen(void) {
  return ((s2)[s2_idx]) == ((uint8_t)(2));
}

int32_t s28_gen(void) {
  return (!(((s2)[s2_idx]) == ((uint8_t)(2)))) ? (s28)[s28_idx] : (((((s2)[s2_idx]) == ((uint8_t)(2))) && (!((s29)[s29_idx]))) ? (int32_t)(((((((uint32_t)(60000000)) / ((uint32_t)(((c_bpm_cpy) <= ((uint8_t)(6))) ? (uint8_t)(6) : (((c_bpm_cpy) >= ((uint8_t)(40))) ? (uint8_t)(40) : (c_bpm_cpy))))) / ((uint32_t)((((((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) <= (((uint8_t)(4)) * (c_ie_inhale_cpy))) ? c_ie_inhale_cpy : ((((((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) % ((uint8_t)(4))) == ((uint8_t)(0))) ? (((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) / ((uint8_t)(4)) : (((((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) / ((uint8_t)(4))) + ((uint8_t)(1))))) + (((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy))))) == ((uint32_t)(0))) ? (uint32_t)(1) : ((((uint32_t)(60000000)) / ((uint32_t)(((c_bpm_cpy) <= ((uint8_t)(6))) ? (uint8_t)(6) : (((c_bpm_cpy) >= ((uint8_t)(40))) ? (uint8_t)(40) : (c_bpm_cpy))))) / ((uint32_t)((((((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) <= (((uint8_t)(4)) * (c_ie_inhale_cpy))) ? c_ie_inhale_cpy : ((((((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) % ((uint8_t)(4))) == ((uint8_t)(0))) ? (((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) / ((uint8_t)(4)) : (((((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) / ((uint8_t)(4))) + ((uint8_t)(1))))) + (((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)))))) * ((uint32_t)(((((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) <= (((uint8_t)(4)) * (c_ie_inhale_cpy))) ? c_ie_inhale_cpy : ((((((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) % ((uint8_t)(4))) == ((uint8_t)(0))) ? (((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) / ((uint8_t)(4)) : (((((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) / ((uint8_t)(4))) + ((uint8_t)(1))))))) : ((((s28)[s28_idx]) > ((int32_t)(0))) ? (((s28)[s28_idx]) <= ((int32_t)((s1)[s1_idx]))) ? ((int32_t)(0)) - ((int32_t)((((uint32_t)(60000000)) / ((uint32_t)(((c_bpm_cpy) <= ((uint8_t)(6))) ? (uint8_t)(6) : (((c_bpm_cpy) >= ((uint8_t)(40))) ? (uint8_t)(40) : (c_bpm_cpy))))) - (((((((uint32_t)(60000000)) / ((uint32_t)(((c_bpm_cpy) <= ((uint8_t)(6))) ? (uint8_t)(6) : (((c_bpm_cpy) >= ((uint8_t)(40))) ? (uint8_t)(40) : (c_bpm_cpy))))) / ((uint32_t)((((((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) <= (((uint8_t)(4)) * (c_ie_inhale_cpy))) ? c_ie_inhale_cpy : ((((((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) % ((uint8_t)(4))) == ((uint8_t)(0))) ? (((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) / ((uint8_t)(4)) : (((((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) / ((uint8_t)(4))) + ((uint8_t)(1))))) + (((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy))))) == ((uint32_t)(0))) ? (uint32_t)(1) : ((((uint32_t)(60000000)) / ((uint32_t)(((c_bpm_cpy) <= ((uint8_t)(6))) ? (uint8_t)(6) : (((c_bpm_cpy) >= ((uint8_t)(40))) ? (uint8_t)(40) : (c_bpm_cpy))))) / ((uint32_t)((((((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) <= (((uint8_t)(4)) * (c_ie_inhale_cpy))) ? c_ie_inhale_cpy : ((((((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) % ((uint8_t)(4))) == ((uint8_t)(0))) ? (((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) / ((uint8_t)(4)) : (((((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) / ((uint8_t)(4))) + ((uint8_t)(1))))) + (((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)))))) * ((uint32_t)(((((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) <= (((uint8_t)(4)) * (c_ie_inhale_cpy))) ? c_ie_inhale_cpy : ((((((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) % ((uint8_t)(4))) == ((uint8_t)(0))) ? (((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) / ((uint8_t)(4)) : (((((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) / ((uint8_t)(4))) + ((uint8_t)(1))))))))) : (((s28)[s28_idx]) - ((int32_t)((s1)[s1_idx]))) : ((((s28)[s28_idx]) < ((int32_t)(0))) ? (((s28)[s28_idx]) >= (((int32_t)(0)) - ((int32_t)((s1)[s1_idx])))) ? (int32_t)(((((((uint32_t)(60000000)) / ((uint32_t)(((c_bpm_cpy) <= ((uint8_t)(6))) ? (uint8_t)(6) : (((c_bpm_cpy) >= ((uint8_t)(40))) ? (uint8_t)(40) : (c_bpm_cpy))))) / ((uint32_t)((((((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) <= (((uint8_t)(4)) * (c_ie_inhale_cpy))) ? c_ie_inhale_cpy : ((((((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) % ((uint8_t)(4))) == ((uint8_t)(0))) ? (((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) / ((uint8_t)(4)) : (((((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) / ((uint8_t)(4))) + ((uint8_t)(1))))) + (((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy))))) == ((uint32_t)(0))) ? (uint32_t)(1) : ((((uint32_t)(60000000)) / ((uint32_t)(((c_bpm_cpy) <= ((uint8_t)(6))) ? (uint8_t)(6) : (((c_bpm_cpy) >= ((uint8_t)(40))) ? (uint8_t)(40) : (c_bpm_cpy))))) / ((uint32_t)((((((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) <= (((uint8_t)(4)) * (c_ie_inhale_cpy))) ? c_ie_inhale_cpy : ((((((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) % ((uint8_t)(4))) == ((uint8_t)(0))) ? (((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) / ((uint8_t)(4)) : (((((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) / ((uint8_t)(4))) + ((uint8_t)(1))))) + (((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)))))) * ((uint32_t)(((((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) <= (((uint8_t)(4)) * (c_ie_inhale_cpy))) ? c_ie_inhale_cpy : ((((((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) % ((uint8_t)(4))) == ((uint8_t)(0))) ? (((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) / ((uint8_t)(4)) : (((((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) / ((uint8_t)(4))) + ((uint8_t)(1))))))) : (((s28)[s28_idx]) + ((int32_t)((s1)[s1_idx]))) : ((int32_t)(0)))));
}

bool raise_alarm_guard(void) {
  return false;
}

bool update_ui_guard(void) {
  return ((s0)[s0_idx]) >= ((uint32_t)(200000));
}

uint8_t update_ui_arg_state(void) {
  return (s2)[s2_idx];
}

uint8_t update_ui_arg_mode(void) {
  return c_mode_cpy;
}

int32_t update_ui_arg_flow(void) {
  return (int32_t)(0);
}

int32_t update_ui_arg_volume_ml(void) {
  return (int32_t)((int64_t)((((double)(8171.282492)) * (((((double)(85.0)) * ((cos)((((double)(3.141592653589793)) * ((((double)(110.51730062094212)) - ((double)(((((s_encoder_position_cpy) - ((s8)[s8_idx])) * ((int32_t)(360000))) / ((int32_t)(2048))) / ((int32_t)(1000))))) / ((double)(180.0))))))) + ((sqrt)((((double)(14400.0)) - ((pow)((((double)(75.0)) - (((double)(85.0)) * ((sin)((((double)(3.141592653589793)) * ((((double)(110.51730062094212)) - ((double)(((((s_encoder_position_cpy) - ((s8)[s8_idx])) * ((int32_t)(360000))) / ((int32_t)(2048))) / ((int32_t)(1000))))) / ((double)(180.0)))))))), ((double)(2.0)))))))) - ((double)(90.12)))) / ((double)(1000.0))));
}

int32_t update_ui_arg_pressure(void) {
  return (int32_t)(0);
}

uint8_t update_ui_arg_bpm_limited(void) {
  return ((c_bpm_cpy) <= ((uint8_t)(6))) ? (uint8_t)(6) : (((c_bpm_cpy) >= ((uint8_t)(40))) ? (uint8_t)(40) : (c_bpm_cpy));
}

uint8_t update_ui_arg_ie_inhale(void) {
  return ((((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) <= (((uint8_t)(4)) * (c_ie_inhale_cpy))) ? c_ie_inhale_cpy : ((((((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) % ((uint8_t)(4))) == ((uint8_t)(0))) ? (((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) / ((uint8_t)(4)) : (((((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy)) / ((uint8_t)(4))) + ((uint8_t)(1))));
}

uint8_t update_ui_arg_ie_exhale(void) {
  return ((c_ie_exhale_cpy) <= ((uint8_t)(0))) ? (uint8_t)(1) : (c_ie_exhale_cpy);
}

bool update_ui_arg_cmv_mode(void) {
  return c_cmv_mode_cpy;
}

uint32_t update_ui_arg_cmv_volume_goal(void) {
  return ((c_cmv_volume_goal_cpy) <= ((uint32_t)(0))) ? (uint32_t)(0) : (((c_cmv_volume_goal_cpy) >= ((uint32_t)(1600))) ? (uint32_t)(1600) : (c_cmv_volume_goal_cpy));
}

uint32_t update_ui_arg_cmv_pressure_goal(void) {
  return ((c_cmv_pressure_goal_cpy) <= ((uint32_t)(784))) ? (uint32_t)(784) : (((c_cmv_pressure_goal_cpy) >= ((uint32_t)(1960))) ? (uint32_t)(1960) : (c_cmv_pressure_goal_cpy));
}

bool control_motor_guard(void) {
  return true;
}

int32_t control_motor_arg_us_per_pulse(void) {
  int32_t local_9 = ((s10)[s10_idx]);
  int32_t local_12 = ((int32_t)((int64_t)((((double)(8171.282492)) * (((((double)(85.0)) * ((cos)((((double)(3.141592653589793)) * ((((double)(110.51730062094212)) - ((double)(((((s_encoder_position_cpy) - ((s8)[s8_idx])) * ((int32_t)(360000))) / ((int32_t)(2048))) / ((int32_t)(1000))))) / ((double)(180.0))))))) + ((sqrt)((((double)(14400.0)) - ((pow)((((double)(75.0)) - (((double)(85.0)) * ((sin)((((double)(3.141592653589793)) * ((((double)(110.51730062094212)) - ((double)(((((s_encoder_position_cpy) - ((s8)[s8_idx])) * ((int32_t)(360000))) / ((int32_t)(2048))) / ((int32_t)(1000))))) / ((double)(180.0)))))))), ((double)(2.0)))))))) - ((double)(90.12)))) / ((double)(1000.0)))));
  int32_t local_14 = ((s10)[s10_idx]);
  int32_t local_15 = ((int32_t)((int64_t)((((double)(8171.282492)) * (((((double)(85.0)) * ((cos)((((double)(3.141592653589793)) * ((((double)(110.51730062094212)) - ((double)(((((s_encoder_position_cpy) - ((s8)[s8_idx])) * ((int32_t)(360000))) / ((int32_t)(2048))) / ((int32_t)(1000))))) / ((double)(180.0))))))) + ((sqrt)((((double)(14400.0)) - ((pow)((((double)(75.0)) - (((double)(85.0)) * ((sin)((((double)(3.141592653589793)) * ((((double)(110.51730062094212)) - ((double)(((((s_encoder_position_cpy) - ((s8)[s8_idx])) * ((int32_t)(360000))) / ((int32_t)(2048))) / ((int32_t)(1000))))) / ((double)(180.0)))))))), ((double)(2.0)))))))) - ((double)(90.12)))) / ((double)(1000.0)))));
  int32_t local_16 = ((s10)[s10_idx]);
  int32_t local_17 = ((int32_t)((int64_t)((((double)(8171.282492)) * (((((double)(85.0)) * ((cos)((((double)(3.141592653589793)) * ((((double)(110.51730062094212)) - ((double)(((((s_encoder_position_cpy) - ((s8)[s8_idx])) * ((int32_t)(360000))) / ((int32_t)(2048))) / ((int32_t)(1000))))) / ((double)(180.0))))))) + ((sqrt)((((double)(14400.0)) - ((pow)((((double)(75.0)) - (((double)(85.0)) * ((sin)((((double)(3.141592653589793)) * ((((double)(110.51730062094212)) - ((double)(((((s_encoder_position_cpy) - ((s8)[s8_idx])) * ((int32_t)(360000))) / ((int32_t)(2048))) / ((int32_t)(1000))))) / ((double)(180.0)))))))), ((double)(2.0)))))))) - ((double)(90.12)))) / ((double)(1000.0)))));
  int32_t local_18 = ((s10)[s10_idx]);
  int32_t local_19 = ((int32_t)((int64_t)((((double)(8171.282492)) * (((((double)(85.0)) * ((cos)((((double)(3.141592653589793)) * ((((double)(110.51730062094212)) - ((double)(((((s_encoder_position_cpy) - ((s8)[s8_idx])) * ((int32_t)(360000))) / ((int32_t)(2048))) / ((int32_t)(1000))))) / ((double)(180.0))))))) + ((sqrt)((((double)(14400.0)) - ((pow)((((double)(75.0)) - (((double)(85.0)) * ((sin)((((double)(3.141592653589793)) * ((((double)(110.51730062094212)) - ((double)(((((s_encoder_position_cpy) - ((s8)[s8_idx])) * ((int32_t)(360000))) / ((int32_t)(2048))) / ((int32_t)(1000))))) / ((double)(180.0)))))))), ((double)(2.0)))))))) - ((double)(90.12)))) / ((double)(1000.0)))));
  uint8_t local_20 = ((s21)[s21_idx]);
  int32_t local_27 = ((s28)[s28_idx]);
  int32_t local_30 = ((int32_t)((int64_t)((((double)(8171.282492)) * (((((double)(85.0)) * ((cos)((((double)(3.141592653589793)) * ((((double)(110.51730062094212)) - ((double)(((((s_encoder_position_cpy) - ((s8)[s8_idx])) * ((int32_t)(360000))) / ((int32_t)(2048))) / ((int32_t)(1000))))) / ((double)(180.0))))))) + ((sqrt)((((double)(14400.0)) - ((pow)((((double)(75.0)) - (((double)(85.0)) * ((sin)((((double)(3.141592653589793)) * ((((double)(110.51730062094212)) - ((double)(((((s_encoder_position_cpy) - ((s8)[s8_idx])) * ((int32_t)(360000))) / ((int32_t)(2048))) / ((int32_t)(1000))))) / ((double)(180.0)))))))), ((double)(2.0)))))))) - ((double)(90.12)))) / ((double)(1000.0)))));
  uint8_t local_31 = ((s21)[s21_idx]);
  int32_t local_32 = ((s28)[s28_idx]);
  int32_t local_33 = ((int32_t)((int64_t)((((double)(8171.282492)) * (((((double)(85.0)) * ((cos)((((double)(3.141592653589793)) * ((((double)(110.51730062094212)) - ((double)(((((s_encoder_position_cpy) - ((s8)[s8_idx])) * ((int32_t)(360000))) / ((int32_t)(2048))) / ((int32_t)(1000))))) / ((double)(180.0))))))) + ((sqrt)((((double)(14400.0)) - ((pow)((((double)(75.0)) - (((double)(85.0)) * ((sin)((((double)(3.141592653589793)) * ((((double)(110.51730062094212)) - ((double)(((((s_encoder_position_cpy) - ((s8)[s8_idx])) * ((int32_t)(360000))) / ((int32_t)(2048))) / ((int32_t)(1000))))) / ((double)(180.0)))))))), ((double)(2.0)))))))) - ((double)(90.12)))) / ((double)(1000.0)))));
  uint8_t local_34 = ((s21)[s21_idx]);
  int32_t local_35 = ((s28)[s28_idx]);
  int32_t local_36 = ((int32_t)((int64_t)((((double)(8171.282492)) * (((((double)(85.0)) * ((cos)((((double)(3.141592653589793)) * ((((double)(110.51730062094212)) - ((double)(((((s_encoder_position_cpy) - ((s8)[s8_idx])) * ((int32_t)(360000))) / ((int32_t)(2048))) / ((int32_t)(1000))))) / ((double)(180.0))))))) + ((sqrt)((((double)(14400.0)) - ((pow)((((double)(75.0)) - (((double)(85.0)) * ((sin)((((double)(3.141592653589793)) * ((((double)(110.51730062094212)) - ((double)(((((s_encoder_position_cpy) - ((s8)[s8_idx])) * ((int32_t)(360000))) / ((int32_t)(2048))) / ((int32_t)(1000))))) / ((double)(180.0)))))))), ((double)(2.0)))))))) - ((double)(90.12)))) / ((double)(1000.0)))));
  uint8_t local_37 = ((s21)[s21_idx]);
  int32_t local_38 = ((s28)[s28_idx]);
  int32_t local_39 = ((int32_t)((int64_t)((((double)(8171.282492)) * (((((double)(85.0)) * ((cos)((((double)(3.141592653589793)) * ((((double)(110.51730062094212)) - ((double)(((((s_encoder_position_cpy) - ((s8)[s8_idx])) * ((int32_t)(360000))) / ((int32_t)(2048))) / ((int32_t)(1000))))) / ((double)(180.0))))))) + ((sqrt)((((double)(14400.0)) - ((pow)((((double)(75.0)) - (((double)(85.0)) * ((sin)((((double)(3.141592653589793)) * ((((double)(110.51730062094212)) - ((double)(((((s_encoder_position_cpy) - ((s8)[s8_idx])) * ((int32_t)(360000))) / ((int32_t)(2048))) / ((int32_t)(1000))))) / ((double)(180.0)))))))), ((double)(2.0)))))))) - ((double)(90.12)))) / ((double)(1000.0)))));
  int32_t local_40 = ((s10)[s10_idx]);
  int32_t local_41 = ((int32_t)((int64_t)((((double)(8171.282492)) * (((((double)(85.0)) * ((cos)((((double)(3.141592653589793)) * ((((double)(110.51730062094212)) - ((double)(((((s_encoder_position_cpy) - ((s8)[s8_idx])) * ((int32_t)(360000))) / ((int32_t)(2048))) / ((int32_t)(1000))))) / ((double)(180.0))))))) + ((sqrt)((((double)(14400.0)) - ((pow)((((double)(75.0)) - (((double)(85.0)) * ((sin)((((double)(3.141592653589793)) * ((((double)(110.51730062094212)) - ((double)(((((s_encoder_position_cpy) - ((s8)[s8_idx])) * ((int32_t)(360000))) / ((int32_t)(2048))) / ((int32_t)(1000))))) / ((double)(180.0)))))))), ((double)(2.0)))))))) - ((double)(90.12)))) / ((double)(1000.0)))));
  int32_t local_42 = ((s10)[s10_idx]);
  int32_t local_43 = ((int32_t)((int64_t)((((double)(8171.282492)) * (((((double)(85.0)) * ((cos)((((double)(3.141592653589793)) * ((((double)(110.51730062094212)) - ((double)(((((s_encoder_position_cpy) - ((s8)[s8_idx])) * ((int32_t)(360000))) / ((int32_t)(2048))) / ((int32_t)(1000))))) / ((double)(180.0))))))) + ((sqrt)((((double)(14400.0)) - ((pow)((((double)(75.0)) - (((double)(85.0)) * ((sin)((((double)(3.141592653589793)) * ((((double)(110.51730062094212)) - ((double)(((((s_encoder_position_cpy) - ((s8)[s8_idx])) * ((int32_t)(360000))) / ((int32_t)(2048))) / ((int32_t)(1000))))) / ((double)(180.0)))))))), ((double)(2.0)))))))) - ((double)(90.12)))) / ((double)(1000.0)))));
  int32_t local_44 = ((s10)[s10_idx]);
  int32_t local_45 = ((int32_t)((int64_t)((((double)(8171.282492)) * (((((double)(85.0)) * ((cos)((((double)(3.141592653589793)) * ((((double)(110.51730062094212)) - ((double)(((((s_encoder_position_cpy) - ((s8)[s8_idx])) * ((int32_t)(360000))) / ((int32_t)(2048))) / ((int32_t)(1000))))) / ((double)(180.0))))))) + ((sqrt)((((double)(14400.0)) - ((pow)((((double)(75.0)) - (((double)(85.0)) * ((sin)((((double)(3.141592653589793)) * ((((double)(110.51730062094212)) - ((double)(((((s_encoder_position_cpy) - ((s8)[s8_idx])) * ((int32_t)(360000))) / ((int32_t)(2048))) / ((int32_t)(1000))))) / ((double)(180.0)))))))), ((double)(2.0)))))))) - ((double)(90.12)))) / ((double)(1000.0)))));
  int32_t local_46 = ((s10)[s10_idx]);
  int32_t local_47 = ((int32_t)((int64_t)((((double)(8171.282492)) * (((((double)(85.0)) * ((cos)((((double)(3.141592653589793)) * ((((double)(110.51730062094212)) - ((double)(((((s_encoder_position_cpy) - ((s8)[s8_idx])) * ((int32_t)(360000))) / ((int32_t)(2048))) / ((int32_t)(1000))))) / ((double)(180.0))))))) + ((sqrt)((((double)(14400.0)) - ((pow)((((double)(75.0)) - (((double)(85.0)) * ((sin)((((double)(3.141592653589793)) * ((((double)(110.51730062094212)) - ((double)(((((s_encoder_position_cpy) - ((s8)[s8_idx])) * ((int32_t)(360000))) / ((int32_t)(2048))) / ((int32_t)(1000))))) / ((double)(180.0)))))))), ((double)(2.0)))))))) - ((double)(90.12)))) / ((double)(1000.0)))));
  uint8_t local_48 = ((s21)[s21_idx]);
  int32_t local_49 = ((s28)[s28_idx]);
  int32_t local_50 = ((int32_t)((int64_t)((((double)(8171.282492)) * (((((double)(85.0)) * ((cos)((((double)(3.141592653589793)) * ((((double)(110.51730062094212)) - ((double)(((((s_encoder_position_cpy) - ((s8)[s8_idx])) * ((int32_t)(360000))) / ((int32_t)(2048))) / ((int32_t)(1000))))) / ((double)(180.0))))))) + ((sqrt)((((double)(14400.0)) - ((pow)((((double)(75.0)) - (((double)(85.0)) * ((sin)((((double)(3.141592653589793)) * ((((double)(110.51730062094212)) - ((double)(((((s_encoder_position_cpy) - ((s8)[s8_idx])) * ((int32_t)(360000))) / ((int32_t)(2048))) / ((int32_t)(1000))))) / ((double)(180.0)))))))), ((double)(2.0)))))))) - ((double)(90.12)))) / ((double)(1000.0)))));
  uint8_t local_51 = ((s21)[s21_idx]);
  int32_t local_52 = ((s28)[s28_idx]);
  int32_t local_53 = ((int32_t)((int64_t)((((double)(8171.282492)) * (((((double)(85.0)) * ((cos)((((double)(3.141592653589793)) * ((((double)(110.51730062094212)) - ((double)(((((s_encoder_position_cpy) - ((s8)[s8_idx])) * ((int32_t)(360000))) / ((int32_t)(2048))) / ((int32_t)(1000))))) / ((double)(180.0))))))) + ((sqrt)((((double)(14400.0)) - ((pow)((((double)(75.0)) - (((double)(85.0)) * ((sin)((((double)(3.141592653589793)) * ((((double)(110.51730062094212)) - ((double)(((((s_encoder_position_cpy) - ((s8)[s8_idx])) * ((int32_t)(360000))) / ((int32_t)(2048))) / ((int32_t)(1000))))) / ((double)(180.0)))))))), ((double)(2.0)))))))) - ((double)(90.12)))) / ((double)(1000.0)))));
  uint8_t local_54 = ((s21)[s21_idx]);
  int32_t local_55 = ((s28)[s28_idx]);
  int32_t local_56 = ((int32_t)((int64_t)((((double)(8171.282492)) * (((((double)(85.0)) * ((cos)((((double)(3.141592653589793)) * ((((double)(110.51730062094212)) - ((double)(((((s_encoder_position_cpy) - ((s8)[s8_idx])) * ((int32_t)(360000))) / ((int32_t)(2048))) / ((int32_t)(1000))))) / ((double)(180.0))))))) + ((sqrt)((((double)(14400.0)) - ((pow)((((double)(75.0)) - (((double)(85.0)) * ((sin)((((double)(3.141592653589793)) * ((((double)(110.51730062094212)) - ((double)(((((s_encoder_position_cpy) - ((s8)[s8_idx])) * ((int32_t)(360000))) / ((int32_t)(2048))) / ((int32_t)(1000))))) / ((double)(180.0)))))))), ((double)(2.0)))))))) - ((double)(90.12)))) / ((double)(1000.0)))));
  uint8_t local_57 = ((s21)[s21_idx]);
  int32_t local_58 = ((s28)[s28_idx]);
  int32_t local_59 = ((int32_t)((int64_t)((((double)(8171.282492)) * (((((double)(85.0)) * ((cos)((((double)(3.141592653589793)) * ((((double)(110.51730062094212)) - ((double)(((((s_encoder_position_cpy) - ((s8)[s8_idx])) * ((int32_t)(360000))) / ((int32_t)(2048))) / ((int32_t)(1000))))) / ((double)(180.0))))))) + ((sqrt)((((double)(14400.0)) - ((pow)((((double)(75.0)) - (((double)(85.0)) * ((sin)((((double)(3.141592653589793)) * ((((double)(110.51730062094212)) - ((double)(((((s_encoder_position_cpy) - ((s8)[s8_idx])) * ((int32_t)(360000))) / ((int32_t)(2048))) / ((int32_t)(1000))))) / ((double)(180.0)))))))), ((double)(2.0)))))))) - ((double)(90.12)))) / ((double)(1000.0)))));
  return (((((s2)[s2_idx]) == ((uint8_t)(128))) ? (int32_t)(0) : ((((s2)[s2_idx]) == ((uint8_t)(0))) ? ((((s3)[s3_idx]) == ((uint8_t)(1))) || (((s3)[s3_idx]) == ((uint8_t)(3)))) ? (int32_t)((!(((s3)[s3_idx]) == ((uint8_t)(3)))) ? (uint32_t)(30) : ((uint32_t)(0))) : (((int32_t)(0)) - ((int32_t)((!(((s3)[s3_idx]) == ((uint8_t)(3)))) ? (uint32_t)(30) : ((uint32_t)(0))))) : ((((s2)[s2_idx]) == ((uint8_t)(1))) ? (((s_encoder_position_cpy) - ((s8)[s8_idx])) <= ((int32_t)(2))) ? (int32_t)(0) : ((int32_t)(-30)) : ((((s2)[s2_idx]) == ((uint8_t)(2))) ? ((c_mode_cpy) == ((uint8_t)(0))) ? ((((local_9) > ((int32_t)(0))) ? ((c_cmv_mode_cpy) == (false)) ? ((local_12) >= ((int32_t)(((c_cmv_volume_goal_cpy) <= ((uint32_t)(0))) ? (uint32_t)(0) : (((c_cmv_volume_goal_cpy) >= ((uint32_t)(1600))) ? (uint32_t)(1600) : (c_cmv_volume_goal_cpy))))) ? (int32_t)(0) : (((((int32_t)(((c_cmv_volume_goal_cpy) <= ((uint32_t)(0))) ? (uint32_t)(0) : (((c_cmv_volume_goal_cpy) >= ((uint32_t)(1600))) ? (uint32_t)(1600) : (c_cmv_volume_goal_cpy)))) - (local_12)) > ((int32_t)(100))) ? (int32_t)(90) : ((int32_t)(45))) : (((c_cmv_mode_cpy) == (true)) ? (int32_t)(45) : ((int32_t)(0))) : (((local_9) < ((int32_t)(0))) ? ((local_9) > ((int32_t)(-10))) ? (int32_t)(0) : (((((((s8)[s8_idx]) - (s_encoder_position_cpy)) * ((int32_t)(360000))) / ((int32_t)(2048))) * ((int32_t)(2000))) / (((int32_t)(0)) - (local_9))) : ((int32_t)(0)))) > ((int32_t)(0))) ? ((((((s13)[s13_idx]) - (s_encoder_position_cpy)) * ((int32_t)(360000))) / ((int32_t)(2048))) <= ((int32_t)(0))) ? (int32_t)(0) : (((((((s13)[s13_idx]) - (s_encoder_position_cpy)) * ((int32_t)(360000))) / ((int32_t)(2048))) < ((int32_t)(1000))) ? (int32_t)(10) : (((local_14) > ((int32_t)(0))) ? ((c_cmv_mode_cpy) == (false)) ? ((local_15) >= ((int32_t)(((c_cmv_volume_goal_cpy) <= ((uint32_t)(0))) ? (uint32_t)(0) : (((c_cmv_volume_goal_cpy) >= ((uint32_t)(1600))) ? (uint32_t)(1600) : (c_cmv_volume_goal_cpy))))) ? (int32_t)(0) : (((((int32_t)(((c_cmv_volume_goal_cpy) <= ((uint32_t)(0))) ? (uint32_t)(0) : (((c_cmv_volume_goal_cpy) >= ((uint32_t)(1600))) ? (uint32_t)(1600) : (c_cmv_volume_goal_cpy)))) - (local_15)) > ((int32_t)(100))) ? (int32_t)(90) : ((int32_t)(45))) : (((c_cmv_mode_cpy) == (true)) ? (int32_t)(45) : ((int32_t)(0))) : (((local_14) < ((int32_t)(0))) ? ((local_14) > ((int32_t)(-10))) ? (int32_t)(0) : (((((((s8)[s8_idx]) - (s_encoder_position_cpy)) * ((int32_t)(360000))) / ((int32_t)(2048))) * ((int32_t)(2000))) / (((int32_t)(0)) - (local_14))) : ((int32_t)(0))))) : (((((local_16) > ((int32_t)(0))) ? ((c_cmv_mode_cpy) == (false)) ? ((local_17) >= ((int32_t)(((c_cmv_volume_goal_cpy) <= ((uint32_t)(0))) ? (uint32_t)(0) : (((c_cmv_volume_goal_cpy) >= ((uint32_t)(1600))) ? (uint32_t)(1600) : (c_cmv_volume_goal_cpy))))) ? (int32_t)(0) : (((((int32_t)(((c_cmv_volume_goal_cpy) <= ((uint32_t)(0))) ? (uint32_t)(0) : (((c_cmv_volume_goal_cpy) >= ((uint32_t)(1600))) ? (uint32_t)(1600) : (c_cmv_volume_goal_cpy)))) - (local_17)) > ((int32_t)(100))) ? (int32_t)(90) : ((int32_t)(45))) : (((c_cmv_mode_cpy) == (true)) ? (int32_t)(45) : ((int32_t)(0))) : (((local_16) < ((int32_t)(0))) ? ((local_16) > ((int32_t)(-10))) ? (int32_t)(0) : (((((((s8)[s8_idx]) - (s_encoder_position_cpy)) * ((int32_t)(360000))) / ((int32_t)(2048))) * ((int32_t)(2000))) / (((int32_t)(0)) - (local_16))) : ((int32_t)(0)))) < ((int32_t)(0))) ? (((((s_encoder_position_cpy) - ((s8)[s8_idx])) * ((int32_t)(360000))) / ((int32_t)(2048))) <= ((int32_t)(0))) ? (int32_t)(0) : ((((((s_encoder_position_cpy) - ((s8)[s8_idx])) * ((int32_t)(360000))) / ((int32_t)(2048))) < ((int32_t)(1000))) ? (int32_t)(-10) : (((local_18) > ((int32_t)(0))) ? ((c_cmv_mode_cpy) == (false)) ? ((local_19) >= ((int32_t)(((c_cmv_volume_goal_cpy) <= ((uint32_t)(0))) ? (uint32_t)(0) : (((c_cmv_volume_goal_cpy) >= ((uint32_t)(1600))) ? (uint32_t)(1600) : (c_cmv_volume_goal_cpy))))) ? (int32_t)(0) : (((((int32_t)(((c_cmv_volume_goal_cpy) <= ((uint32_t)(0))) ? (uint32_t)(0) : (((c_cmv_volume_goal_cpy) >= ((uint32_t)(1600))) ? (uint32_t)(1600) : (c_cmv_volume_goal_cpy)))) - (local_19)) > ((int32_t)(100))) ? (int32_t)(90) : ((int32_t)(45))) : (((c_cmv_mode_cpy) == (true)) ? (int32_t)(45) : ((int32_t)(0))) : (((local_18) < ((int32_t)(0))) ? ((local_18) > ((int32_t)(-10))) ? (int32_t)(0) : (((((((s8)[s8_idx]) - (s_encoder_position_cpy)) * ((int32_t)(360000))) / ((int32_t)(2048))) * ((int32_t)(2000))) / (((int32_t)(0)) - (local_18))) : ((int32_t)(0))))) : ((int32_t)(0))) : (((c_mode_cpy) == ((uint8_t)(1))) ? ((((local_20) == ((uint8_t)(0))) ? ((c_cmv_mode_cpy) == (false)) ? ((local_30) >= ((int32_t)(((c_cmv_volume_goal_cpy) <= ((uint32_t)(0))) ? (uint32_t)(0) : (((c_cmv_volume_goal_cpy) >= ((uint32_t)(1600))) ? (uint32_t)(1600) : (c_cmv_volume_goal_cpy))))) ? (int32_t)(0) : (((((int32_t)(((c_cmv_volume_goal_cpy) <= ((uint32_t)(0))) ? (uint32_t)(0) : (((c_cmv_volume_goal_cpy) >= ((uint32_t)(1600))) ? (uint32_t)(1600) : (c_cmv_volume_goal_cpy)))) - (local_30)) > ((int32_t)(100))) ? (int32_t)(90) : ((int32_t)(45))) : (((c_cmv_mode_cpy) == (true)) ? (int32_t)(45) : ((int32_t)(0))) : (((local_20) == ((uint8_t)(1))) ? ((local_27) > ((int32_t)(-10))) ? (int32_t)(0) : (((((((s8)[s8_idx]) - (s_encoder_position_cpy)) * ((int32_t)(360000))) / ((int32_t)(2048))) * ((int32_t)(2000))) / (((int32_t)(0)) - (local_27))) : (((local_20) == ((uint8_t)(2))) ? (int32_t)(30) : ((int32_t)(0))))) > ((int32_t)(0))) ? ((((((s13)[s13_idx]) - (s_encoder_position_cpy)) * ((int32_t)(360000))) / ((int32_t)(2048))) <= ((int32_t)(0))) ? (int32_t)(0) : (((((((s13)[s13_idx]) - (s_encoder_position_cpy)) * ((int32_t)(360000))) / ((int32_t)(2048))) < ((int32_t)(1000))) ? (int32_t)(10) : (((local_31) == ((uint8_t)(0))) ? ((c_cmv_mode_cpy) == (false)) ? ((local_33) >= ((int32_t)(((c_cmv_volume_goal_cpy) <= ((uint32_t)(0))) ? (uint32_t)(0) : (((c_cmv_volume_goal_cpy) >= ((uint32_t)(1600))) ? (uint32_t)(1600) : (c_cmv_volume_goal_cpy))))) ? (int32_t)(0) : (((((int32_t)(((c_cmv_volume_goal_cpy) <= ((uint32_t)(0))) ? (uint32_t)(0) : (((c_cmv_volume_goal_cpy) >= ((uint32_t)(1600))) ? (uint32_t)(1600) : (c_cmv_volume_goal_cpy)))) - (local_33)) > ((int32_t)(100))) ? (int32_t)(90) : ((int32_t)(45))) : (((c_cmv_mode_cpy) == (true)) ? (int32_t)(45) : ((int32_t)(0))) : (((local_31) == ((uint8_t)(1))) ? ((local_32) > ((int32_t)(-10))) ? (int32_t)(0) : (((((((s8)[s8_idx]) - (s_encoder_position_cpy)) * ((int32_t)(360000))) / ((int32_t)(2048))) * ((int32_t)(2000))) / (((int32_t)(0)) - (local_32))) : (((local_31) == ((uint8_t)(2))) ? (int32_t)(30) : ((int32_t)(0)))))) : (((((local_34) == ((uint8_t)(0))) ? ((c_cmv_mode_cpy) == (false)) ? ((local_36) >= ((int32_t)(((c_cmv_volume_goal_cpy) <= ((uint32_t)(0))) ? (uint32_t)(0) : (((c_cmv_volume_goal_cpy) >= ((uint32_t)(1600))) ? (uint32_t)(1600) : (c_cmv_volume_goal_cpy))))) ? (int32_t)(0) : (((((int32_t)(((c_cmv_volume_goal_cpy) <= ((uint32_t)(0))) ? (uint32_t)(0) : (((c_cmv_volume_goal_cpy) >= ((uint32_t)(1600))) ? (uint32_t)(1600) : (c_cmv_volume_goal_cpy)))) - (local_36)) > ((int32_t)(100))) ? (int32_t)(90) : ((int32_t)(45))) : (((c_cmv_mode_cpy) == (true)) ? (int32_t)(45) : ((int32_t)(0))) : (((local_34) == ((uint8_t)(1))) ? ((local_35) > ((int32_t)(-10))) ? (int32_t)(0) : (((((((s8)[s8_idx]) - (s_encoder_position_cpy)) * ((int32_t)(360000))) / ((int32_t)(2048))) * ((int32_t)(2000))) / (((int32_t)(0)) - (local_35))) : (((local_34) == ((uint8_t)(2))) ? (int32_t)(30) : ((int32_t)(0))))) < ((int32_t)(0))) ? (((((s_encoder_position_cpy) - ((s8)[s8_idx])) * ((int32_t)(360000))) / ((int32_t)(2048))) <= ((int32_t)(0))) ? (int32_t)(0) : ((((((s_encoder_position_cpy) - ((s8)[s8_idx])) * ((int32_t)(360000))) / ((int32_t)(2048))) < ((int32_t)(1000))) ? (int32_t)(-10) : (((local_37) == ((uint8_t)(0))) ? ((c_cmv_mode_cpy) == (false)) ? ((local_39) >= ((int32_t)(((c_cmv_volume_goal_cpy) <= ((uint32_t)(0))) ? (uint32_t)(0) : (((c_cmv_volume_goal_cpy) >= ((uint32_t)(1600))) ? (uint32_t)(1600) : (c_cmv_volume_goal_cpy))))) ? (int32_t)(0) : (((((int32_t)(((c_cmv_volume_goal_cpy) <= ((uint32_t)(0))) ? (uint32_t)(0) : (((c_cmv_volume_goal_cpy) >= ((uint32_t)(1600))) ? (uint32_t)(1600) : (c_cmv_volume_goal_cpy)))) - (local_39)) > ((int32_t)(100))) ? (int32_t)(90) : ((int32_t)(45))) : (((c_cmv_mode_cpy) == (true)) ? (int32_t)(45) : ((int32_t)(0))) : (((local_37) == ((uint8_t)(1))) ? ((local_38) > ((int32_t)(-10))) ? (int32_t)(0) : (((((((s8)[s8_idx]) - (s_encoder_position_cpy)) * ((int32_t)(360000))) / ((int32_t)(2048))) * ((int32_t)(2000))) / (((int32_t)(0)) - (local_38))) : (((local_37) == ((uint8_t)(2))) ? (int32_t)(30) : ((int32_t)(0)))))) : ((int32_t)(0))) : ((int32_t)(0))) : ((((s2)[s2_idx]) == ((uint8_t)(3))) ? (int32_t)(0) : ((((s2)[s2_idx]) == ((uint8_t)(4))) ? (int32_t)(-30) : ((int32_t)(0)))))))) == ((int32_t)(0))) ? (int32_t)(0) : (((int32_t)(360000000)) / (((int32_t)(3200)) * ((((s2)[s2_idx]) == ((uint8_t)(128))) ? (int32_t)(0) : ((((s2)[s2_idx]) == ((uint8_t)(0))) ? ((((s3)[s3_idx]) == ((uint8_t)(1))) || (((s3)[s3_idx]) == ((uint8_t)(3)))) ? (int32_t)((!(((s3)[s3_idx]) == ((uint8_t)(3)))) ? (uint32_t)(30) : ((uint32_t)(0))) : (((int32_t)(0)) - ((int32_t)((!(((s3)[s3_idx]) == ((uint8_t)(3)))) ? (uint32_t)(30) : ((uint32_t)(0))))) : ((((s2)[s2_idx]) == ((uint8_t)(1))) ? (((s_encoder_position_cpy) - ((s8)[s8_idx])) <= ((int32_t)(2))) ? (int32_t)(0) : ((int32_t)(-30)) : ((((s2)[s2_idx]) == ((uint8_t)(2))) ? ((c_mode_cpy) == ((uint8_t)(0))) ? ((((local_40) > ((int32_t)(0))) ? ((c_cmv_mode_cpy) == (false)) ? ((local_41) >= ((int32_t)(((c_cmv_volume_goal_cpy) <= ((uint32_t)(0))) ? (uint32_t)(0) : (((c_cmv_volume_goal_cpy) >= ((uint32_t)(1600))) ? (uint32_t)(1600) : (c_cmv_volume_goal_cpy))))) ? (int32_t)(0) : (((((int32_t)(((c_cmv_volume_goal_cpy) <= ((uint32_t)(0))) ? (uint32_t)(0) : (((c_cmv_volume_goal_cpy) >= ((uint32_t)(1600))) ? (uint32_t)(1600) : (c_cmv_volume_goal_cpy)))) - (local_41)) > ((int32_t)(100))) ? (int32_t)(90) : ((int32_t)(45))) : (((c_cmv_mode_cpy) == (true)) ? (int32_t)(45) : ((int32_t)(0))) : (((local_40) < ((int32_t)(0))) ? ((local_40) > ((int32_t)(-10))) ? (int32_t)(0) : (((((((s8)[s8_idx]) - (s_encoder_position_cpy)) * ((int32_t)(360000))) / ((int32_t)(2048))) * ((int32_t)(2000))) / (((int32_t)(0)) - (local_40))) : ((int32_t)(0)))) > ((int32_t)(0))) ? ((((((s13)[s13_idx]) - (s_encoder_position_cpy)) * ((int32_t)(360000))) / ((int32_t)(2048))) <= ((int32_t)(0))) ? (int32_t)(0) : (((((((s13)[s13_idx]) - (s_encoder_position_cpy)) * ((int32_t)(360000))) / ((int32_t)(2048))) < ((int32_t)(1000))) ? (int32_t)(10) : (((local_42) > ((int32_t)(0))) ? ((c_cmv_mode_cpy) == (false)) ? ((local_43) >= ((int32_t)(((c_cmv_volume_goal_cpy) <= ((uint32_t)(0))) ? (uint32_t)(0) : (((c_cmv_volume_goal_cpy) >= ((uint32_t)(1600))) ? (uint32_t)(1600) : (c_cmv_volume_goal_cpy))))) ? (int32_t)(0) : (((((int32_t)(((c_cmv_volume_goal_cpy) <= ((uint32_t)(0))) ? (uint32_t)(0) : (((c_cmv_volume_goal_cpy) >= ((uint32_t)(1600))) ? (uint32_t)(1600) : (c_cmv_volume_goal_cpy)))) - (local_43)) > ((int32_t)(100))) ? (int32_t)(90) : ((int32_t)(45))) : (((c_cmv_mode_cpy) == (true)) ? (int32_t)(45) : ((int32_t)(0))) : (((local_42) < ((int32_t)(0))) ? ((local_42) > ((int32_t)(-10))) ? (int32_t)(0) : (((((((s8)[s8_idx]) - (s_encoder_position_cpy)) * ((int32_t)(360000))) / ((int32_t)(2048))) * ((int32_t)(2000))) / (((int32_t)(0)) - (local_42))) : ((int32_t)(0))))) : (((((local_44) > ((int32_t)(0))) ? ((c_cmv_mode_cpy) == (false)) ? ((local_45) >= ((int32_t)(((c_cmv_volume_goal_cpy) <= ((uint32_t)(0))) ? (uint32_t)(0) : (((c_cmv_volume_goal_cpy) >= ((uint32_t)(1600))) ? (uint32_t)(1600) : (c_cmv_volume_goal_cpy))))) ? (int32_t)(0) : (((((int32_t)(((c_cmv_volume_goal_cpy) <= ((uint32_t)(0))) ? (uint32_t)(0) : (((c_cmv_volume_goal_cpy) >= ((uint32_t)(1600))) ? (uint32_t)(1600) : (c_cmv_volume_goal_cpy)))) - (local_45)) > ((int32_t)(100))) ? (int32_t)(90) : ((int32_t)(45))) : (((c_cmv_mode_cpy) == (true)) ? (int32_t)(45) : ((int32_t)(0))) : (((local_44) < ((int32_t)(0))) ? ((local_44) > ((int32_t)(-10))) ? (int32_t)(0) : (((((((s8)[s8_idx]) - (s_encoder_position_cpy)) * ((int32_t)(360000))) / ((int32_t)(2048))) * ((int32_t)(2000))) / (((int32_t)(0)) - (local_44))) : ((int32_t)(0)))) < ((int32_t)(0))) ? (((((s_encoder_position_cpy) - ((s8)[s8_idx])) * ((int32_t)(360000))) / ((int32_t)(2048))) <= ((int32_t)(0))) ? (int32_t)(0) : ((((((s_encoder_position_cpy) - ((s8)[s8_idx])) * ((int32_t)(360000))) / ((int32_t)(2048))) < ((int32_t)(1000))) ? (int32_t)(-10) : (((local_46) > ((int32_t)(0))) ? ((c_cmv_mode_cpy) == (false)) ? ((local_47) >= ((int32_t)(((c_cmv_volume_goal_cpy) <= ((uint32_t)(0))) ? (uint32_t)(0) : (((c_cmv_volume_goal_cpy) >= ((uint32_t)(1600))) ? (uint32_t)(1600) : (c_cmv_volume_goal_cpy))))) ? (int32_t)(0) : (((((int32_t)(((c_cmv_volume_goal_cpy) <= ((uint32_t)(0))) ? (uint32_t)(0) : (((c_cmv_volume_goal_cpy) >= ((uint32_t)(1600))) ? (uint32_t)(1600) : (c_cmv_volume_goal_cpy)))) - (local_47)) > ((int32_t)(100))) ? (int32_t)(90) : ((int32_t)(45))) : (((c_cmv_mode_cpy) == (true)) ? (int32_t)(45) : ((int32_t)(0))) : (((local_46) < ((int32_t)(0))) ? ((local_46) > ((int32_t)(-10))) ? (int32_t)(0) : (((((((s8)[s8_idx]) - (s_encoder_position_cpy)) * ((int32_t)(360000))) / ((int32_t)(2048))) * ((int32_t)(2000))) / (((int32_t)(0)) - (local_46))) : ((int32_t)(0))))) : ((int32_t)(0))) : (((c_mode_cpy) == ((uint8_t)(1))) ? ((((local_48) == ((uint8_t)(0))) ? ((c_cmv_mode_cpy) == (false)) ? ((local_50) >= ((int32_t)(((c_cmv_volume_goal_cpy) <= ((uint32_t)(0))) ? (uint32_t)(0) : (((c_cmv_volume_goal_cpy) >= ((uint32_t)(1600))) ? (uint32_t)(1600) : (c_cmv_volume_goal_cpy))))) ? (int32_t)(0) : (((((int32_t)(((c_cmv_volume_goal_cpy) <= ((uint32_t)(0))) ? (uint32_t)(0) : (((c_cmv_volume_goal_cpy) >= ((uint32_t)(1600))) ? (uint32_t)(1600) : (c_cmv_volume_goal_cpy)))) - (local_50)) > ((int32_t)(100))) ? (int32_t)(90) : ((int32_t)(45))) : (((c_cmv_mode_cpy) == (true)) ? (int32_t)(45) : ((int32_t)(0))) : (((local_48) == ((uint8_t)(1))) ? ((local_49) > ((int32_t)(-10))) ? (int32_t)(0) : (((((((s8)[s8_idx]) - (s_encoder_position_cpy)) * ((int32_t)(360000))) / ((int32_t)(2048))) * ((int32_t)(2000))) / (((int32_t)(0)) - (local_49))) : (((local_48) == ((uint8_t)(2))) ? (int32_t)(30) : ((int32_t)(0))))) > ((int32_t)(0))) ? ((((((s13)[s13_idx]) - (s_encoder_position_cpy)) * ((int32_t)(360000))) / ((int32_t)(2048))) <= ((int32_t)(0))) ? (int32_t)(0) : (((((((s13)[s13_idx]) - (s_encoder_position_cpy)) * ((int32_t)(360000))) / ((int32_t)(2048))) < ((int32_t)(1000))) ? (int32_t)(10) : (((local_51) == ((uint8_t)(0))) ? ((c_cmv_mode_cpy) == (false)) ? ((local_53) >= ((int32_t)(((c_cmv_volume_goal_cpy) <= ((uint32_t)(0))) ? (uint32_t)(0) : (((c_cmv_volume_goal_cpy) >= ((uint32_t)(1600))) ? (uint32_t)(1600) : (c_cmv_volume_goal_cpy))))) ? (int32_t)(0) : (((((int32_t)(((c_cmv_volume_goal_cpy) <= ((uint32_t)(0))) ? (uint32_t)(0) : (((c_cmv_volume_goal_cpy) >= ((uint32_t)(1600))) ? (uint32_t)(1600) : (c_cmv_volume_goal_cpy)))) - (local_53)) > ((int32_t)(100))) ? (int32_t)(90) : ((int32_t)(45))) : (((c_cmv_mode_cpy) == (true)) ? (int32_t)(45) : ((int32_t)(0))) : (((local_51) == ((uint8_t)(1))) ? ((local_52) > ((int32_t)(-10))) ? (int32_t)(0) : (((((((s8)[s8_idx]) - (s_encoder_position_cpy)) * ((int32_t)(360000))) / ((int32_t)(2048))) * ((int32_t)(2000))) / (((int32_t)(0)) - (local_52))) : (((local_51) == ((uint8_t)(2))) ? (int32_t)(30) : ((int32_t)(0)))))) : (((((local_54) == ((uint8_t)(0))) ? ((c_cmv_mode_cpy) == (false)) ? ((local_56) >= ((int32_t)(((c_cmv_volume_goal_cpy) <= ((uint32_t)(0))) ? (uint32_t)(0) : (((c_cmv_volume_goal_cpy) >= ((uint32_t)(1600))) ? (uint32_t)(1600) : (c_cmv_volume_goal_cpy))))) ? (int32_t)(0) : (((((int32_t)(((c_cmv_volume_goal_cpy) <= ((uint32_t)(0))) ? (uint32_t)(0) : (((c_cmv_volume_goal_cpy) >= ((uint32_t)(1600))) ? (uint32_t)(1600) : (c_cmv_volume_goal_cpy)))) - (local_56)) > ((int32_t)(100))) ? (int32_t)(90) : ((int32_t)(45))) : (((c_cmv_mode_cpy) == (true)) ? (int32_t)(45) : ((int32_t)(0))) : (((local_54) == ((uint8_t)(1))) ? ((local_55) > ((int32_t)(-10))) ? (int32_t)(0) : (((((((s8)[s8_idx]) - (s_encoder_position_cpy)) * ((int32_t)(360000))) / ((int32_t)(2048))) * ((int32_t)(2000))) / (((int32_t)(0)) - (local_55))) : (((local_54) == ((uint8_t)(2))) ? (int32_t)(30) : ((int32_t)(0))))) < ((int32_t)(0))) ? (((((s_encoder_position_cpy) - ((s8)[s8_idx])) * ((int32_t)(360000))) / ((int32_t)(2048))) <= ((int32_t)(0))) ? (int32_t)(0) : ((((((s_encoder_position_cpy) - ((s8)[s8_idx])) * ((int32_t)(360000))) / ((int32_t)(2048))) < ((int32_t)(1000))) ? (int32_t)(-10) : (((local_57) == ((uint8_t)(0))) ? ((c_cmv_mode_cpy) == (false)) ? ((local_59) >= ((int32_t)(((c_cmv_volume_goal_cpy) <= ((uint32_t)(0))) ? (uint32_t)(0) : (((c_cmv_volume_goal_cpy) >= ((uint32_t)(1600))) ? (uint32_t)(1600) : (c_cmv_volume_goal_cpy))))) ? (int32_t)(0) : (((((int32_t)(((c_cmv_volume_goal_cpy) <= ((uint32_t)(0))) ? (uint32_t)(0) : (((c_cmv_volume_goal_cpy) >= ((uint32_t)(1600))) ? (uint32_t)(1600) : (c_cmv_volume_goal_cpy)))) - (local_59)) > ((int32_t)(100))) ? (int32_t)(90) : ((int32_t)(45))) : (((c_cmv_mode_cpy) == (true)) ? (int32_t)(45) : ((int32_t)(0))) : (((local_57) == ((uint8_t)(1))) ? ((local_58) > ((int32_t)(-10))) ? (int32_t)(0) : (((((((s8)[s8_idx]) - (s_encoder_position_cpy)) * ((int32_t)(360000))) / ((int32_t)(2048))) * ((int32_t)(2000))) / (((int32_t)(0)) - (local_58))) : (((local_57) == ((uint8_t)(2))) ? (int32_t)(30) : ((int32_t)(0)))))) : ((int32_t)(0))) : ((int32_t)(0))) : ((((s2)[s2_idx]) == ((uint8_t)(3))) ? (int32_t)(0) : ((((s2)[s2_idx]) == ((uint8_t)(4))) ? (int32_t)(-30) : ((int32_t)(0))))))))));
}

bool debug_guard(void) {
  return true;
}

int32_t debug_arg_encoder(void) {
  return s_encoder_position_cpy;
}

int32_t debug_arg_encoder_low(void) {
  return (s8)[s8_idx];
}

int32_t debug_arg_encoder_high(void) {
  return (s13)[s13_idx];
}

void step(void) {
  (t_delta_us_cpy) = (t_delta_us);
  (s_limit_low_cpy) = (s_limit_low);
  (s_limit_high_cpy) = (s_limit_high);
  (c_button_start_cpy) = (c_button_start);
  (c_button_stop_cpy) = (c_button_stop);
  (s_encoder_position_cpy) = (s_encoder_position);
  (c_bpm_cpy) = (c_bpm);
  (c_ie_exhale_cpy) = (c_ie_exhale);
  (c_ie_inhale_cpy) = (c_ie_inhale);
  (s_insp_flow_1_cpy) = (s_insp_flow_1);
  (c_mode_cpy) = (c_mode);
  (c_cmv_mode_cpy) = (c_cmv_mode);
  (c_cmv_volume_goal_cpy) = (c_cmv_volume_goal);
  (c_cmv_pressure_goal_cpy) = (c_cmv_pressure_goal);
  if ((raise_alarm_guard)()) {
    (raise_alarm)();
  };
  if ((update_ui_guard)()) {
    (update_ui)(((update_ui_arg_state)()), ((update_ui_arg_mode)()), ((update_ui_arg_flow)()), ((update_ui_arg_volume_ml)()), ((update_ui_arg_pressure)()), ((update_ui_arg_bpm_limited)()), ((update_ui_arg_ie_inhale)()), ((update_ui_arg_ie_exhale)()), ((update_ui_arg_cmv_mode)()), ((update_ui_arg_cmv_volume_goal)()), ((update_ui_arg_cmv_pressure_goal)()));
  };
  if ((control_motor_guard)()) {
    (control_motor)(((control_motor_arg_us_per_pulse)()));
  };
  if ((debug_guard)()) {
    (debug)(((debug_arg_encoder)()), ((debug_arg_encoder_low)()), ((debug_arg_encoder_high)()));
  };
  ((s1)[((s1_idx) + (1)) % (2)]) = ((s1_gen)());
  ((s0)[((s0_idx) + (1)) % (2)]) = ((s0_gen)());
  ((s4)[((s4_idx) + (1)) % (2)]) = ((s4_gen)());
  ((s5)[((s5_idx) + (1)) % (2)]) = ((s5_gen)());
  ((s3)[((s3_idx) + (1)) % (2)]) = ((s3_gen)());
  ((s6)[((s6_idx) + (1)) % (2)]) = ((s6_gen)());
  ((s7)[((s7_idx) + (1)) % (2)]) = ((s7_gen)());
  ((s2)[((s2_idx) + (1)) % (2)]) = ((s2_gen)());
  ((s8)[((s8_idx) + (1)) % (2)]) = ((s8_gen)());
  ((s11)[((s11_idx) + (1)) % (2)]) = ((s11_gen)());
  ((s10)[((s10_idx) + (1)) % (2)]) = ((s10_gen)());
  ((s13)[((s13_idx) + (1)) % (2)]) = ((s13_gen)());
  ((s22)[((s22_idx) + (1)) % (2)]) = ((s22_gen)());
  ((s24)[((s24_idx) + (1)) % (2)]) = ((s24_gen)());
  ((s23)[((s23_idx) + (1)) % (2)]) = ((s23_gen)());
  ((s26)[((s26_idx) + (1)) % (2)]) = ((s26_gen)());
  ((s25)[((s25_idx) + (1)) % (2)]) = ((s25_gen)());
  ((s21)[((s21_idx) + (1)) % (2)]) = ((s21_gen)());
  ((s29)[((s29_idx) + (1)) % (2)]) = ((s29_gen)());
  ((s28)[((s28_idx) + (1)) % (2)]) = ((s28_gen)());
  (s1_idx) = ((++(s1_idx)) % (2));
  (s0_idx) = ((++(s0_idx)) % (2));
  (s4_idx) = ((++(s4_idx)) % (3));
  (s5_idx) = ((++(s5_idx)) % (3));
  (s3_idx) = ((++(s3_idx)) % (2));
  (s6_idx) = ((++(s6_idx)) % (2));
  (s7_idx) = ((++(s7_idx)) % (2));
  (s2_idx) = ((++(s2_idx)) % (2));
  (s8_idx) = ((++(s8_idx)) % (2));
  (s11_idx) = ((++(s11_idx)) % (2));
  (s10_idx) = ((++(s10_idx)) % (2));
  (s13_idx) = ((++(s13_idx)) % (2));
  (s22_idx) = ((++(s22_idx)) % (2));
  (s24_idx) = ((++(s24_idx)) % (2));
  (s23_idx) = ((++(s23_idx)) % (2));
  (s26_idx) = ((++(s26_idx)) % (2));
  (s25_idx) = ((++(s25_idx)) % (2));
  (s21_idx) = ((++(s21_idx)) % (2));
  (s29_idx) = ((++(s29_idx)) % (2));
  (s28_idx) = ((++(s28_idx)) % (2));
}
