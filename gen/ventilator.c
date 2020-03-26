#include <stdint.h>
#include <stdbool.h>
#include <string.h>

#include "ventilator.h"

static uint32_t t_delta_us_cpy;
static uint8_t c_bpm_cpy;
static uint16_t c_ie_ratio_cpy;
static int32_t s_flow_cpy;
static bool c_cmv_mode_cpy;
static int32_t s_volume_cpy;
static uint32_t c_cmv_volume_goal_cpy;
static uint32_t c_volume_limit_cpy;
static int32_t s_internal_pressure_1_cpy;
static uint32_t c_cmv_pressure_goal_cpy;
static uint32_t c_pressure_limit_cpy;
static bool s_piston_high_cpy;
static bool s_piston_low_cpy;
static uint32_t s2[(2)] = {(0), (0)};
static uint32_t s1[(2)] = {(0), (0)};
static bool s0[(2)] = {(false), (0)};
static uint32_t s4[(2)] = {(0), (0)};
static bool s5[(2)] = {(false), (0)};
static uint32_t s3[(2)] = {(0), (0)};
static int8_t s6[(2)] = {(0), (0)};
static uint32_t s8[(2)] = {(0), (0)};
static bool s7[(2)] = {(false), (0)};
static size_t s2_idx = (0);
static size_t s1_idx = (0);
static size_t s0_idx = (0);
static size_t s4_idx = (0);
static size_t s5_idx = (0);
static size_t s3_idx = (0);
static size_t s6_idx = (0);
static size_t s8_idx = (0);
static size_t s7_idx = (0);

uint32_t s2_gen(void) {
  return t_delta_us_cpy;
}

uint32_t s1_gen(void) {
  return ((s0)[s0_idx]) ? 0 : (((s1)[s1_idx]) + ((s2)[s2_idx]));
}

bool s0_gen(void) {
  return (((s1)[s1_idx]) >= (10000)) ? true : (false);
}

uint32_t s4_gen(void) {
  return (((s4)[s4_idx]) >= ((s3)[s3_idx])) ? 0 : (((s4)[s4_idx]) + ((s2)[s2_idx]));
}

bool s5_gen(void) {
  return (((s4)[s4_idx]) >= ((s3)[s3_idx])) ? !((s5)[s5_idx]) : ((s5)[s5_idx]);
}

uint32_t s3_gen(void) {
  return (((s4)[s4_idx]) >= ((s3)[s3_idx])) ? (!((s5)[s5_idx])) ? (((((60000000) / ((uint32_t)(((c_bpm_cpy) <= (6)) ? 6 : (((c_bpm_cpy) >= (60)) ? 60 : (c_bpm_cpy))))) / ((uint32_t)(((uint8_t)(((((uint16_t)((((((uint8_t)((c_ie_ratio_cpy) >> (8))) == (0)) ? 1 : ((uint8_t)((c_ie_ratio_cpy) >> (8)))) > ((((uint8_t)((c_ie_ratio_cpy) & (255))) == (0)) ? 1 : ((uint8_t)((c_ie_ratio_cpy) & (255))))) ? (((uint8_t)((c_ie_ratio_cpy) & (255))) == (0)) ? 1 : ((uint8_t)((c_ie_ratio_cpy) & (255))) : ((((uint16_t)((((uint8_t)((c_ie_ratio_cpy) & (255))) == (0)) ? 1 : ((uint8_t)((c_ie_ratio_cpy) & (255))))) > (((uint16_t)((((uint8_t)((c_ie_ratio_cpy) >> (8))) == (0)) ? 1 : ((uint8_t)((c_ie_ratio_cpy) >> (8))))) * (4))) ? (((((uint8_t)((c_ie_ratio_cpy) & (255))) == (0)) ? 1 : ((uint8_t)((c_ie_ratio_cpy) & (255)))) / (4)) + (1) : ((((uint8_t)((c_ie_ratio_cpy) >> (8))) == (0)) ? 1 : ((uint8_t)((c_ie_ratio_cpy) >> (8))))))) << (8)) | (((uint16_t)((((uint8_t)((c_ie_ratio_cpy) & (255))) == (0)) ? 1 : ((uint8_t)((c_ie_ratio_cpy) & (255))))) & (255))) >> (8))) + ((uint8_t)(((((uint16_t)((((((uint8_t)((c_ie_ratio_cpy) >> (8))) == (0)) ? 1 : ((uint8_t)((c_ie_ratio_cpy) >> (8)))) > ((((uint8_t)((c_ie_ratio_cpy) & (255))) == (0)) ? 1 : ((uint8_t)((c_ie_ratio_cpy) & (255))))) ? (((uint8_t)((c_ie_ratio_cpy) & (255))) == (0)) ? 1 : ((uint8_t)((c_ie_ratio_cpy) & (255))) : ((((uint16_t)((((uint8_t)((c_ie_ratio_cpy) & (255))) == (0)) ? 1 : ((uint8_t)((c_ie_ratio_cpy) & (255))))) > (((uint16_t)((((uint8_t)((c_ie_ratio_cpy) >> (8))) == (0)) ? 1 : ((uint8_t)((c_ie_ratio_cpy) >> (8))))) * (4))) ? (((((uint8_t)((c_ie_ratio_cpy) & (255))) == (0)) ? 1 : ((uint8_t)((c_ie_ratio_cpy) & (255)))) / (4)) + (1) : ((((uint8_t)((c_ie_ratio_cpy) >> (8))) == (0)) ? 1 : ((uint8_t)((c_ie_ratio_cpy) >> (8))))))) << (8)) | (((uint16_t)((((uint8_t)((c_ie_ratio_cpy) & (255))) == (0)) ? 1 : ((uint8_t)((c_ie_ratio_cpy) & (255))))) & (255))) & (255)))))) == (0)) ? 1 : (((60000000) / ((uint32_t)(((c_bpm_cpy) <= (6)) ? 6 : (((c_bpm_cpy) >= (60)) ? 60 : (c_bpm_cpy))))) / ((uint32_t)(((uint8_t)(((((uint16_t)((((((uint8_t)((c_ie_ratio_cpy) >> (8))) == (0)) ? 1 : ((uint8_t)((c_ie_ratio_cpy) >> (8)))) > ((((uint8_t)((c_ie_ratio_cpy) & (255))) == (0)) ? 1 : ((uint8_t)((c_ie_ratio_cpy) & (255))))) ? (((uint8_t)((c_ie_ratio_cpy) & (255))) == (0)) ? 1 : ((uint8_t)((c_ie_ratio_cpy) & (255))) : ((((uint16_t)((((uint8_t)((c_ie_ratio_cpy) & (255))) == (0)) ? 1 : ((uint8_t)((c_ie_ratio_cpy) & (255))))) > (((uint16_t)((((uint8_t)((c_ie_ratio_cpy) >> (8))) == (0)) ? 1 : ((uint8_t)((c_ie_ratio_cpy) >> (8))))) * (4))) ? (((((uint8_t)((c_ie_ratio_cpy) & (255))) == (0)) ? 1 : ((uint8_t)((c_ie_ratio_cpy) & (255)))) / (4)) + (1) : ((((uint8_t)((c_ie_ratio_cpy) >> (8))) == (0)) ? 1 : ((uint8_t)((c_ie_ratio_cpy) >> (8))))))) << (8)) | (((uint16_t)((((uint8_t)((c_ie_ratio_cpy) & (255))) == (0)) ? 1 : ((uint8_t)((c_ie_ratio_cpy) & (255))))) & (255))) >> (8))) + ((uint8_t)(((((uint16_t)((((((uint8_t)((c_ie_ratio_cpy) >> (8))) == (0)) ? 1 : ((uint8_t)((c_ie_ratio_cpy) >> (8)))) > ((((uint8_t)((c_ie_ratio_cpy) & (255))) == (0)) ? 1 : ((uint8_t)((c_ie_ratio_cpy) & (255))))) ? (((uint8_t)((c_ie_ratio_cpy) & (255))) == (0)) ? 1 : ((uint8_t)((c_ie_ratio_cpy) & (255))) : ((((uint16_t)((((uint8_t)((c_ie_ratio_cpy) & (255))) == (0)) ? 1 : ((uint8_t)((c_ie_ratio_cpy) & (255))))) > (((uint16_t)((((uint8_t)((c_ie_ratio_cpy) >> (8))) == (0)) ? 1 : ((uint8_t)((c_ie_ratio_cpy) >> (8))))) * (4))) ? (((((uint8_t)((c_ie_ratio_cpy) & (255))) == (0)) ? 1 : ((uint8_t)((c_ie_ratio_cpy) & (255)))) / (4)) + (1) : ((((uint8_t)((c_ie_ratio_cpy) >> (8))) == (0)) ? 1 : ((uint8_t)((c_ie_ratio_cpy) >> (8))))))) << (8)) | (((uint16_t)((((uint8_t)((c_ie_ratio_cpy) & (255))) == (0)) ? 1 : ((uint8_t)((c_ie_ratio_cpy) & (255))))) & (255))) & (255))))))) * ((uint32_t)((uint8_t)(((((uint16_t)((((((uint8_t)((c_ie_ratio_cpy) >> (8))) == (0)) ? 1 : ((uint8_t)((c_ie_ratio_cpy) >> (8)))) > ((((uint8_t)((c_ie_ratio_cpy) & (255))) == (0)) ? 1 : ((uint8_t)((c_ie_ratio_cpy) & (255))))) ? (((uint8_t)((c_ie_ratio_cpy) & (255))) == (0)) ? 1 : ((uint8_t)((c_ie_ratio_cpy) & (255))) : ((((uint16_t)((((uint8_t)((c_ie_ratio_cpy) & (255))) == (0)) ? 1 : ((uint8_t)((c_ie_ratio_cpy) & (255))))) > (((uint16_t)((((uint8_t)((c_ie_ratio_cpy) >> (8))) == (0)) ? 1 : ((uint8_t)((c_ie_ratio_cpy) >> (8))))) * (4))) ? (((((uint8_t)((c_ie_ratio_cpy) & (255))) == (0)) ? 1 : ((uint8_t)((c_ie_ratio_cpy) & (255)))) / (4)) + (1) : ((((uint8_t)((c_ie_ratio_cpy) >> (8))) == (0)) ? 1 : ((uint8_t)((c_ie_ratio_cpy) >> (8))))))) << (8)) | (((uint16_t)((((uint8_t)((c_ie_ratio_cpy) & (255))) == (0)) ? 1 : ((uint8_t)((c_ie_ratio_cpy) & (255))))) & (255))) >> (8)))) : (((60000000) / ((uint32_t)(((c_bpm_cpy) <= (6)) ? 6 : (((c_bpm_cpy) >= (60)) ? 60 : (c_bpm_cpy))))) - ((((((60000000) / ((uint32_t)(((c_bpm_cpy) <= (6)) ? 6 : (((c_bpm_cpy) >= (60)) ? 60 : (c_bpm_cpy))))) / ((uint32_t)(((uint8_t)(((((uint16_t)((((((uint8_t)((c_ie_ratio_cpy) >> (8))) == (0)) ? 1 : ((uint8_t)((c_ie_ratio_cpy) >> (8)))) > ((((uint8_t)((c_ie_ratio_cpy) & (255))) == (0)) ? 1 : ((uint8_t)((c_ie_ratio_cpy) & (255))))) ? (((uint8_t)((c_ie_ratio_cpy) & (255))) == (0)) ? 1 : ((uint8_t)((c_ie_ratio_cpy) & (255))) : ((((uint16_t)((((uint8_t)((c_ie_ratio_cpy) & (255))) == (0)) ? 1 : ((uint8_t)((c_ie_ratio_cpy) & (255))))) > (((uint16_t)((((uint8_t)((c_ie_ratio_cpy) >> (8))) == (0)) ? 1 : ((uint8_t)((c_ie_ratio_cpy) >> (8))))) * (4))) ? (((((uint8_t)((c_ie_ratio_cpy) & (255))) == (0)) ? 1 : ((uint8_t)((c_ie_ratio_cpy) & (255)))) / (4)) + (1) : ((((uint8_t)((c_ie_ratio_cpy) >> (8))) == (0)) ? 1 : ((uint8_t)((c_ie_ratio_cpy) >> (8))))))) << (8)) | (((uint16_t)((((uint8_t)((c_ie_ratio_cpy) & (255))) == (0)) ? 1 : ((uint8_t)((c_ie_ratio_cpy) & (255))))) & (255))) >> (8))) + ((uint8_t)(((((uint16_t)((((((uint8_t)((c_ie_ratio_cpy) >> (8))) == (0)) ? 1 : ((uint8_t)((c_ie_ratio_cpy) >> (8)))) > ((((uint8_t)((c_ie_ratio_cpy) & (255))) == (0)) ? 1 : ((uint8_t)((c_ie_ratio_cpy) & (255))))) ? (((uint8_t)((c_ie_ratio_cpy) & (255))) == (0)) ? 1 : ((uint8_t)((c_ie_ratio_cpy) & (255))) : ((((uint16_t)((((uint8_t)((c_ie_ratio_cpy) & (255))) == (0)) ? 1 : ((uint8_t)((c_ie_ratio_cpy) & (255))))) > (((uint16_t)((((uint8_t)((c_ie_ratio_cpy) >> (8))) == (0)) ? 1 : ((uint8_t)((c_ie_ratio_cpy) >> (8))))) * (4))) ? (((((uint8_t)((c_ie_ratio_cpy) & (255))) == (0)) ? 1 : ((uint8_t)((c_ie_ratio_cpy) & (255)))) / (4)) + (1) : ((((uint8_t)((c_ie_ratio_cpy) >> (8))) == (0)) ? 1 : ((uint8_t)((c_ie_ratio_cpy) >> (8))))))) << (8)) | (((uint16_t)((((uint8_t)((c_ie_ratio_cpy) & (255))) == (0)) ? 1 : ((uint8_t)((c_ie_ratio_cpy) & (255))))) & (255))) & (255)))))) == (0)) ? 1 : (((60000000) / ((uint32_t)(((c_bpm_cpy) <= (6)) ? 6 : (((c_bpm_cpy) >= (60)) ? 60 : (c_bpm_cpy))))) / ((uint32_t)(((uint8_t)(((((uint16_t)((((((uint8_t)((c_ie_ratio_cpy) >> (8))) == (0)) ? 1 : ((uint8_t)((c_ie_ratio_cpy) >> (8)))) > ((((uint8_t)((c_ie_ratio_cpy) & (255))) == (0)) ? 1 : ((uint8_t)((c_ie_ratio_cpy) & (255))))) ? (((uint8_t)((c_ie_ratio_cpy) & (255))) == (0)) ? 1 : ((uint8_t)((c_ie_ratio_cpy) & (255))) : ((((uint16_t)((((uint8_t)((c_ie_ratio_cpy) & (255))) == (0)) ? 1 : ((uint8_t)((c_ie_ratio_cpy) & (255))))) > (((uint16_t)((((uint8_t)((c_ie_ratio_cpy) >> (8))) == (0)) ? 1 : ((uint8_t)((c_ie_ratio_cpy) >> (8))))) * (4))) ? (((((uint8_t)((c_ie_ratio_cpy) & (255))) == (0)) ? 1 : ((uint8_t)((c_ie_ratio_cpy) & (255)))) / (4)) + (1) : ((((uint8_t)((c_ie_ratio_cpy) >> (8))) == (0)) ? 1 : ((uint8_t)((c_ie_ratio_cpy) >> (8))))))) << (8)) | (((uint16_t)((((uint8_t)((c_ie_ratio_cpy) & (255))) == (0)) ? 1 : ((uint8_t)((c_ie_ratio_cpy) & (255))))) & (255))) >> (8))) + ((uint8_t)(((((uint16_t)((((((uint8_t)((c_ie_ratio_cpy) >> (8))) == (0)) ? 1 : ((uint8_t)((c_ie_ratio_cpy) >> (8)))) > ((((uint8_t)((c_ie_ratio_cpy) & (255))) == (0)) ? 1 : ((uint8_t)((c_ie_ratio_cpy) & (255))))) ? (((uint8_t)((c_ie_ratio_cpy) & (255))) == (0)) ? 1 : ((uint8_t)((c_ie_ratio_cpy) & (255))) : ((((uint16_t)((((uint8_t)((c_ie_ratio_cpy) & (255))) == (0)) ? 1 : ((uint8_t)((c_ie_ratio_cpy) & (255))))) > (((uint16_t)((((uint8_t)((c_ie_ratio_cpy) >> (8))) == (0)) ? 1 : ((uint8_t)((c_ie_ratio_cpy) >> (8))))) * (4))) ? (((((uint8_t)((c_ie_ratio_cpy) & (255))) == (0)) ? 1 : ((uint8_t)((c_ie_ratio_cpy) & (255)))) / (4)) + (1) : ((((uint8_t)((c_ie_ratio_cpy) >> (8))) == (0)) ? 1 : ((uint8_t)((c_ie_ratio_cpy) >> (8))))))) << (8)) | (((uint16_t)((((uint8_t)((c_ie_ratio_cpy) & (255))) == (0)) ? 1 : ((uint8_t)((c_ie_ratio_cpy) & (255))))) & (255))) & (255))))))) * ((uint32_t)((uint8_t)(((((uint16_t)((((((uint8_t)((c_ie_ratio_cpy) >> (8))) == (0)) ? 1 : ((uint8_t)((c_ie_ratio_cpy) >> (8)))) > ((((uint8_t)((c_ie_ratio_cpy) & (255))) == (0)) ? 1 : ((uint8_t)((c_ie_ratio_cpy) & (255))))) ? (((uint8_t)((c_ie_ratio_cpy) & (255))) == (0)) ? 1 : ((uint8_t)((c_ie_ratio_cpy) & (255))) : ((((uint16_t)((((uint8_t)((c_ie_ratio_cpy) & (255))) == (0)) ? 1 : ((uint8_t)((c_ie_ratio_cpy) & (255))))) > (((uint16_t)((((uint8_t)((c_ie_ratio_cpy) >> (8))) == (0)) ? 1 : ((uint8_t)((c_ie_ratio_cpy) >> (8))))) * (4))) ? (((((uint8_t)((c_ie_ratio_cpy) & (255))) == (0)) ? 1 : ((uint8_t)((c_ie_ratio_cpy) & (255)))) / (4)) + (1) : ((((uint8_t)((c_ie_ratio_cpy) >> (8))) == (0)) ? 1 : ((uint8_t)((c_ie_ratio_cpy) >> (8))))))) << (8)) | (((uint16_t)((((uint8_t)((c_ie_ratio_cpy) & (255))) == (0)) ? 1 : ((uint8_t)((c_ie_ratio_cpy) & (255))))) & (255))) >> (8)))))) : ((s3)[s3_idx]);
}

int8_t s6_gen(void) {
  return (((s_flow_cpy) >= (1500)) ? 0 : ((((s_flow_cpy) < ((((s3)[s3_idx]) < (1000)) ? 0 : (((s5)[s5_idx]) ? (c_cmv_mode_cpy) ? ((s_volume_cpy) >= ((int32_t)(((c_cmv_volume_goal_cpy) <= (100000)) ? 100000 : (((c_cmv_volume_goal_cpy) >= (((c_volume_limit_cpy) <= (100000)) ? 100000 : (((c_volume_limit_cpy) >= (1000000)) ? 1000000 : (c_volume_limit_cpy)))) ? ((c_volume_limit_cpy) <= (100000)) ? 100000 : (((c_volume_limit_cpy) >= (1000000)) ? 1000000 : (c_volume_limit_cpy)) : (c_cmv_volume_goal_cpy))))) ? 0 : ((((int32_t)(((c_cmv_volume_goal_cpy) <= (100000)) ? 100000 : (((c_cmv_volume_goal_cpy) >= (((c_volume_limit_cpy) <= (100000)) ? 100000 : (((c_volume_limit_cpy) >= (1000000)) ? 1000000 : (c_volume_limit_cpy)))) ? ((c_volume_limit_cpy) <= (100000)) ? 100000 : (((c_volume_limit_cpy) >= (1000000)) ? 1000000 : (c_volume_limit_cpy)) : (c_cmv_volume_goal_cpy)))) / ((int32_t)(((s3)[s3_idx]) / (1000)))) + (1)) : ((((s_internal_pressure_1_cpy) >= ((int32_t)(((c_cmv_pressure_goal_cpy) <= (100)) ? 100 : (((c_cmv_pressure_goal_cpy) >= (((c_pressure_limit_cpy) <= (100)) ? 100 : (((c_pressure_limit_cpy) >= (5000)) ? 5000 : (c_pressure_limit_cpy)))) ? ((c_pressure_limit_cpy) <= (100)) ? 100 : (((c_pressure_limit_cpy) >= (5000)) ? 5000 : (c_pressure_limit_cpy)) : (c_cmv_pressure_goal_cpy))))) || ((s_volume_cpy) >= ((int32_t)(((c_volume_limit_cpy) <= (100000)) ? 100000 : (((c_volume_limit_cpy) >= (1000000)) ? 1000000 : (c_volume_limit_cpy)))))) ? 0 : (300)) : (((s_volume_cpy) <= (0)) ? 0 : ((0) - ((((int32_t)(((c_cmv_volume_goal_cpy) <= (100000)) ? 100000 : (((c_cmv_volume_goal_cpy) >= (((c_volume_limit_cpy) <= (100000)) ? 100000 : (((c_volume_limit_cpy) >= (1000000)) ? 1000000 : (c_volume_limit_cpy)))) ? ((c_volume_limit_cpy) <= (100000)) ? 100000 : (((c_volume_limit_cpy) >= (1000000)) ? 1000000 : (c_volume_limit_cpy)) : (c_cmv_volume_goal_cpy)))) / ((int32_t)(((s3)[s3_idx]) / (1000)))) + (1))))))) && (((s6)[s6_idx]) < (127))) ? 1 : ((((s_flow_cpy) > ((((s3)[s3_idx]) < (1000)) ? 0 : (((s5)[s5_idx]) ? (c_cmv_mode_cpy) ? ((s_volume_cpy) >= ((int32_t)(((c_cmv_volume_goal_cpy) <= (100000)) ? 100000 : (((c_cmv_volume_goal_cpy) >= (((c_volume_limit_cpy) <= (100000)) ? 100000 : (((c_volume_limit_cpy) >= (1000000)) ? 1000000 : (c_volume_limit_cpy)))) ? ((c_volume_limit_cpy) <= (100000)) ? 100000 : (((c_volume_limit_cpy) >= (1000000)) ? 1000000 : (c_volume_limit_cpy)) : (c_cmv_volume_goal_cpy))))) ? 0 : ((((int32_t)(((c_cmv_volume_goal_cpy) <= (100000)) ? 100000 : (((c_cmv_volume_goal_cpy) >= (((c_volume_limit_cpy) <= (100000)) ? 100000 : (((c_volume_limit_cpy) >= (1000000)) ? 1000000 : (c_volume_limit_cpy)))) ? ((c_volume_limit_cpy) <= (100000)) ? 100000 : (((c_volume_limit_cpy) >= (1000000)) ? 1000000 : (c_volume_limit_cpy)) : (c_cmv_volume_goal_cpy)))) / ((int32_t)(((s3)[s3_idx]) / (1000)))) + (1)) : ((((s_internal_pressure_1_cpy) >= ((int32_t)(((c_cmv_pressure_goal_cpy) <= (100)) ? 100 : (((c_cmv_pressure_goal_cpy) >= (((c_pressure_limit_cpy) <= (100)) ? 100 : (((c_pressure_limit_cpy) >= (5000)) ? 5000 : (c_pressure_limit_cpy)))) ? ((c_pressure_limit_cpy) <= (100)) ? 100 : (((c_pressure_limit_cpy) >= (5000)) ? 5000 : (c_pressure_limit_cpy)) : (c_cmv_pressure_goal_cpy))))) || ((s_volume_cpy) >= ((int32_t)(((c_volume_limit_cpy) <= (100000)) ? 100000 : (((c_volume_limit_cpy) >= (1000000)) ? 1000000 : (c_volume_limit_cpy)))))) ? 0 : (300)) : (((s_volume_cpy) <= (0)) ? 0 : ((0) - ((((int32_t)(((c_cmv_volume_goal_cpy) <= (100000)) ? 100000 : (((c_cmv_volume_goal_cpy) >= (((c_volume_limit_cpy) <= (100000)) ? 100000 : (((c_volume_limit_cpy) >= (1000000)) ? 1000000 : (c_volume_limit_cpy)))) ? ((c_volume_limit_cpy) <= (100000)) ? 100000 : (((c_volume_limit_cpy) >= (1000000)) ? 1000000 : (c_volume_limit_cpy)) : (c_cmv_volume_goal_cpy)))) / ((int32_t)(((s3)[s3_idx]) / (1000)))) + (1))))))) && (((s6)[s6_idx]) > (-127))) ? -1 : (0)))) + ((s6)[s6_idx]);
}

uint32_t s8_gen(void) {
  return ((s7)[s7_idx]) ? 0 : (((s8)[s8_idx]) + ((s2)[s2_idx]));
}

bool s7_gen(void) {
  return (((s8)[s8_idx]) >= (0)) ? true : (false);
}

bool raise_alarm_guard(void) {
  return false;
}

bool update_ui_guard(void) {
  return (s0)[s0_idx];
}

int32_t update_ui_arg_desired_flow(void) {
  return (((s3)[s3_idx]) < (1000)) ? 0 : (((s5)[s5_idx]) ? (c_cmv_mode_cpy) ? ((s_volume_cpy) >= ((int32_t)(((c_cmv_volume_goal_cpy) <= (100000)) ? 100000 : (((c_cmv_volume_goal_cpy) >= (((c_volume_limit_cpy) <= (100000)) ? 100000 : (((c_volume_limit_cpy) >= (1000000)) ? 1000000 : (c_volume_limit_cpy)))) ? ((c_volume_limit_cpy) <= (100000)) ? 100000 : (((c_volume_limit_cpy) >= (1000000)) ? 1000000 : (c_volume_limit_cpy)) : (c_cmv_volume_goal_cpy))))) ? 0 : ((((int32_t)(((c_cmv_volume_goal_cpy) <= (100000)) ? 100000 : (((c_cmv_volume_goal_cpy) >= (((c_volume_limit_cpy) <= (100000)) ? 100000 : (((c_volume_limit_cpy) >= (1000000)) ? 1000000 : (c_volume_limit_cpy)))) ? ((c_volume_limit_cpy) <= (100000)) ? 100000 : (((c_volume_limit_cpy) >= (1000000)) ? 1000000 : (c_volume_limit_cpy)) : (c_cmv_volume_goal_cpy)))) / ((int32_t)(((s3)[s3_idx]) / (1000)))) + (1)) : ((((s_internal_pressure_1_cpy) >= ((int32_t)(((c_cmv_pressure_goal_cpy) <= (100)) ? 100 : (((c_cmv_pressure_goal_cpy) >= (((c_pressure_limit_cpy) <= (100)) ? 100 : (((c_pressure_limit_cpy) >= (5000)) ? 5000 : (c_pressure_limit_cpy)))) ? ((c_pressure_limit_cpy) <= (100)) ? 100 : (((c_pressure_limit_cpy) >= (5000)) ? 5000 : (c_pressure_limit_cpy)) : (c_cmv_pressure_goal_cpy))))) || ((s_volume_cpy) >= ((int32_t)(((c_volume_limit_cpy) <= (100000)) ? 100000 : (((c_volume_limit_cpy) >= (1000000)) ? 1000000 : (c_volume_limit_cpy)))))) ? 0 : (300)) : (((s_volume_cpy) <= (0)) ? 0 : ((0) - ((((int32_t)(((c_cmv_volume_goal_cpy) <= (100000)) ? 100000 : (((c_cmv_volume_goal_cpy) >= (((c_volume_limit_cpy) <= (100000)) ? 100000 : (((c_volume_limit_cpy) >= (1000000)) ? 1000000 : (c_volume_limit_cpy)))) ? ((c_volume_limit_cpy) <= (100000)) ? 100000 : (((c_volume_limit_cpy) >= (1000000)) ? 1000000 : (c_volume_limit_cpy)) : (c_cmv_volume_goal_cpy)))) / ((int32_t)(((s3)[s3_idx]) / (1000)))) + (1)))));
}

int8_t update_ui_arg_motor_velocity(void) {
  return ((((s6)[s6_idx]) > (0)) && (s_piston_high_cpy)) ? 0 : (((((s6)[s6_idx]) < (0)) && (s_piston_low_cpy)) ? 0 : ((s6)[s6_idx]));
}

bool update_ui_arg_s_piston_high(void) {
  return s_piston_high_cpy;
}

bool update_ui_arg_s_piston_low(void) {
  return s_piston_low_cpy;
}

uint8_t update_ui_arg_bpm_limited(void) {
  return ((c_bpm_cpy) <= (6)) ? 6 : (((c_bpm_cpy) >= (60)) ? 60 : (c_bpm_cpy));
}

uint32_t update_ui_arg_volume_limit(void) {
  return ((c_volume_limit_cpy) <= (100000)) ? 100000 : (((c_volume_limit_cpy) >= (1000000)) ? 1000000 : (c_volume_limit_cpy));
}

uint32_t update_ui_arg_pressure_limit(void) {
  return ((c_pressure_limit_cpy) <= (100)) ? 100 : (((c_pressure_limit_cpy) >= (5000)) ? 5000 : (c_pressure_limit_cpy));
}

uint8_t update_ui_arg_ie_inhale(void) {
  return (uint8_t)(((((uint16_t)((((((uint8_t)((c_ie_ratio_cpy) >> (8))) == (0)) ? 1 : ((uint8_t)((c_ie_ratio_cpy) >> (8)))) > ((((uint8_t)((c_ie_ratio_cpy) & (255))) == (0)) ? 1 : ((uint8_t)((c_ie_ratio_cpy) & (255))))) ? (((uint8_t)((c_ie_ratio_cpy) & (255))) == (0)) ? 1 : ((uint8_t)((c_ie_ratio_cpy) & (255))) : ((((uint16_t)((((uint8_t)((c_ie_ratio_cpy) & (255))) == (0)) ? 1 : ((uint8_t)((c_ie_ratio_cpy) & (255))))) > (((uint16_t)((((uint8_t)((c_ie_ratio_cpy) >> (8))) == (0)) ? 1 : ((uint8_t)((c_ie_ratio_cpy) >> (8))))) * (4))) ? (((((uint8_t)((c_ie_ratio_cpy) & (255))) == (0)) ? 1 : ((uint8_t)((c_ie_ratio_cpy) & (255)))) / (4)) + (1) : ((((uint8_t)((c_ie_ratio_cpy) >> (8))) == (0)) ? 1 : ((uint8_t)((c_ie_ratio_cpy) >> (8))))))) << (8)) | (((uint16_t)((((uint8_t)((c_ie_ratio_cpy) & (255))) == (0)) ? 1 : ((uint8_t)((c_ie_ratio_cpy) & (255))))) & (255))) >> (8));
}

uint8_t update_ui_arg_ie_exhale(void) {
  return (uint8_t)(((((uint16_t)((((((uint8_t)((c_ie_ratio_cpy) >> (8))) == (0)) ? 1 : ((uint8_t)((c_ie_ratio_cpy) >> (8)))) > ((((uint8_t)((c_ie_ratio_cpy) & (255))) == (0)) ? 1 : ((uint8_t)((c_ie_ratio_cpy) & (255))))) ? (((uint8_t)((c_ie_ratio_cpy) & (255))) == (0)) ? 1 : ((uint8_t)((c_ie_ratio_cpy) & (255))) : ((((uint16_t)((((uint8_t)((c_ie_ratio_cpy) & (255))) == (0)) ? 1 : ((uint8_t)((c_ie_ratio_cpy) & (255))))) > (((uint16_t)((((uint8_t)((c_ie_ratio_cpy) >> (8))) == (0)) ? 1 : ((uint8_t)((c_ie_ratio_cpy) >> (8))))) * (4))) ? (((((uint8_t)((c_ie_ratio_cpy) & (255))) == (0)) ? 1 : ((uint8_t)((c_ie_ratio_cpy) & (255)))) / (4)) + (1) : ((((uint8_t)((c_ie_ratio_cpy) >> (8))) == (0)) ? 1 : ((uint8_t)((c_ie_ratio_cpy) >> (8))))))) << (8)) | (((uint16_t)((((uint8_t)((c_ie_ratio_cpy) & (255))) == (0)) ? 1 : ((uint8_t)((c_ie_ratio_cpy) & (255))))) & (255))) & (255));
}

bool update_ui_arg_cmv_mode(void) {
  return c_cmv_mode_cpy;
}

uint32_t update_ui_arg_cmv_volume_goal(void) {
  return ((c_cmv_volume_goal_cpy) <= (100000)) ? 100000 : (((c_cmv_volume_goal_cpy) >= (((c_volume_limit_cpy) <= (100000)) ? 100000 : (((c_volume_limit_cpy) >= (1000000)) ? 1000000 : (c_volume_limit_cpy)))) ? ((c_volume_limit_cpy) <= (100000)) ? 100000 : (((c_volume_limit_cpy) >= (1000000)) ? 1000000 : (c_volume_limit_cpy)) : (c_cmv_volume_goal_cpy));
}

uint32_t update_ui_arg_cmv_pressure_goal(void) {
  return ((c_cmv_pressure_goal_cpy) <= (100)) ? 100 : (((c_cmv_pressure_goal_cpy) >= (((c_pressure_limit_cpy) <= (100)) ? 100 : (((c_pressure_limit_cpy) >= (5000)) ? 5000 : (c_pressure_limit_cpy)))) ? ((c_pressure_limit_cpy) <= (100)) ? 100 : (((c_pressure_limit_cpy) >= (5000)) ? 5000 : (c_pressure_limit_cpy)) : (c_cmv_pressure_goal_cpy));
}

uint32_t update_ui_arg_global_volume_max(void) {
  return 1000000;
}

uint32_t update_ui_arg_global_volume_min(void) {
  return 100000;
}

uint32_t update_ui_arg_global_pressure_max(void) {
  return 5000;
}

uint32_t update_ui_arg_global_pressure_min(void) {
  return 100;
}

bool control_motor_guard(void) {
  return (s7)[s7_idx];
}

int8_t control_motor_arg_motor_velocity(void) {
  return ((((s6)[s6_idx]) > (0)) && (s_piston_high_cpy)) ? 0 : (((((s6)[s6_idx]) < (0)) && (s_piston_low_cpy)) ? 0 : ((s6)[s6_idx]));
}

void step(void) {
  (t_delta_us_cpy) = (t_delta_us);
  (c_bpm_cpy) = (c_bpm);
  (c_ie_ratio_cpy) = (c_ie_ratio);
  (s_flow_cpy) = (s_flow);
  (c_cmv_mode_cpy) = (c_cmv_mode);
  (s_volume_cpy) = (s_volume);
  (c_cmv_volume_goal_cpy) = (c_cmv_volume_goal);
  (c_volume_limit_cpy) = (c_volume_limit);
  (s_internal_pressure_1_cpy) = (s_internal_pressure_1);
  (c_cmv_pressure_goal_cpy) = (c_cmv_pressure_goal);
  (c_pressure_limit_cpy) = (c_pressure_limit);
  (s_piston_high_cpy) = (s_piston_high);
  (s_piston_low_cpy) = (s_piston_low);
  if ((raise_alarm_guard)()) {
    (raise_alarm)();
  };
  if ((update_ui_guard)()) {
    (update_ui)(((update_ui_arg_desired_flow)()), ((update_ui_arg_motor_velocity)()), ((update_ui_arg_s_piston_high)()), ((update_ui_arg_s_piston_low)()), ((update_ui_arg_bpm_limited)()), ((update_ui_arg_volume_limit)()), ((update_ui_arg_pressure_limit)()), ((update_ui_arg_ie_inhale)()), ((update_ui_arg_ie_exhale)()), ((update_ui_arg_cmv_mode)()), ((update_ui_arg_cmv_volume_goal)()), ((update_ui_arg_cmv_pressure_goal)()), ((update_ui_arg_global_volume_max)()), ((update_ui_arg_global_volume_min)()), ((update_ui_arg_global_pressure_max)()), ((update_ui_arg_global_pressure_min)()));
  };
  if ((control_motor_guard)()) {
    (control_motor)(((control_motor_arg_motor_velocity)()));
  };
  ((s2)[((s2_idx) + (1)) % (2)]) = ((s2_gen)());
  ((s1)[((s1_idx) + (1)) % (2)]) = ((s1_gen)());
  ((s0)[((s0_idx) + (1)) % (2)]) = ((s0_gen)());
  ((s4)[((s4_idx) + (1)) % (2)]) = ((s4_gen)());
  ((s5)[((s5_idx) + (1)) % (2)]) = ((s5_gen)());
  ((s3)[((s3_idx) + (1)) % (2)]) = ((s3_gen)());
  ((s6)[((s6_idx) + (1)) % (2)]) = ((s6_gen)());
  ((s8)[((s8_idx) + (1)) % (2)]) = ((s8_gen)());
  ((s7)[((s7_idx) + (1)) % (2)]) = ((s7_gen)());
  (s2_idx) = ((++(s2_idx)) % (2));
  (s1_idx) = ((++(s1_idx)) % (2));
  (s0_idx) = ((++(s0_idx)) % (2));
  (s4_idx) = ((++(s4_idx)) % (2));
  (s5_idx) = ((++(s5_idx)) % (2));
  (s3_idx) = ((++(s3_idx)) % (2));
  (s6_idx) = ((++(s6_idx)) % (2));
  (s8_idx) = ((++(s8_idx)) % (2));
  (s7_idx) = ((++(s7_idx)) % (2));
}
