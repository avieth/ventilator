/*
   20200328
   author: Colin Gallacher
   v0.1

*/

#include "pins.h"
#include "pressure_sensors.h"

#define PASCAL_PER_CM_H2O 98.0665

#define SENSOR_COUNT 4
/**
 * Indices in the offset and pin arrays.
 */
#define INSPPRESSURE 0
#define INSPFLOW 1
#define EXPFLOW 2
#define AIRINFLOW 3

uint16_t sensor_offsets[SENSOR_COUNT] = { 0, 0, 0, 0 };
uint8_t sensor_pins[SENSOR_COUNT] = { 
    PIN_INSP_PRESSURE,
    PIN_INSP_FLOW,
    PIN_EXP_FLOW,
    PIN_AIR_IN_FLOW
  };

/**
 * Only call with one of the defined sensor names above.
 * You get 0 otherwise.
 */
int16_t read_sensor_with_offset(uint8_t sensor_id) {
  if (sensor_id >= SENSOR_COUNT) {
    return 0;
  } else {
    return ((int16_t) read_sensor(sensor_pins[sensor_id])) - ((int16_t) sensor_offsets[sensor_id]);
  }
}

/**
 * This is 0.6x + 0.3y + 0.2z but without nasty fractional computation.
 */
int32_t smooth(int32_t x, int32_t y, int32_t z) {
  return (6*x + 3*y + 2*z) / 10;
}

//INITIALIZATION 

bool initializeSensors(){
  sensor_offsets[INSPPRESSURE] = sensor_offset(sensor_pins[INSPPRESSURE]);
  sensor_offsets[INSPFLOW]     = sensor_offset(sensor_pins[INSPFLOW]);
  sensor_offsets[EXPFLOW]      = sensor_offset(sensor_pins[EXPFLOW]);
  sensor_offsets[AIRINFLOW]    = sensor_offset(sensor_pins[AIRINFLOW]);
}

//SENSORS

/**
 * Inspiration pressure at this instant.
 * TODO make it integral.
 */
float get_insp_pressure() {
  int32_t rawData = read_sensor_with_offset(INSPPRESSURE);
  return pressure_difference(rawData);
}

/**
 * Inspiration flow at this instant.
 * TODO make it integral.
 */
float get_insp_flow() {
  int32_t rawData = read_sensor_with_offset(INSPFLOW);
  return flow_rate(rawData); 
}

/**
 * Expiration flow at this instant.
 * TODO make it integral.
 */
float get_exp_flow() {
  int32_t rawData = read_sensor_with_offset(EXPFLOW);
  return flow_rate(rawData); 
}

/**
 * Air in flow at this instant.
 * TODO make it integral
 */
float get_air_in_flow() {
  int32_t rawData = read_sensor_with_offset(AIRINFLOW);
  return flow_rate(rawData); 
}


//COMPUTED VOLUMES

/*bool isInspFlow = false;
float inspSensedVolume = 0.0f; 


float getInspSensedVolume(long sensorLoopTime){

  float f_sensorLoopTime = sensorLoopTime/1000000.0f; 

  if(isInspFlow){
    inspSensedVolume = inspSensedVolume + getInspFlow()*f_sensorLoopTime;
    
  }else{
    inspSensedVolume = 0.0f; 
  }

  
  return inspSensedVolume; 
}*/

/*bool isExpFlow = false;
float expSensedVolume = 0.0f; 

float getExpSensedVolume(long sensorLoopTime){
  
  float f_sensorLoopTime = sensorLoopTime/1000000.0f; 

  if(isExpFlow){
    expSensedVolume = expSensedVolume + getExpFlow()*f_sensorLoopTime;
    
  }else{
    expSensedVolume = 0.0f; 
  }

  
  return expSensedVolume; 
}*/


/*bool isAirInFlow = false; 
float airInSensedVolume = 0.0f; 

float getAirInSensedVolume(long sensorLoopTime){
  
  float f_sensorLoopTime = sensorLoopTime/1000000.0f; 

  if(isAirInFlow){
    airInSensedVolume = airInSensedVolume + getAirInFlow()*f_sensorLoopTime;
    
  }else{
    airInSensedVolume = 0.0f; 
  }

  
  return airInSensedVolume; 
}*/


//CONVERSIONS


float volumeToCubicMeters(float volume) {
  float convertedVolume = volume / 1000000.0f;
  return convertedVolume;
}

float pressureToPascals(float pressure) {
  float convertedPressure = pressure * 1.0f;
  return convertedPressure;
}
