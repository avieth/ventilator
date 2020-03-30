/*
   20200328
   author: Colin Gallacher
   v0.1

*/

#include "pins.h"
#include "Pressure_Sensors.h"

#define PASCAL_PER_CM_H2O 98.0665

#define SENSOR_COUNT 5
/**
 * Indices in the offset and pin arrays.
 */
#define INSPPRESSURE 0
#define EXPPRESSURE 1
#define INSPFLOW 2
#define EXPFLOW 3
#define AIRINFLOW 4

int sensor_offsets[SENSOR_COUNT] = { 0, 0, 0, 0, 0 };
int sensor_pins[SENSOR_COUNT] = { 
    PIN_INSP_PRESSURE,
    PIN_EXP_PRESSURE,
    PIN_INSP_FLOW,
    PIN_EXP_FLOW,
    PIN_AIR_IN_FLOW
  };

/**
 * Only call with one of the defined sensor names above.
 * You get 0 otherwise.
 */
int read_sensor_with_offset(unsigned char sensor_id) {
  if (sensor_id >= SENSOR_COUNT) {
    return 0;
  } else {
    return read_sensor(sensor_pins[sensor_id]) - sensor_offsets[sensor_id];
  }
}

/**
 * This is 0.6x + 0.3y + 0.2z but without nasty fractional computation.
 * We do 6x + 3y + 2*z and then divide by 10.
 */
long smooth(long x, long y, long z) {
  return (6*x + 3*y + 2*z) / 10;
}

//INITIALIZATION 

bool initializeSensors(){
  sensor_offsets[INSPPRESSURE] = sensor_offset(sensor_pins[INSPPRESSURE]);
  sensor_offsets[EXPPRESSURE]  = sensor_offset(sensor_pins[EXPPRESSURE]);
  sensor_offsets[INSPFLOW]     = sensor_offset(sensor_pins[INSPFLOW]);
  sensor_offsets[EXPFLOW]      = sensor_offset(sensor_pins[EXPFLOW]);
  sensor_offsets[AIRINFLOW]    = sensor_offset(sensor_pins[AIRINFLOW]);
}

//SENSORS
int lastInspPressureReading = 0; 
int lastLastInspPressureReading = 0; 

float getInspPressure() {

  int rawData = read_sensor_with_offset(INSPPRESSURE);
  float pressure = pressure_difference(rawData); 

  float inspPressure= 0.6f*pressure + 0.3f*lastInspPressureReading + 0.2f*lastLastInspPressureReading;   
  
  lastInspPressureReading = pressure; 
  lastLastInspPressureReading = lastInspPressureReading; 

  return inspPressure;
}

int lastExpPressureReading = 0; 
int lastLastExpPressureReading = 0; 

float getExpPressure() {

  int rawData = read_sensor_with_offset(EXPPRESSURE);
  float pressure = pressure_difference(rawData); 

  float expPressure= 0.6f*pressure + 0.3f*lastExpPressureReading + 0.2f*lastLastExpPressureReading;   
  
  lastExpPressureReading = pressure; 
  lastLastExpPressureReading = lastExpPressureReading; 

  return expPressure;
}

int lastInspFlowReading = 0; 
int lastLastInspFlowReading = 0; 

float getInspFlow() {

  int rawData = read_sensor_with_offset(INSPFLOW);
  float flow = flow_rate(rawData); 

  float inspFlow= 0.6f*flow + 0.3f*lastInspFlowReading + 0.2f*lastLastInspFlowReading;   
  
  lastInspFlowReading = flow; 
  lastLastInspFlowReading = lastInspFlowReading; 

  return inspFlow;
}

int lastExpFlowReading = 0; 
int lastLastExpFlowReading = 0; 

float getExpFlow() {

  int rawData = read_sensor_with_offset(EXPFLOW);
  float flow = flow_rate(rawData); 

  float expFlow= 0.6f*flow + 0.3f*lastExpFlowReading + 0.2f*lastLastExpFlowReading;   
  
  lastExpFlowReading = flow; 
  lastLastExpFlowReading = lastExpFlowReading; 

  return expFlow;
}


int lastAirInFlowReading = 0; 
int lastLastAirInFlowReading = 0; 

float getAirInFlow() {

  int rawData = read_sensor_with_offset(AIRINFLOW);
  float flow = flow_rate(rawData); 

  float airInFlow= 0.6f*flow + 0.3f*lastAirInFlowReading + 0.2f*lastLastAirInFlowReading;   
  
  lastAirInFlowReading = flow; 
  lastLastAirInFlowReading = lastAirInFlowReading; 

  return airInFlow;
}


//COMPUTED VOLUMES

bool isInspFlow = false;
float inspSensedVolume = 0.0f; 


float getInspSensedVolume(long sensorLoopTime){

  float f_sensorLoopTime = sensorLoopTime/1000000.0f; 

  if(isInspFlow){
    inspSensedVolume = inspSensedVolume + getInspFlow()*f_sensorLoopTime;
    
  }else{
    inspSensedVolume = 0.0f; 
  }

  
  return inspSensedVolume; 
}

bool isExpFlow = false;
float expSensedVolume = 0.0f; 

float getExpSensedVolume(long sensorLoopTime){
  
  float f_sensorLoopTime = sensorLoopTime/1000000.0f; 

  if(isExpFlow){
    expSensedVolume = expSensedVolume + getExpFlow()*f_sensorLoopTime;
    
  }else{
    expSensedVolume = 0.0f; 
  }

  
  return expSensedVolume; 
}


bool isAirInFlow = false; 
float airInSensedVolume = 0.0f; 

float getAirInSensedVolume(long sensorLoopTime){
  
  float f_sensorLoopTime = sensorLoopTime/1000000.0f; 

  if(isAirInFlow){
    airInSensedVolume = airInSensedVolume + getAirInFlow()*f_sensorLoopTime;
    
  }else{
    airInSensedVolume = 0.0f; 
  }

  
  return airInSensedVolume; 
}


//CONVERSIONS


float volumeToCubicMeters(float volume) {
  float convertedVolume = volume / 1000000.0f;
  return convertedVolume;
}

float pressureToPascals(float pressure) {
  float convertedPressure = pressure * 1.0f;
  return convertedPressure;
}
