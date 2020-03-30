/*
   20200328
   author: Colin Gallacher
   v0.1

*/

#include "Pressure_Sensors.h"

#define CMH20TOPASCAL 98.0665; 

#define INSPPRESSUREPIN 8
#define EXPPRESSUREPIN 9
#define INSPFLOWPIN 10
#define EXPFLOWPIN 11
#define AIRINFLOWPIN 12

int inspPressureOffset = 0; 
int expPressureOffset = 0; 
int inspFlowOffset= 0; 
int expFlowOffset = 0; 
int airInFlowOffset = 0; 

//INITIALIZATION 

bool initializeSensors(){
  
 inspPressureOffset = sensor_offset(INSPPRESSUREPIN);
 expPressureOffset = sensor_offset(EXPPRESSUREPIN);
 inspFlowOffset= sensor_offset(INSPFLOWPIN);
 expFlowOffset = sensor_offset(EXPFLOWPIN);
 airInFlowOffset = sensor_offset(AIRINFLOWPIN);
  
}

//SENSORS
int lastInspPressureReading = 0; 
int lastLastInspPressureReading = 0; 

float getInspPressure() {

  int rawData = read_sensor(INSPPRESSUREPIN); 
  float pressure = pressure_difference(inspPressureOffset, rawData); 

  float inspPressure= 0.6f*pressure + 0.3f*lastInspPressureReading + 0.2f*lastLastInspPressureReading;   
  
  lastInspPressureReading = pressure; 
  lastLastInspPressureReading = lastInspPressureReading; 

  return inspPressure;
}

int lastExpPressureReading = 0; 
int lastLastExpPressureReading = 0; 

float getExpPressure() {

  int rawData = read_sensor(EXPPRESSUREPIN); 
  float pressure = pressure_difference(expPressureOffset, rawData); 

  float expPressure= 0.6f*pressure + 0.3f*lastExpPressureReading + 0.2f*lastLastExpPressureReading;   
  
  lastExpPressureReading = pressure; 
  lastLastExpPressureReading = lastExpPressureReading; 

  return expPressure;
}

int lastInspFlowReading = 0; 
int lastLastInspFlowReading = 0; 

float getInspFlow() {

  int rawData = read_sensor(INSPFLOWPIN); 
  float flow = flow_rate(inspFlowOffset, rawData); 

  float inspFlow= 0.6f*flow + 0.3f*lastInspFlowReading + 0.2f*lastLastInspFlowReading;   
  
  lastInspFlowReading = flow; 
  lastLastInspFlowReading = lastInspFlowReading; 

  return inspFlow;
}

int lastExpFlowReading = 0; 
int lastLastExpFlowReading = 0; 

float getExpFlow() {

  int rawData = read_sensor(EXPFLOWPIN); 
  float flow = flow_rate(expFlowOffset, rawData); 

  float expFlow= 0.6f*flow + 0.3f*lastExpFlowReading + 0.2f*lastLastExpFlowReading;   
  
  lastExpFlowReading = flow; 
  lastLastExpFlowReading = lastExpFlowReading; 

  return expFlow;
}


int lastAirInFlowReading = 0; 
int lastLastAirInFlowReading = 0; 

float getAirInFlow() {

  int rawData = read_sensor(AIRINFLOWPIN); 
  float flow = flow_rate(airInFlowOffset, rawData); 

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
