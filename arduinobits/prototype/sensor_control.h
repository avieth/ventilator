#include "pressure_sensors.h"
#include "MotorDriver.h"

#define SENSOR_CONTROL_RATE       176000

#define SENSOR_LOOP_TIME          0.00293//0.176

#define INSP_PRESSURE_PIN         A0
#define INSP_FLOW_PIN             A2
#define EXP_FLOW_PIN              A3
#define AIR_INFLOW_PIN            A1         


typedef struct sens{
  int inspPressureOffset;   
  int inspFlowOffset; 
  int expFlowOffset; 
  int airInFlowOffset;

  bool isInspFlow;
  bool isExpFlow;
  bool isAirInFlow; 
}sensors;



void sensors_setup(sensors *sensorStates);
float get_insp_pressure(sensors *sensorStates);
float get_insp_flow(int inspFlowOffset);
float get_exp_flow(int expFlowOffset);
float get_air_in_flow(int airInFlowOffset);
float get_insp_sensed_Volume(sensors *sensorStates);
float get_exp_sensed_Volume(sensors *sensorStates);
float get_air_in_sensed_volume(sensors *sensorStates);
float get_piston_volume();
float volume_To_CubicMeters(float volume);
float pressure_To_Pascals(float pressure);



void sensors_setup(sensors *sensorStates){
  sensorStates->inspPressureOffset = sensor_offset(INSP_PRESSURE_PIN);
  sensorStates->inspFlowOffset = sensor_offset(INSP_FLOW_PIN);
  sensorStates->expFlowOffset = sensor_offset(EXP_FLOW_PIN);
  sensorStates->airInFlowOffset = sensor_offset(AIR_INFLOW_PIN);

  sensorStates->isInspFlow = false;
  sensorStates->isExpFlow = false;
  sensorStates->isAirInFlow = false;
}


float get_insp_pressure(sensors *sensorStates){
  static int lastInspPressureReading = 0;
  static int lastLastInspPressureReading = 0;

  int rawData = read_sensor(INSP_PRESSURE_PIN); 
  float pressure = pressure_difference(sensorStates->inspPressureOffset, rawData); 

  float inspPressure= 0.6f*pressure + 0.3f*lastInspPressureReading + 0.2f*lastLastInspPressureReading;   
  
  lastInspPressureReading = pressure; 
  lastLastInspPressureReading = lastInspPressureReading; 

  return inspPressure;
}


float get_insp_flow(int inspFlowOffset){
  static int lastInspFlowReading = 0;
  static int lastLastInspFlowReading = 0;

  int rawData = read_sensor(INSP_FLOW_PIN); 
  float flow = flow_rate(inspFlowOffset, rawData); 

  float inspFlow= 0.6f*flow + 0.3f*lastInspFlowReading + 0.2f*lastLastInspFlowReading;   
  
  lastInspFlowReading = flow; 
  lastLastInspFlowReading = lastInspFlowReading; 

  return inspFlow;
}



float get_exp_flow(int expFlowOffset){
  static int lastExpFlowReading = 0;
  static int lastLastExpFlowReading = 0;

  int rawData = read_sensor(EXP_FLOW_PIN); 
  float flow = flow_rate(expFlowOffset, rawData); 

  float expFlow= 0.6f*flow + 0.3f*lastExpFlowReading + 0.2f*lastLastExpFlowReading;   
  
  lastExpFlowReading = flow; 
  lastLastExpFlowReading = lastExpFlowReading; 

  return expFlow;
}



float get_air_in_flow(int airInFlowOffset){
  static int lastAirInFlowReading = 0;
  static int lastLastAirInFlowReading = 0;
  
  int rawData = read_sensor(AIR_INFLOW_PIN); 
  float flow = flow_rate(airInFlowOffset, rawData); 

  float airInFlow= 0.6f*flow + 0.3f*lastAirInFlowReading + 0.2f*lastLastAirInFlowReading;   
  
  lastAirInFlowReading = flow; 
  lastLastAirInFlowReading = lastAirInFlowReading; 

  return airInFlow;
}


float get_insp_sensed_volume(sensors *sensorStates){
  static float inspSensedVolume = 0.0;
  
  if(sensorStates->isInspFlow){
    inspSensedVolume = inspSensedVolume + get_insp_flow(sensorStates->inspFlowOffset) * SENSOR_LOOP_TIME;    
  }
  else{
    inspSensedVolume = 0.0;
  }

  return inspSensedVolume;
}


float get_exp_sensed_volume(sensors *sensorStates){
  static float expSensedVolume = 0;
  
  if(sensorStates->isExpFlow){
    expSensedVolume = expSensedVolume + get_exp_flow(sensorStates->expFlowOffset) * SENSOR_LOOP_TIME;
  }
  else{
    expSensedVolume = 0.0;
  }

  return expSensedVolume;
}


float get_air_in_sensed_volume(sensors *sensorStates){
  static float airInSensedVolume;
  
  if(sensorStates->isAirInFlow){
    airInSensedVolume = airInSensedVolume + get_air_in_flow(sensorStates->airInFlowOffset) * SENSOR_LOOP_TIME;  
  }
  else{
    airInSensedVolume = 0.0;
  }
    
  return airInSensedVolume;
}


float get_piston_volume(){
  float volume = (BELLOWSDIAMETER * BELLOWSDIAMETER / 4.0f * 3.14f) * (forwardKinematics(getAngleClosedLoop())-DEVICESTARTPOSITION) ;

  return volume; 
}



// conversions
float volume_To_CubicMeters(float volume) {
  float convertedVolume = volume / 1000000.0f;
  return convertedVolume;
}


float pressure_To_Pascals(float pressure){
  float convertedPressure = pressure * 1.0f;
  return convertedPressure;
}
