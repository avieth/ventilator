
#include "display_control.h"

#define SENSORLOOPTIME 176000 //microseconds
#define CONTROLLERLOOPTIME 10000 // microseconds
#define DISPLAYLOOPTIME 100000 //microseconds 
#define HARDWARELOOPTIME 0 //microseconds

long	currentTime;


controls inputs;
sensors sensorStates;
recordings measuredValues;



void setup() {

  Serial.begin(115200);
  while(!Serial){}

  currentTime = 0;
  
  pinMode(STEPPIN, OUTPUT);
  pinMode(DIRPIN, OUTPUT);
  pinMode(LIMITSWITCHUPPER, INPUT_PULLUP);
  pinMode(LIMITSWITCHLOWER, INPUT_PULLUP);
  digitalWrite(DIRPIN, HIGH);
  
  setAngleClosedLoop(0); 
  setAngleOpenLoop(0); 
  InitMotor(); 
  display_setup();
  inputs_setup(&inputs);
  sensors_setup(&sensorStates);
  recordings_setup(&measuredValues);
}


long lastSensorLoopTime = 0;
long lastControllerLoopTime = 0;
long lastDisplayLoopTime = 0;

float mode_target = 10.0f;  
float mode_velocity = 10.0f; 

long t_insp = 0; //seconds
long t_exp = 0; //seconds

float x_stroke = 0.0f; //m
float fullStrokeAngle = 0.0f; //millidegrees

bool activeDeviceTarget = false;

float inspVolume;
float airVolume;


void loop() {
  
  currentTime = micros();
  input_control(currentTime, &inputs, &measuredValues);

  if ((currentTime - lastDisplayLoopTime) > DISPLAYLOOPTIME) {

   Serial.print("Open Loop Angle: "); Serial.println(getAngleOpenLoop()); 
   Serial.print("Closed Loop Angle: "); Serial.println(getAngleClosedLoop()); 
   Serial.print("Stroke Angle: "); Serial.println(fullStrokeAngle); 
   Serial.print("x stroke: "); Serial.println(x_stroke);    
   Serial.print("x position: "); Serial.println(forwardKinematics(getAngleClosedLoop()));   
   Serial.print("inv angle: "); Serial.println(inverseKinematics(forwardKinematics(getAngleClosedLoop()))); 
   Serial.print("target: "); Serial.println(mode_target);    
   Serial.print("target velocity: "); Serial.println(mode_velocity );
   Serial.print("sensed velocity: "); Serial.println(getAngularVelocityClosedLoop()); 
   Serial.print("upper limit switch: "); Serial.println(digitalRead(LIMITSWITCHUPPER)); 
   Serial.print("lower limit switch: "); Serial.println(digitalRead(LIMITSWITCHLOWER)); 
   Serial.print("insp pressure reading: "); Serial.println(get_insp_pressure(&sensorStates) * 10.2);
   
   
   Serial.println(); 
        
    lastDisplayLoopTime = currentTime; 
  }



  if ((currentTime - lastSensorLoopTime) > SENSORLOOPTIME) {
    lastSensorLoopTime = currentTime; 

    // 3 flow info
    if(sensorStates.isExpFlow){
      float expVolume = get_exp_sensed_volume(&sensorStates);
      expVolume = expVolume * 1000;
      measuredValues.vte = (int)expVolume;    
    }

    inspVolume = get_insp_sensed_volume(&sensorStates);
    airVolume = get_air_in_sensed_volume(&sensorStates);
  }

  
  if ((currentTime - lastControllerLoopTime) > CONTROLLERLOOPTIME) {

    int desiredBPM = measuredValues.breathRate;
    float desiredVolume = measuredValues.tidalVolume;     
    float desiredPressure = measuredValues.pPeak / 10.2; //cmH2O to PA
    float ieRatio = measuredValues.ieRatio;
    
    if(measuredValues.startStopState && measuredValues.startStopStateFlag == 0){
      controlMode0(desiredBPM, desiredVolume, desiredPressure, ieRatio);
//       controlMode0(12,900,0);  
    }
     
    lastControllerLoopTime = currentTime; 
  }

  
  if ((currentTime - lastControllerLoopTime) > HARDWARELOOPTIME) {
      if(activeDeviceTarget){

         if(!digitalRead(LIMITSWITCHUPPER) || !digitalRead(LIMITSWITCHLOWER)){
          
         }else{
          moveToClosedLoop(mode_target, mode_velocity); 
        }
         
      }
  }
    
  lcd_control(currentTime, &measuredValues);
}



int cycleState = 3;
long lastCycleTime = 0;

int setFrequency = 0; //cycles per minute
float setVolume = 0; //millilitres
float setPressure = 0; // Pa 


void controlMode0(int frequency, float volume, float pressure, float idRatio) { //frequency in BPM, volume in mL, Pa 

  activeDeviceTarget = true;

//  Serial.println(cycleState); 
  
  long currentCycleTime = millis(); 
  
  if(cycleState == 3 && ((setFrequency != frequency) || (setVolume != volume) ||(setPressure != pressure))){
    setFrequency = frequency;
    setVolume = volume;
    cycleState = 0;
  }
    
  if(cycleState == 0){
    float millisecsperbreath = (60.0f *1000.0f) / setFrequency; 

    t_insp = millisecsperbreath/3; //1/3 the period for inspiration in milliseconds
    t_exp = 2*millisecsperbreath/3; //2/3 the period for expiration in milliseconds
      
    x_stroke = volume_To_CubicMeters(volume)/ (BELLOWSDIAMETER * BELLOWSDIAMETER*3.14f/ 4.0f); // in mm
    fullStrokeAngle = inverseKinematics(x_stroke+DEVICESTARTPOSITION);
  
    setPressure = pressure;

    cycleState = 1;
    lastCycleTime = millis();
  }
  else if(cycleState == 1){
   
    sensorStates.isInspFlow = 1; 
    
    //sense pressure and if the pressure exceeds max pressure then set the target to the full stroke angle
    if (get_insp_pressure(&sensorStates) > setPressure) {
      fullStrokeAngle = getAngleClosedLoop();
    }
      
    mode_target = fullStrokeAngle; 
    mode_velocity = fullStrokeAngle / (t_insp/1000.0f);

    if ((currentCycleTime - lastCycleTime) > t_insp) {
      // before expiration, finished inspiration
      measuredValues.pMean = get_insp_pressure(&sensorStates) * 10.2; //kpa to cmH2O
             
      lastCycleTime = millis();
      sensorStates.isInspFlow = 0; 
      cycleState = 2;
    }
  }
  else if(cycleState == 2){
    sensorStates.isExpFlow = 1; 
    sensorStates.isAirInFlow = 1; 
    mode_target = startAngle; 
    mode_velocity = fullStrokeAngle / (t_exp/1000.0f);
    
    if ((currentCycleTime - lastCycleTime) > t_exp) {
      lastCycleTime = millis();
      cycleState = 3;
      sensorStates.isExpFlow = 0; 
      sensorStates.isAirInFlow = 0; 

      // pressure at end of expiration 
      measuredValues.peep = get_insp_pressure(&sensorStates) * 10.2; //kpa to cmH2O
    }
  }
  else if(cycleState == 3){ //completedCycle

    //TODO:check angle and correct steps

    
    cycleState = 1;
  }
}




void standByMode() { 
  activeDeviceTarget = false;
}
