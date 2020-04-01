/*
   20200328
   author: Colin Gallacher
   v0.1

*/

#include <Encoder.h>
#include "DeviceKinematics.h"

// Define stepper motor connections:
#define DIRPIN 6
#define STEPPIN 7
#define LIMITSWITCHLOWER A14
#define LIMITSWITCHUPPER A13


#define INITVELOCITY 30
#define STEPSPERREV 800

#define ENCPIN0 20
#define ENCPIN1 21
#define PULSESPERREV 2800
#define INITANGLEOFFSET 144
#define ANGLERETRACT 4

#define FORWARD 1
#define REVERSE 0

Encoder Enc(ENCPIN0, ENCPIN1);
float ol_angle = 0;
float cl_angle = 0; 

bool moveUntilLimit(float angularVelocity, int motorDirection); 
bool moveToOpenLoop(float target, float angularVelocity); 
bool moveOpenLoop(float angle, float angularVelocity, int motorDirection); 
bool moveToClosedLoop(float target, float angularVelocity); 
bool moveToClosedLoopVelocity(float angularVelocity); 
bool moveClosedLoop(float angle, float angularVelocity, int motorDirection); 
float getAngleClosedLoop(); 
float getAngleOpenLoop(); 
float getAngularVelocityClosedLoop(); 
bool setAngleOpenLoop(float angle); 
bool setAngleClosedLoop(float angle);
bool InitMotor();
bool stepMotor(float angularVelocity, int motorDirection);
bool stepMotor1(float angularVelocity, int motorDirection);


//STEPPER CONTROL

int switchState = 1; 
int lastSwitchState =1; 

bool moveUntilLimit(float angularVelocity, int motorDirection) {

   switchState = digitalRead(LIMITSWITCHUPPER);

  while (digitalRead(LIMITSWITCHUPPER) && lastSwitchState == 1) {

    stepMotor(angularVelocity, motorDirection);
    lastSwitchState = switchState; 

  }
    setAngleOpenLoop(INITANGLEOFFSET); 
    setAngleClosedLoop(INITANGLEOFFSET);
 return true; 
}

bool moveToOpenLoop(float target, float angularVelocity) {

  bool activeTarget = abs(target - getAngleOpenLoop()) > 2.0f; // (360.0f / STEPSPERREV);

  if (activeTarget) {
    bool motorDirection = ((target - getAngleOpenLoop()) < 0) ? FORWARD : REVERSE;
    stepMotor(angularVelocity, motorDirection);
     return(0); 
  }
  else{      
    return(1); 
  }

}                                                                                                        

bool moveOpenLoop(float angle, float angularVelocity, int motorDirection) { //Blocking //angle in millidegrees , angularVelocity: millidegreespersecond, 

float target = motorDirection ? getAngleClosedLoop()- angle : getAngleClosedLoop()+angle;

  while (!moveToOpenLoop(target, angularVelocity)); 

  return true; 

}

bool moveToClosedLoop(float target, float angularVelocity) {

  bool activeTarget = abs(target - getAngleClosedLoop()) > 0.7f; // angle threshold for accuracy 

  if (activeTarget) {

    int motorDirection = ((target - getAngleClosedLoop()) < 0) ? FORWARD : REVERSE;
    stepMotor(angularVelocity, motorDirection);
    return(0); 
  }
  else{
    return(1); 
  }
}

bool moveToClosedLoopVelocity(float angularVelocity) {
//
//float target = getAngleClosedLoop() - angularVelocity*(CONTROLLERLOOPTIME/1000000.0f); 
//
////Serial.println(target); 
//moveToClosedLoop(target, angularVelocity);   

return true; 
}


bool moveClosedLoop(float angle, float angularVelocity, int motorDirection){


float target = motorDirection ? getAngleClosedLoop()- angle : getAngleClosedLoop()+angle; //deg

  while (!moveToClosedLoop(target, 2*angularVelocity)); 

  return true; 
}



float getAngleClosedLoop() {

  float angle = Enc.read() * 360.0f / PULSESPERREV;

  angle = angle > 360.0f ? angle - 360.0f : angle; //mdeg
  angle = angle < -360.0f ? angle + 360.0f : angle;

  cl_angle = angle; 

  return cl_angle;
}


float getAngleOpenLoop() {

  float angle = ol_angle;
  

  angle = angle > 360.0f ? angle - 360.0f : angle; //mdeg
  angle = angle < -360.0f ? angle + 360.0f : angle;

  ol_angle = angle; 

  return ol_angle;
}


long lastVelocityTime = 0; 
float lastAngleClosedLoop = 0; 
float lastVelocity =0; 
float lastLastVelocity =0; 
float lastLastLastVelocity = 0; 

float getAngularVelocityClosedLoop() {

  long currentVelocityTime = millis(); 

  float currentAngle = getAngleClosedLoop(); 

  float velocity = (currentAngle - lastAngleClosedLoop)*1000/(currentVelocityTime - lastVelocityTime); //*1000000 to put in seconds 

  velocity = (0.6f*velocity + 0.3f*lastVelocity+0.1f*lastLastVelocity); //+0.1f*lastLastLastVelocity );

  lastVelocity = velocity; 
  lastLastVelocity  = lastVelocity; 
  lastLastLastVelocity = lastLastVelocity; 
  
  lastVelocityTime = currentVelocityTime; 
  lastAngleClosedLoop = currentAngle; 

  return velocity;
}

bool setAngleOpenLoop(float angle) {

  ol_angle = angle;

  return true; 
}



bool setAngleClosedLoop(float angle) {

  Enc.write(angle* PULSESPERREV / (360.0f));

  return true; 
  
}

float startAngle;

bool InitMotor() {

  moveUntilLimit(INITVELOCITY, REVERSE);
//  moveOpenLoop(ANGLERETRACT, INITVELOCITY , FORWARD);
//  while(! moveToClosedLoop(143, INITVELOCITY)); 
  moveOpenLoop(ANGLERETRACT, INITVELOCITY , FORWARD);
  moveUntilLimit(INITVELOCITY , REVERSE);
  moveOpenLoop(ANGLERETRACT, INITVELOCITY , FORWARD);
  setAngleClosedLoop(INITANGLEOFFSET - ANGLERETRACT);
  setAngleOpenLoop(INITANGLEOFFSET - ANGLERETRACT); 
  startAngle = inverseKinematics(DEVICESTARTPOSITION);
  while(!moveToClosedLoop(startAngle, INITVELOCITY));

    return true; 
}

long lastPulseTime = 0; 
long lastStepTime = 0;
long pulseState = 0;
long stepDelay = 250;
bool hasPulsed = false; 
long timeBetweenSteps = 0; 


 bool stepMotor(float angularVelocity, int motorDirection) {
//  Serial.prlongln(pulseState); 

  angularVelocity= abs(angularVelocity); 


  long currentTime = micros();
  long a = 1000000*360; 
  long b = a/STEPSPERREV;
  long c = b/((long)angularVelocity) ; 
 
  stepDelay = c/2; 

  switch (pulseState) {
    case 0:
      
      digitalWrite(DIRPIN, motorDirection);
      digitalWrite(STEPPIN, HIGH);
      lastPulseTime = currentTime;
      pulseState = 1;
      break;

    case 1:
  
      if ((currentTime - lastPulseTime) > stepDelay) {    
        digitalWrite(STEPPIN, LOW);
        lastPulseTime = currentTime;
        pulseState = 2;
      }
      break;

    case 2:
      if ((currentTime - lastPulseTime) > stepDelay) {
        pulseState = 0;
        float angle = motorDirection ? getAngleOpenLoop() - 360.0f / STEPSPERREV : getAngleOpenLoop() + 360.0f / STEPSPERREV; //degrees to mdegrees
        setAngleOpenLoop(angle);
      }
      break;

    default:
      break; 
  }

   return true; 
}

bool stepMotor1(float angularVelocity, int motorDirection) {

  long currentTime = micros();
  long a = 1000000*360; 
  long b = a/STEPSPERREV;
  long c = b/((long)angularVelocity) ; 

      stepDelay = c/2; 

  digitalWrite(DIRPIN, motorDirection);

  // These four lines result in 1 step:
  digitalWrite(STEPPIN, HIGH);
  delayMicroseconds(stepDelay);
  digitalWrite(STEPPIN, LOW);
  delayMicroseconds(stepDelay);
  float angle = motorDirection ? getAngleOpenLoop() - 360.0f / STEPSPERREV : getAngleOpenLoop() + 360.0f / STEPSPERREV;
  setAngleOpenLoop(angle);

  return true; 
}
