#include "sensor_control.h"


#define INPUTS_CONTROL_RATE           100000 // reading every 20000us or 50Hz

#define DEBOUNCE_LIMIT                400000
#define START_STOP_LIMIT              2000000
#define CURSOR_ACTIVITY_LIMIT         6000000
#define START_STOP_CONFIRM_LIMIT      4000000 

#define MAX_INDEX                     3

#define BREATHRATE_INCREMENT          1
#define TIDALVOLUME_INCREMENT         10
#define PPEAK_INCREMENT               1.0
#define IERATIO_INCREMENT             0.1


#define MAX_BREATHRATE                40
#define MAX_TIDALVOLUME               999
#define MAX_PPEAK                     40.0
#define MAX_IERATIO                   4.0

#define MIN_BREATHRATE                12
#define MIN_TIDALVOLUME               300
#define MIN_PPEAK                     5.0
#define MIN_IERATIO                   1



#define KNOB_ENC_A                    3         
#define KNOB_ENC_B                    2

#define BUTTON_LEFT                   15
#define BUTTON_RIGHT                  14
#define BUTTON_CONFIRM                4
#define BUTTON_START_STOP             5

#define START_LED                     A6



typedef struct ctrls{     
  int startStopEventFlag;
  
  int rotationCount;
  Encoder *enc;
}controls;


typedef struct data{
  int cursorIndex;
  int cursorActive;
  int valueSet;
  int startStopState;
  int startStopStateFlag;
    
  int breathRate;     // Respiratory rate 
  int tidalVolume;    // tidal volume, inspiration volume
  float pPeak;        // peak inspiratory pressure
  float ieRatio;      // time ratio between inspiration expiration
  
  int FiO2;           // FiO2 oxygen concentration mixture level   
  int vte;            // end tidal volume, exhalation volume
  float pMean;        // pressure during pause before expiration
  float peep;         // pressure remaining in lungs at end expiration
  
  int iTemp;
  int fTemp;
}recordings;





void inputs_setup(controls *inputs);
void recordings_setup(recordings *values);

int cursorIndex_manager(int cursorActive, int cursorIndex, int increment);

int value_change_manager(int currentEncoderValue, int pastEncoderValue, int resetFlag, recordings *measurements);
int breathRate_manager(int currentValue, int increment);
int tidalVolume_manager(int currentValue, int increment);
float pPeak_manager(float currentValue, int increment);
float ieRatio_manager(float currentValue, int increment);

//float peep_manager(float currentValue, int increment);
//int oxygenPercent_manager(int currentValue, int increment);
//int vte_manager(int currentValue, int increment);
//float pMean_manager(float currentValue, int increment);
//float fTotal_manager(float currentValue, int increment);

void read_inputs(controls *inputs, recordings *measurements, long currentTime, int timerReset);
void input_control(long currenTime, controls *inputs, recordings *measurements);



void inputs_setup(controls *inputs){
  pinMode(START_LED, OUTPUT);
  digitalWrite(START_LED, LOW);
  
  inputs->enc = new Encoder(KNOB_ENC_A, KNOB_ENC_B);
  inputs->enc->write(0);
}


void recordings_setup(recordings *values){
  values->cursorIndex = 0;
  values->cursorActive = 0;
  values->valueSet = 1; // reset due to changes in cursorIndex 
  values->startStopState = 0;
  values->startStopStateFlag = 0;
  
  values->breathRate = 20;
  values->tidalVolume = 800;
  values->pPeak = 40.0;
  values->ieRatio = 2.0;

  values->FiO2 = 20;
  values->vte = 0.0;
  values->pMean = 0.0;
  values->peep = 0.0;
}



int cursorIndex_manager(int cursorActive, int cursorIndex, int increment){
  int newIndex = cursorIndex;
  
  if(cursorActive){
    newIndex = newIndex + increment;
    if(newIndex > MAX_INDEX){
      newIndex = 0;  
    }
    if(newIndex < 0){
      newIndex = MAX_INDEX;  
    }
  }

  return newIndex;
}



int value_change_manager(int currentEncoderValue, int pastEncoderValue, int resetFlag, recordings *measurements){

  int increment = currentEncoderValue - pastEncoderValue;
  int index = measurements->cursorIndex;
  
  if(resetFlag == 0 && increment == 0){
    index = -1;
  }
  else{
    measurements->valueSet = 0;
  }

  switch(index){
    case 0:
      if(measurements->valueSet){
        measurements->iTemp = measurements->breathRate;   
      }
      if(resetFlag){
        measurements->breathRate = measurements->iTemp;
        measurements->valueSet = 1;
        break;
      }
      measurements->breathRate = breathRate_manager(measurements->breathRate, increment);
    break;

      
    case 1:
      if(measurements->valueSet){
        measurements->iTemp = measurements->tidalVolume;   
      }
      if(resetFlag){
        measurements->tidalVolume = measurements->iTemp;
        measurements->valueSet = 1;  
        break;
      }
      measurements->tidalVolume = tidalVolume_manager(measurements->tidalVolume, increment);
    break;


    case 2:
      if(measurements->valueSet){
        measurements->fTemp = measurements->pPeak;   
      }
      if(resetFlag){
        measurements->pPeak = measurements->fTemp;
        measurements->valueSet = 1;
        break;
      }
      measurements->pPeak = pPeak_manager(measurements->pPeak, increment);
    break;


    case 3:
      if(measurements->valueSet){
        measurements->fTemp = measurements->ieRatio;   
      }
      if(resetFlag){
        measurements->ieRatio = measurements->fTemp;
        measurements->valueSet = 1;
        break;
      }
      measurements->ieRatio = ieRatio_manager(measurements->ieRatio, increment);
    break;

    default:
    break;
  }

  return increment;
}


int breathRate_manager(int currentValue, int increment){
  int breathRate = currentValue;
  
  if(increment > 0){
    breathRate = breathRate + BREATHRATE_INCREMENT;
    if(breathRate > MAX_BREATHRATE){
      breathRate = MAX_BREATHRATE;
    }
  }
  else{
	  breathRate = breathRate - BREATHRATE_INCREMENT;
	  if(breathRate < MIN_BREATHRATE){
	    breathRate = 0;
	  }
    
  }

  return breathRate;
}


int tidalVolume_manager(int currentValue, int increment){
  int tidalVolume = currentValue;  
  
  if(increment > 0){
	  tidalVolume = tidalVolume + TIDALVOLUME_INCREMENT;
	  if(tidalVolume > MAX_TIDALVOLUME){
	    tidalVolume = MAX_TIDALVOLUME;
	  }
  }
  else{
	  tidalVolume = tidalVolume - TIDALVOLUME_INCREMENT;
	  if(tidalVolume < MIN_TIDALVOLUME){
	    tidalVolume = 0;
	  } 
  }    
  
  return tidalVolume;
}


float pPeak_manager(float currentValue, int increment){
  float pPeak = currentValue;
  
  if(increment > 0){
    pPeak = pPeak + PPEAK_INCREMENT;
    if(pPeak > MAX_PPEAK){
      pPeak = MAX_PPEAK;
    }
  }
  else{
    pPeak = pPeak - PPEAK_INCREMENT;
    if(pPeak < MIN_PPEAK){
      pPeak = 0.0;  
    }
  }  
  
  return pPeak;
}


float ieRatio_manager(float currentValue, int increment){
  float ieRatio = currentValue;

  if(increment > 0){
    ieRatio = ieRatio + IERATIO_INCREMENT;
    if(ieRatio > MAX_IERATIO){
      ieRatio = MAX_IERATIO;
    }
  }
  else{
    ieRatio = ieRatio + IERATIO_INCREMENT;
    if(ieRatio < MIN_IERATIO){
      ieRatio = 0.0;
    }
  }

  return ieRatio;
}


void read_inputs(controls *inputs, recordings *measurements, long currentTime, int timerReset){

  static long buttonLeftTimeout = 0;
  static long buttonRightTimeout = 0;
  static long buttonConfirmTimeout = 0;
  static long startStopEventTimeout = 0;
  static long cursorActivityTimeout = 0;
  static long startStopConfirmTimeout = 0;

  int currentEncoderReading;
  
  if(currentTime - buttonLeftTimeout >= DEBOUNCE_LIMIT){
    if(digitalRead(BUTTON_LEFT)){
      if(!measurements->valueSet){ //value not set revert 
        value_change_manager(0, 0, 1, measurements); 
      }
      measurements->cursorIndex = cursorIndex_manager(measurements->cursorActive, measurements->cursorIndex, -1);
      measurements->cursorActive = 1;
      cursorActivityTimeout = currentTime;
      buttonLeftTimeout = currentTime;      
    }
  }


  if(currentTime - buttonRightTimeout >= DEBOUNCE_LIMIT){
    if(digitalRead(BUTTON_RIGHT)){
      if(!measurements->valueSet){
        value_change_manager(0, 0, 1, measurements);    
      } 
      measurements->cursorIndex = cursorIndex_manager(measurements->cursorActive, measurements->cursorIndex, 1);
      measurements->cursorActive = 1;
      cursorActivityTimeout = currentTime;
      buttonRightTimeout = currentTime;
    }
  }

  
  if(currentTime - buttonConfirmTimeout >= DEBOUNCE_LIMIT){
    if(digitalRead(BUTTON_CONFIRM)){

      if(measurements->startStopStateFlag){
        measurements->startStopStateFlag = 0;
        digitalWrite(START_LED, measurements->startStopState);
      }
      
      
      measurements->valueSet = 1;
      buttonConfirmTimeout = currentTime;
    }
  }


  if(digitalRead(BUTTON_START_STOP)){
    if(currentTime - startStopEventTimeout >= START_STOP_LIMIT){
      startStopEventTimeout = currentTime;

      if(inputs->startStopEventFlag){
        startStopConfirmTimeout = currentTime;
        measurements->startStopState = !measurements->startStopState;
        measurements->startStopStateFlag = 1;    
      }

          
      inputs->startStopEventFlag = 1;
    }
  }
  else{
    inputs->startStopEventFlag = 0;
  }


  //encoder
  if(measurements->cursorActive){
    currentEncoderReading = inputs->enc->read();
    if(value_change_manager(currentEncoderReading, inputs->rotationCount, 0, measurements) != 0){
      cursorActivityTimeout = currentTime;
    }
    inputs->rotationCount = currentEncoderReading;
  }


  // timeouts
  if(currentTime - cursorActivityTimeout >= CURSOR_ACTIVITY_LIMIT){
    if(!measurements->valueSet){
      value_change_manager(0, 0, 1, measurements);
    }
    measurements->cursorActive = 0;
  }


  if(currentTime - startStopConfirmTimeout >= START_STOP_CONFIRM_LIMIT){
    if(measurements->startStopStateFlag){
      measurements->startStopStateFlag = 0;
      measurements->startStopState = !measurements->startStopState;      
    }
  }

  if(timerReset){
    buttonLeftTimeout = 0;
    buttonRightTimeout = 0;
    buttonConfirmTimeout = 0;
    startStopEventTimeout = 0;
    cursorActivityTimeout = 0;
    startStopConfirmTimeout = 0;  
  }
}



void input_control(long currentTime, controls *inputs, recordings *measurements){
  static long lastInputsEvent = 0;
  
  if(currentTime - lastInputsEvent >= INPUTS_CONTROL_RATE){
    lastInputsEvent = currentTime;
    read_inputs(inputs, measurements, currentTime, 0); 
  }

  if(lastInputsEvent - currentTime > INPUTS_CONTROL_RATE){
    lastInputsEvent = 0;
    read_inputs(inputs, measurements, currentTime, 1);
  }
}
