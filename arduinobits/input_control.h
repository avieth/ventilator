#define INPUTS_CONTROL_RATE           50000

#define DEBOUNCE_LIMIT                400000
#define START_STOP_LIMIT              2000000
#define CURSOR_ACTIVITY_LIMIT         6000000
#define START_STOP_CONFIRM_LIMIT      4000000 

#define MAX_INDEX                     3

#define BREATHRATE_INCREMENT          1
#define TIDALVOLUME_INCREMENT         10
#define PPEAK_INCREMENT               98.0

// TODO not necessary; this is expressed in the
// ventilator logic itself, the UI should simply
// obey what it's told.
#define MAX_BREATHRATE                40
#define MAX_TIDALVOLUME               999
#define MAX_PPEAK                     4000.0

#define MIN_BREATHRATE                1
#define MIN_TIDALVOLUME               300
#define MIN_PPEAK                     0.0
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

/**
 * 
 */
typedef struct input_state {
  display_state *input_display_state;
  controls input_controls;

  // These pointers will be written with new values whenever there is
  // a commit. See corresponding stuff in display_state.
  uint8_t *bpm;
  uint32_t *volume;
  uint32_t *pressure;
  uint8_t *ie_inhale;
  uint8_t *ie_exhale;
} input_state;

// To be populated on inputs_setup.
input_state inputState;


input_state *inputs_setup(display_state *displayState, uint8_t *in_bpm_ptr, uint32_t *in_volume_ptr, uint32_t *in_pressure_ptr, uint8_t *in_ie_inhale_ptr, uint8_t *in_ie_exhale_ptr);

int cursorIndex_manager(int cursorActive, int cursorIndex, int increment);
int value_change_manager(int currentEncoderValue, int pastEncoderValue, int resetFlag);
uint8_t breathRate_manager(uint8_t currentValue, int increment);
uint32_t tidalVolume_manager(uint32_t currentValue, int increment);
uint32_t pPeak_manager(uint32_t currentValue, int increment);
void ieRatio_manager(uint8_t *currentInhale, uint8_t *currentExhale, int increment);

//float peep_manager(float currentValue, int increment);
//int oxygenPercent_manager(int currentValue, int increment);
//int vte_manager(int currentValue, int increment);
//float pMean_manager(float currentValue, int increment);
//float fTotal_manager(float currentValue, int increment);

void read_inputs(long currentTime, int timerReset);
void input_control(long currenTime);

/**
 * Prepares the input state and returns a pointer to it.
 */
input_state *inputs_setup(display_state *displayState, uint8_t *in_bpm_ptr, uint32_t *in_volume_ptr, uint32_t *in_pressure_ptr, uint8_t *in_ie_inhale_ptr, uint8_t *in_ie_exhale_ptr){
  pinMode(START_LED, OUTPUT);
  digitalWrite(START_LED, HIGH);
  inputState.input_controls.enc = new Encoder(KNOB_ENC_A, KNOB_ENC_B);
  inputState.input_controls.enc->write(0);
  inputState.input_display_state = displayState;
  inputState.bpm = in_bpm_ptr;
  inputState.volume = in_volume_ptr;
  inputState.pressure = in_pressure_ptr;
  inputState.ie_inhale = in_ie_inhale_ptr;
  inputState.ie_exhale = in_ie_exhale_ptr;
  return &inputState;
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



int value_change_manager(int currentEncoderValue, int pastEncoderValue, int resetFlag){

  // Bad name, no search+replace in this stupid IDE AFAIK
  display_state *measurements = inputState.input_display_state;

  int increment = currentEncoderValue - pastEncoderValue;
  int index = measurements->cursorIndex;
  
  if(resetFlag == 0 && increment == 0){
    index = -1;
  }
  else{
    measurements->valueSet = 0;
  }

  switch(index){
    // Set BPM
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
      // Set tidal volume
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


    // Changes the I:E ratio.
    // TODO should be able to change numerator or denominator.
    case 3:
      if(measurements->valueSet){
        measurements->fTemp = measurements->ieExhale;
      }
      if(resetFlag){
        measurements->ieExhale = measurements->fTemp;
        measurements->valueSet = 1;
        break;
      }
      ieRatio_manager(&(measurements->ieInhale), &(measurements->ieExhale), increment);
    break;

    default:
    break;
  }

  return increment;
}


uint8_t breathRate_manager(uint8_t currentValue, int increment){
  uint8_t breathRate = currentValue;
  
  if(increment > 0){
    if(breathRate > MAX_BREATHRATE){
      return MAX_BREATHRATE;
    } else {
      return currentValue + 1;
    }
  } else{
	  if(breathRate < MIN_BREATHRATE){
	    return MIN_BREATHRATE;
	  } else { 
      return currentValue - 1;
    }
  }
}


uint32_t tidalVolume_manager(uint32_t currentValue, int increment){
  int32_t tidalVolume = currentValue;  
  if(increment > 0){
	  if(tidalVolume > MAX_TIDALVOLUME){
	    return MAX_TIDALVOLUME;
	  } else {
	    return tidalVolume + TIDALVOLUME_INCREMENT;
	  }
  } else{
	  if(tidalVolume < MIN_TIDALVOLUME){
	    return MIN_TIDALVOLUME;
	  } else {
      return tidalVolume - TIDALVOLUME_INCREMENT;
	  }
  }    
}


uint32_t pPeak_manager(uint32_t currentValue, int increment){
  uint32_t pPeak = currentValue;
  
  if(increment > 0){
    if(pPeak > MAX_PPEAK){
      return MAX_PPEAK;
    } else {
      return pPeak + PPEAK_INCREMENT;
    }
  } else {
    if(pPeak < MIN_PPEAK){
      return MIN_PPEAK;
    } else {
      return pPeak - PPEAK_INCREMENT;
    }
  }  
  
  return pPeak;
}

/**
 * Increments/decrements the exhale portion only.
 * 
 * How can we control a ratio succinctly in one variable?
 * Imagine 1:1 is the lowest. What lies in between?
 * 
 *  1:1
 *  2:3
 *  1:2
 *  1:3
 *  1:4
 * 
 * Can we do this in such a way that we change only the denominator?
 * Surely not....
 */
void ieRatio_manager(uint8_t *currentInhale, uint8_t *currentExhale, int increment){
  // Increase the ratio (make it bigger as a fraction).  Maximum is 1:1.
  if (increment > 0){
    if (*currentInhale == 1 && *currentExhale == 4) {
      *currentInhale = 1;
      *currentExhale = 3;
    } else if (*currentInhale == 1 && *currentExhale == 3) {
      *currentInhale = 1;
      *currentExhale = 2;
    } else if (*currentInhale == 1 && *currentExhale == 2) {
      *currentInhale = 2;
      *currentExhale = 3;
    } else {
      *currentInhale = 1;
      *currentExhale = 1;
    }
  } else {
    if (*currentInhale == 1 && *currentExhale == 1) {
      *currentInhale = 2;
      *currentExhale = 3;
    } else if (*currentInhale == 2 && *currentExhale == 3) {
      *currentInhale = 1;
      *currentExhale = 2;
    } else if (*currentInhale == 1 && *currentExhale == 2) {
      *currentInhale = 1;
      *currentExhale = 3;
    } else if (*currentInhale == 1 && *currentExhale == 3) {
      *currentInhale = 1;
      *currentExhale = 4;
    } else {
      *currentInhale = 1;
      *currentExhale = 4;
    }
  }
}


void read_inputs(long currentTime, int timerReset){

  controls *inputs = &(inputState.input_controls);
  display_state *measurements = inputState.input_display_state;

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
        value_change_manager(0, 0, 1); 
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
        value_change_manager(0, 0, 1);    
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

      // TODO factor this out into its own routine for clarity: commits the changes.
      *(inputState.bpm) = inputState.input_display_state->breathRate;
      *(inputState.volume) = inputState.input_display_state->tidalVolume;
      *(inputState.pressure) = inputState.input_display_state->pPeak;
      *(inputState.ie_inhale) = inputState.input_display_state->ieInhale;
      *(inputState.ie_exhale) = inputState.input_display_state->ieExhale;

      
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
    if(value_change_manager(currentEncoderReading, inputs->rotationCount, 0) != 0){
      cursorActivityTimeout = currentTime;
    }
    inputs->rotationCount = currentEncoderReading;
  }


  // timeouts
  if(currentTime - cursorActivityTimeout >= CURSOR_ACTIVITY_LIMIT){
    if(!measurements->valueSet){
      value_change_manager(0, 0, 1);
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



void input_control(long currentTime){
  static long lastInputsEvent = 0;
  
  if(currentTime - lastInputsEvent >= INPUTS_CONTROL_RATE){
    lastInputsEvent = currentTime;
    read_inputs(currentTime, 0); 
  }

  if(lastInputsEvent - currentTime > INPUTS_CONTROL_RATE){
    lastInputsEvent = 0;
    read_inputs(currentTime, 1);
  }
}
