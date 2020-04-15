#include <LiquidCrystal.h> 

#define DISPLAY_CONTROL_RATE  400000

#define MAX_LCD_COLUMNS       16
#define MAX_LCD_ROWS          2
#define LCD_BLOCK_SIZE        4
#define LCD_BLOCKS()          ((MAX_LCD_COLUMNS/LCD_BLOCK_SIZE)*MAX_LCD_ROWS)

#define LCD_RS                A4                        
#define LCD_EN                A5
#define LCD_D4                16
#define LCD_D5                17
#define LCD_D6                18
#define LCD_D7                19

#define LED A6

LiquidCrystal display(LCD_RS, LCD_EN, LCD_D4, LCD_D5, LCD_D6, LCD_D7);

/**
 * A display block on screen: 4 characters.
 * The valueSet is either 0 or 1. 1 means the value is set, 0 means it's
 * being changed.
 */
typedef struct block{
  char blockArray[LCD_BLOCK_SIZE];
  int valueSet;
}displayBlock; 

displayBlock lcdDisplay[LCD_BLOCKS()];

/**
 * What the display needs in order to render and to deal with stateful
 * change interactions.
 * 
 * The fields which are pointers are to be set to the addresses of global
 * operator control values, which shall be read by the ventilator program.
 * The UI may change them freely, but they will not necessarily be the same
 * as the displayed values, for the user input values may be nonsense.
 * Values to display will come as parametres to a display function.
 * 
 * Also, in the case of volume and pressure, these are pointers to the settings,
 * but what is displayed when these are not being changed is the current observed
 * value that corresponds to it.
 */
typedef struct display_state {
  
  int cursorIndex;
  int cursorActive;
  int valueSet;
  int modeState;
  int modeStateFlag;

  uint8_t state;

  // These hold the values that are being edited.
  uint8_t breathRate;
  uint32_t tidalVolume;
  uint32_t pPeak;
  // Numerator and denominator of the I:E ratio.
  uint8_t ieInhale;
  uint8_t ieExhale;
  
  int FiO2;           // FiO2 oxygen concentration mixture level   
  int vte;            // end tidal volume, exhalation volume
  float pMean;        // pressure during pause before expiration
  float peep;         // pressure remaining in lungs at end expiration

  int iTemp;
  int fTemp;
  
} display_state;

display_state *setup_display(uint8_t in_bpm_ptr, uint32_t in_volume_ptr, uint32_t in_pressure_ptr, uint8_t in_ie_inhale_ptr, uint8_t in_ie_exhale_ptr);
void load_display(displayBlock lcd[], uint8_t in_bpm, uint32_t in_volume, uint32_t in_pressure, uint8_t in_ie_inhale, uint8_t in_ie_exhale);
void display_values(displayBlock lcd[]);
void display_block(char blockArray[], int row, int column, int cursorBlink, int valueSet);
void format_display_block(char blockArray[], int blockIndex, int cursorState);
void int_to_display(int value, char blockArray[], int blockIndex, int cursorState);
void float_to_display(float value, char blockArray[], int blockIndex, int cursorState);
void ratio_to_display(unsigned char numerator, unsigned char denominator, char blockArray[], int blockIndex, int cursorState);
void display_start();
void display_stop();
void lcd_control(uint8_t in_bpm, uint32_t in_volume, uint32_t in_pressure, uint8_t in_ie_inhale, uint8_t in_ie_exhale);

/**
 * The state which this display controller will use in order to display
 * a GUI.
 */
display_state displayState;

/**
 * Populate the global displayState with initial values.
 * 
 * Also initializes the LCD library.
 * 
 * You get a pointer to the global display_state, which can be used to
 * implement an input controller (this display program only reads the
 * state, you can write as you wish).
 */
display_state *setup_display(uint8_t in_bpm, uint32_t in_volume, uint32_t in_pressure, uint8_t in_ie_inhale, uint8_t in_ie_exhale) {
  displayState.cursorIndex = 0;
  displayState.cursorActive = 0;
  displayState.valueSet = 1;
  displayState.modeState = 0;
  displayState.modeStateFlag = 0;

  displayState.state = 0;
  
  displayState.breathRate = in_bpm;
  displayState.tidalVolume = in_volume;
  displayState.pPeak = in_pressure;
  displayState.ieInhale = in_ie_inhale;
  displayState.ieExhale = in_ie_exhale;

  displayState.FiO2 = 20;
  displayState.vte = 0.0;
  displayState.pMean = 0.0;
  displayState.peep = 0.0;
  
  display.begin(MAX_LCD_COLUMNS, MAX_LCD_ROWS);
  return &displayState;
}

/**
 * Fill in the displayBlock according to the given information.
 * 
 * TODO this is not correct at the moment. Must take actual values from the
 * ventilator logic. We only show what's at the pointers when the user is modifying them.
 */
void load_display(displayBlock lcd[], uint8_t in_bpm, uint32_t in_volume, uint32_t in_pressure, uint8_t in_ie_inhale, uint8_t in_ie_exhale){
  int index = 0;
  int cursorState = 0;
  
  cursorState = (displayState.cursorActive && displayState.cursorIndex == 0) ? 1 : 0;
  // lcd[index].valueSet is 1 if the value is set, not if we _want_ to set it.
  // Sad overloading of the English word "set".
  lcd[index].valueSet = (displayState.valueSet == 0 && cursorState) ? 0 : 1;
  uint8_t bpm_to_display = (lcd[index].valueSet == 1) ? in_bpm : displayState.breathRate;
  int_to_display(bpm_to_display, lcd[index].blockArray, index, cursorState);
  index++;
  
  cursorState = (displayState.cursorActive && displayState.cursorIndex == 1) ? 1 : 0;
  lcd[index].valueSet = (displayState.valueSet == 0 && cursorState) ? 0 : 1;
  uint32_t volume_to_display = (lcd[index].valueSet == 1) ? in_volume : displayState.tidalVolume;
  int_to_display(volume_to_display, lcd[index].blockArray, index, cursorState);
  index++;
  
  cursorState = (displayState.cursorActive && displayState.cursorIndex == 2) ? 1 : 0;
  lcd[index].valueSet = (displayState.valueSet == 0 && cursorState) ? 0 : 1;
  uint32_t pressure_to_display = (lcd[index].valueSet == 1) ? in_pressure : displayState.pPeak;
  // TODO It's in Pa; convert to cm water, approximately.
  // Currently no support for showing negatives.
  pressure_to_display = pressure_to_display / 98;
  int_to_display(pressure_to_display, lcd[index].blockArray, index, cursorState);
  index++;
  
  cursorState = (displayState.cursorActive && displayState.cursorIndex == 3) ? 1 : 0;
  lcd[index].valueSet = (displayState.valueSet == 0 && cursorState) ? 0 : 1;
  uint8_t ie_inhale_to_display = (lcd[index].valueSet == 1) ? in_ie_inhale : displayState.ieInhale;
  uint8_t ie_exhale_to_display = (lcd[index].valueSet == 1) ? in_ie_exhale : displayState.ieExhale;
  ratio_to_display(ie_inhale_to_display, ie_exhale_to_display, lcd[index].blockArray, index, cursorState);
  index++;
  
  cursorState = (displayState.cursorActive && displayState.cursorIndex == 4) ? 1 : 0;
  lcd[index].valueSet = (displayState.valueSet == 0 && cursorState) ? 0 : 1;
  int_to_display(displayState.FiO2, lcd[index].blockArray, index, cursorState);
  index++;
  
  cursorState = (displayState.cursorActive && displayState.cursorIndex == 5) ? 1 : 0;
  lcd[index].valueSet = (displayState.valueSet == 0 && cursorState) ? 0 : 1;
  int_to_display(displayState.vte, lcd[index].blockArray, index, cursorState);
  index++;
  
  cursorState = (displayState.cursorActive && displayState.cursorIndex == 6) ? 1 : 0;
  lcd[index].valueSet = (displayState.valueSet == 0 && cursorState) ? 0 : 1;
  float_to_display(displayState.pMean, lcd[index].blockArray, index, cursorState);
  index++;
  
  cursorState = (displayState.cursorActive && displayState.cursorIndex == 7) ? 1 : 0;
  lcd[index].valueSet = (displayState.valueSet == 0 && cursorState) ? 0 : 1;
  float_to_display(displayState.peep, lcd[index].blockArray, index, cursorState);
}

void display_block(char blockArray[], int row, int column, int cursorBlink, int valueSet){
  display.setCursor(column, row);
  
  if(cursorBlink){
    display.write(32);

    // Alternate between spaces and the value, to indicate that it is
    // being set.
    if(valueSet){
      for(int i = 1; i < LCD_BLOCK_SIZE; i++){
        display.setCursor(column+i, row);
        display.write(blockArray[i]);
      }
    }
    else{
      for(int i = 1; i < LCD_BLOCK_SIZE; i++){
        display.setCursor(column+i, row);
        display.write(32);
      }
    }
  }
  else{
    for(int i = 0; i < LCD_BLOCK_SIZE; i++){
      display.setCursor(column+i, row);
      display.write(blockArray[i]);
    }
  }
}


void format_display_block(char blockArray[], int blockIndex, int cursorState){
  if(cursorState){
    blockArray[0] = 62;
  }
  else{
    blockArray[0] = 32;
  }
}

/**
 * Show the least-significant digits of numerator and denominator with a colon in between.
 * Numerator on the left, as is convention.
 */
void ratio_to_display(unsigned char numerator, unsigned char denominator, char blockArray[], int blockIndex, int cursorState) {
  blockArray[LCD_BLOCK_SIZE-3] = (numerator % 10) + 48;
  blockArray[LCD_BLOCK_SIZE-2] = 58; // ASCII code for :
  blockArray[LCD_BLOCK_SIZE-1] = (denominator % 10) + 48;
  format_display_block(blockArray, blockIndex, cursorState);
}

/**
 * Shows 999 if the value is greater than 999.
 * Shows 0 if it's less than 0.
 */
void int_to_display(int value, char blockArray[], int blockIndex, int cursorState){
  int number = value;
  if (number >= 999) {
    blockArray[LCD_BLOCK_SIZE-1] = 9 + 48;
    blockArray[LCD_BLOCK_SIZE-2] = 9 + 48;
    blockArray[LCD_BLOCK_SIZE-3] = 9 + 48;
  } else if (number <= 0) {
    // No support for negatives at the moment.
    blockArray[LCD_BLOCK_SIZE-1] = 48;
    blockArray[LCD_BLOCK_SIZE-2] = 32;
    blockArray[LCD_BLOCK_SIZE-3] = 32;
  } else {
    for(int i = 0; i < (LCD_BLOCK_SIZE-1); i++){
      blockArray[LCD_BLOCK_SIZE-1-i] = (number % 10) + 48; //offset by ascii zero for actual number
      number = number / 10;
    }
    if (value < 100) {
      blockArray[LCD_BLOCK_SIZE-3] = 32;      
    }
    if (value < 10) {
      blockArray[LCD_BLOCK_SIZE-2] = 32;
    }
  }  
  format_display_block(blockArray, blockIndex, cursorState);
}


void float_to_display(float value, char blockArray[], int blockIndex, int cursorState){
  float number = value;
  int truncNumber;
  
  if(number > 10.0){
    truncNumber = (int)number;
    
    for(int i = 0; i < (LCD_BLOCK_SIZE-1); i++){
      blockArray[LCD_BLOCK_SIZE-1-i] = truncNumber % 10 + 48;
      truncNumber = truncNumber / 10;
    }

    format_display_block(blockArray, blockIndex, cursorState);
  }
  else{
    number = number * 10.0;
    truncNumber = (int)number;
    
    blockArray[LCD_BLOCK_SIZE-1] = truncNumber % 10 + 48;
    blockArray[LCD_BLOCK_SIZE-2] = 46;
    truncNumber = truncNumber / 10;
    blockArray[LCD_BLOCK_SIZE-3] = truncNumber % 10 + 48; 

    if(cursorState){
      blockArray[0] = 62;
    }
    else{
      blockArray[0] = 32;
    }
  }
}

void display_start(){
  display.setCursor(4, 0);
  display.print("Start ?");  
}

void display_stop(){
  display.setCursor(5, 0);
  display.print("Stop ?");
}

/**
 * Run this often. It will blink the light according to the current state.
 * 
 * - Slow flashing in ready or stopped
 * - Fast flashing in calibration
 * - Solid in normal operation
 * - 
 * 
 * TODO when we have more user feedback hardware, also do alarm signals.
 */
void led_display(void) {
  uint8_t in_state = displayState.state;
  static uint8_t current = 0;
  // 2 is running
  if (in_state == 2 && current != 2) {
    digitalWrite(LED, HIGH);
  } else if (in_state == 0) {
    // Calibrating: blink fast.
    uint32_t ms = millis();
    if ((ms % 500) <= 250) {
      digitalWrite(LED, HIGH);
    } else {
      digitalWrite(LED, LOW);
    }
  } else if (in_state == 1) {
    // Ready: blink slowly.
    uint32_t ms = millis();
    if ((ms % 4000) <= 2000) {
      digitalWrite(LED, HIGH);
    } else {
      digitalWrite(LED, LOW);
    }
  } else if (in_state == 3 && current != 3) {
    // Stopped: show nothing
    digitalWrite(LED, LOW);
  }
  current = in_state;
}

/**
 * Set the values to display, but do not carry out the actual LCD update, as that is very expensive (see lcd_display).
 */
void lcd_control(uint8_t in_state, uint8_t in_mode, uint8_t in_bpm, uint32_t in_volume, uint32_t in_pressure, uint8_t in_ie_inhale, uint8_t in_ie_exhale){
  displayState.state = in_state;
  if(displayState.modeStateFlag){
    display.clear();
    if(displayState.modeState){
      display_start();  
    }
    else{
      display_stop();
    }
  } else {
    load_display(lcdDisplay, in_bpm, in_volume, in_pressure, in_ie_inhale, in_ie_exhale);
  }
}

// One pass every 20ms
#define LCD_DRAW_RATE 20000
// Blink once every half second
#define CURSOR_BLINK_RATE 500000

/**
 * Call this at every step. It will update at most one LCD block, so as to not take too much time from any
 * process cycle. Updating the whole display takes around 20ms which is unaccetptably slow, as it interferes
 * with the ventilator system logic and the motor step control loop.
 * 
 * FIXME there are a bunch of delay calls in the LiquidCrystal library. Why? Can't we make it non-blocking?
 */
void lcd_display(void) {
  static uint8_t block_index = 0;
  static uint8_t row = 0;
  static uint8_t col = 0;
  static uint8_t cursor_blink = 0;
  static uint32_t accumulator_us = 0;
  static uint32_t blink_accumulator_us = 0;
  static uint32_t last_draw = 0;
  uint32_t now = micros();
  uint32_t delta_us = now - last_draw;
  accumulator_us = accumulator_us + delta_us;
  blink_accumulator_us = blink_accumulator_us + delta_us;
  bool should_draw = accumulator_us >= LCD_DRAW_RATE;
  if (should_draw) {
    accumulator_us = (accumulator_us - LCD_DRAW_RATE) % LCD_DRAW_RATE;
    last_draw = now;
    if (block_index < 4) {
      row = 0;
      col = block_index * 4;
    } else {
      row = 1;
      col = (block_index * 4) - 16;
    }
    display_block(lcdDisplay[block_index].blockArray, row, col, cursor_blink, lcdDisplay[block_index].valueSet);
    block_index = (block_index + 1) % LCD_BLOCKS();
  }
  if (blink_accumulator_us >= CURSOR_BLINK_RATE) {
    cursor_blink = !cursor_blink;
    blink_accumulator_us = (blink_accumulator_us - CURSOR_BLINK_RATE) % CURSOR_BLINK_RATE;
  }
}
