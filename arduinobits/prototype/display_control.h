#include <LiquidCrystal.h> 
#include "input_control.h"

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

LiquidCrystal display(LCD_RS, LCD_EN, LCD_D4, LCD_D5, LCD_D6, LCD_D7);



typedef struct block{
  char blockArray[LCD_BLOCK_SIZE];
  int valueSet;
}displayBlock; 

displayBlock lcdDisplay[LCD_BLOCKS()];


void display_setup();
void load_display(displayBlock lcd[], recordings *information);
void display_values(displayBlock lcd[]);
void display_block(char blockArray[], int row, int column, int cursorBlink, int valueSet);
void format_display_block(char blockArray[], int blockIndex, int cursorState);
void int_to_display(int value, char blockArray[], int blockIndex, int cursorState);
void float_to_display(float value, char blockArray[], int blockIndex, int cursorState);
void display_start();
void display_stop();
void lcd_control(long currentTime, recordings *values);




// setup display to intended columns and rows  
void display_setup(){
  display.begin(MAX_LCD_COLUMNS, MAX_LCD_ROWS);
}



void load_display(displayBlock lcd[], recordings *information){
  int index = 0;
  int cursorState = 0;
  
  cursorState = (information->cursorActive && information->cursorIndex == 0) ? 1 : 0;
  lcd[index].valueSet = (information->valueSet == 0 && cursorState) ? 0 : 1;
  int_to_display(information->breathRate, lcd[index].blockArray, index, cursorState);
  index++;
  
  cursorState = (information->cursorActive && information->cursorIndex == 1) ? 1 : 0;
  lcd[index].valueSet = (information->valueSet == 0 && cursorState) ? 0 : 1;
  int_to_display(information->tidalVolume, lcd[index].blockArray, index, cursorState);
  index++;
  
  cursorState = (information->cursorActive && information->cursorIndex == 2) ? 1 : 0;
  lcd[index].valueSet = (information->valueSet == 0 && cursorState) ? 0 : 1;
  float_to_display(information->pPeak, lcd[index].blockArray, index, cursorState);
  index++;
  
  cursorState = (information->cursorActive && information->cursorIndex == 3) ? 1 : 0;
  lcd[index].valueSet = (information->valueSet == 0 && cursorState) ? 0 : 1;
  float_to_display(information->ieRatio, lcd[index].blockArray, index, cursorState);
  index++;
  
  cursorState = (information->cursorActive && information->cursorIndex == 4) ? 1 : 0;
  lcd[index].valueSet = (information->valueSet == 0 && cursorState) ? 0 : 1;
  int_to_display(information->FiO2, lcd[index].blockArray, index, cursorState);
  index++;
  
  cursorState = (information->cursorActive && information->cursorIndex == 5) ? 1 : 0;
  lcd[index].valueSet = (information->valueSet == 0 && cursorState) ? 0 : 1;
  int_to_display(information->vte, lcd[index].blockArray, index, cursorState);
  index++;
  
  cursorState = (information->cursorActive && information->cursorIndex == 6) ? 1 : 0;
  lcd[index].valueSet = (information->valueSet == 0 && cursorState) ? 0 : 1;
  float_to_display(information->pMean, lcd[index].blockArray, index, cursorState);
  index++;
  
  cursorState = (information->cursorActive && information->cursorIndex == 7) ? 1 : 0;
  lcd[index].valueSet = (information->valueSet == 0 && cursorState) ? 0 : 1;
  float_to_display(information->peep, lcd[index].blockArray, index, cursorState);
}



void display_values(displayBlock lcd[]){
  static int cursorBlink = 0;
  int row;
  int column;
  
  for(int i = 0; i < LCD_BLOCKS(); i++){
    if(i < 4){
      row = 0;
      column = i * 4;
    }
    else{
      row = 1;
      column = (i * 4) - 16; 
    }
    display_block(lcd[i].blockArray, row, column, cursorBlink, lcd[i].valueSet);
  }
  
  cursorBlink = !cursorBlink;
}


void display_block(char blockArray[], int row, int column, int cursorBlink, int valueSet){
  display.setCursor(column, row);
  
  if(cursorBlink){
    display.write(32);
    
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
  int shiftCount = 0;
  
  for(int i  = 1; i < LCD_BLOCK_SIZE; i++){
    if(blockArray[i] == 48){ //check how many previous zeros there are
      shiftCount++;
    }
    else{
      break;
    }
  }
  
  
  for(int i = 0; i < shiftCount; i++){
    if(blockIndex % 2 == 0){
      for(int j = 0; j < LCD_BLOCK_SIZE-1; j++){
        blockArray[j] = blockArray[j+1];
      }
    }
  }
  
  
  for(int i = 0; i < shiftCount; i++){
    if(blockIndex % 2 == 0){
      blockArray[LCD_BLOCK_SIZE-1-i] = 32;
    }
    else{
      blockArray[i+1] = 32;
    }
  }

  if(shiftCount >= LCD_BLOCK_SIZE-1){
      blockArray[LCD_BLOCK_SIZE-1] = 48;
  }
  
  if(cursorState){
    blockArray[0] = 62;
  }
  else{
    blockArray[0] = 32;
  }
}


void int_to_display(int value, char blockArray[], int blockIndex, int cursorState){
  int number = value;
  
  for(int i = 0; i < (LCD_BLOCK_SIZE-1); i++){
    blockArray[LCD_BLOCK_SIZE-1-i] = (number % 10) + 48; //offset by ascii zero for actual number
    number = number / 10;
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





// main lcd control, updates info and displays, refresh rate set at 2.5Hz 
void lcd_control(long currentTime, recordings *values){
  static long lastDisplayEvent = 0;
  
  if(currentTime - lastDisplayEvent >= DISPLAY_CONTROL_RATE){
    lastDisplayEvent = currentTime;

    if(values->startStopStateFlag){
      display.clear();
      
      if(values->startStopState){
        display_start();  
      }
      else{
        display_stop();
      }
    }
    else{
      load_display(lcdDisplay, values);
      display_values(lcdDisplay);
    }
  }

  if(lastDisplayEvent - currentTime > DISPLAY_CONTROL_RATE){
    lastDisplayEvent = 0;
  }
}
