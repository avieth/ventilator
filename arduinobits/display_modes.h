#include <Arduino.h>

#define DISPLAY_STATE_CALIBRATING 0x00
#define DISPLAY_STATE_READY       0x01
#define DISPLAY_STATE_RUNNING     0x02
#define DISPLAY_STATE_STOPPED     0x03
#define DISPLAY_STATE_RESETTING   0x04
#define DISPLAY_STATE_ERROR       0x80

/**
 * Info to display in "running" state.
 */
typedef struct displayData {
  uint8_t state;
  uint8_t mode;
  uint8_t bpm;
  uint8_t ieInhale;
  uint8_t ieExhale;
  uint32_t tidalVolume;
  uint32_t volumeLimit;
  uint32_t pressurePeak;
  uint32_t pressureLimit;
  uint32_t peep;
  uint32_t oxygen;
} displayData;

/**
 * Targets for on-screen parameter changes.
 * Use pointers to data that will be used by the program business logic.
 */
typedef struct displayWriteData {
  uint8_t *mode;
  uint8_t *bpm;
  uint8_t *ieInhale;
  uint8_t *ieExhale;
  uint32_t *volume;
  uint32_t *pressure;
} displayWriteData;

/**
 * Callbacks for changing display data on a particular index.
 */
typedef struct displayDataInput {
  void (*highlight)(bool);
  void (*select)(displayData*, displayWriteData*);
  void (*change)(int32_t);
  /**
   * Display the changed value without committing it.
   */
  void (*overlay)(void);
  void (*deselect)(bool, displayWriteData*);
} displayDataInput;

displayDataInput* get_display_data(uint8_t index);
displayDataInput* next_display_data(bool direction);
void display_format_running(displayData *data);
