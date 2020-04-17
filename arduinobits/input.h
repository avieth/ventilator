// TODO clear out unused defines. Many are not even relevant to the input
// control.
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

#define KNOB_ENC_A                    A10
#define KNOB_ENC_B                    A11

#define BUTTON_MAIN                   A7
#define BUTTON_RIGHT                  A8
#define BUTTON_LEFT                   A9

typedef struct inputCallbacks {
  void (*buttonLeft)(bool);
  void (*buttonRight)(bool);
  void (*buttonMain)(bool);
  /**
   * Positive value means clockwise rotation.
   */
  void (*encoderPosition)(int32_t);
} inputCallbacks;

void input_setup(inputCallbacks cbs);
void input_poll(uint32_t now_us);
