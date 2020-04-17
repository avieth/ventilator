#include <Arduino.h>

#define LCD_COLUMNS       20
#define LCD_ROWS          4
#define LCD_SIZE()        (LCD_COLUMNS*LCD_ROWS)

#define LCD_RS                14                        
#define LCD_EN                15
#define LCD_D4                2
#define LCD_D5                3
#define LCD_D6                4
#define LCD_D7                5

#define LED_GREEN             16
#define LED_RED               A5
// The speaker counts as part of the display
#define SPEAKER               A6

// Will not draw more than once per this interval in microseconds.
#define DRAW_RATE_LIMIT_US    1000

void display_setup(void);
void display_speaker(bool on);
void display_led_green(bool on);
void display_led_green_flashing(uint32_t now_us, uint32_t interval_us);
void display_led_red(bool on);
void display_led_red_flashing(uint32_t now_us, uint32_t interval_us);
void display_clear(uint8_t col, uint8_t row, uint8_t size);
void display_clear();
void display_write(uint8_t col, uint8_t row, uint8_t c);
void display_uint32(uint8_t col, uint8_t row, uint8_t size, uint32_t n);
void display_int32(uint8_t col, uint8_t row, uint8_t size, int32_t n);
void display_string(uint8_t col, uint8_t row, uint8_t size, char str[]);
void display_ratio(uint8_t col, uint8_t row, uint8_t num, uint8_t den);
void display_draw(uint32_t now_us);
