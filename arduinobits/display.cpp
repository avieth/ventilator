#include <LiquidCrystal.h>
#include "display.h"

LiquidCrystal display(LCD_RS, LCD_EN, LCD_D4, LCD_D5, LCD_D6, LCD_D7);

/**
 * We use double buffering to draw the screen. Put what you want here, and
 * it will be periodically drawn without blocking the main thread too much.
 */
uint8_t buffer[LCD_SIZE()];

/**
 * The buffer is row-major. No overflow checks here: col and row are 0 indexed
 * and must be less than LCD_ROWS and LCD_COLUMNS.
 * Bad interface I know but whatever.
 */
uint8_t buffer_index(uint8_t col, uint8_t row) {
  return col + row * LCD_COLUMNS;
}

void display_setup(void) {
  pinMode(LED_GREEN, OUTPUT);
  pinMode(LED_RED, OUTPUT);
  pinMode(SPEAKER, OUTPUT);
  display_led_green(false);
  display_led_red(false);
  display_speaker(false);
  display.begin(LCD_COLUMNS, LCD_ROWS);
  for (uint8_t i = 0; i < LCD_SIZE(); ++i) {
    buffer[i] = ' ';
  }
}

/**
 * Set true to sound the speaker, false to turn it off.
 */
void display_speaker(bool on) {
  static bool current = false;
  if (on != current) {
    analogWrite(SPEAKER, on ? 255 : 0);
    current = on;
  }
}

/**
 * Set the green LED state (true for on).
 * This is instantaneous, unlike the LCD display which is buffered.
 */
void display_led_green(bool on) {
  static bool current = false;
  if (on != current) {
    digitalWrite(LED_GREEN, on ? HIGH : LOW);
    current = on;
  }
}

void display_led_green_flashing(uint32_t now_us, uint32_t interval_us) {
  static uint32_t last_us = 0;
  static bool current = false;
  if ((now_us - last_us) >= interval_us) {
    display_led_green(!current);
    current = !current;
    last_us = now_us;
  }
}

/**
 * Set the red LED state (true for on).
 * This is instantaneous, unlike the LCD display which is buffered.
 */
void display_led_red(bool on) {
  static bool current = false;
  if (on != current) {
    digitalWrite(LED_RED, on ? HIGH : LOW);
    current = on;
  }
}

void display_clear() {
  display_clear(0, 0, LCD_SIZE());
}

void display_clear(uint8_t col, uint8_t row, uint8_t size) {
  uint8_t idx = buffer_index(col, row);
  for (uint8_t i = 0; i < size; i++) {
    buffer[idx+i] = ' ';
  }
}

/**
 * Write a single thing at a single cell. You probably want to use more
 * semantically-rich variants like display_string or display_uint32.
 */
void display_write(uint8_t col, uint8_t row, uint8_t c) {
  buffer[buffer_index(col, row)] = c;
}

/**
 * Display an unsigned number at a given point in a given number of decimal
 * digits. No leading zeros are shown. It will wrap if you go over the right
 * hand side.
 */
void display_uint32(uint8_t col, uint8_t row, uint8_t size, uint32_t n) {
  display_clear(col, row, size);
  uint8_t c = col + (size - 1);
  uint32_t m = n;
  uint8_t digit;
  uint8_t idx = buffer_index(c, row);
  for (uint8_t i = 0; i < size; ++i) {
    digit = (m % 10) + 48;
    buffer[idx] = digit;
    m = m / 10;
    if (m == 0) {
      break;
    }
    idx = idx - 1;
  }
}

/**
 * Display a signed number at a given point in a given number of decimal
 * digits. No leading zeros are shown. It will wrap if you go over the right
 * hand side. The sign always takes a place, even if it's not shown because
 * the number is positive.
 */
void display_int32(uint8_t col, uint8_t row, uint8_t size, int32_t n) {
  display_uint32(col+1, row, size-1, (uint32_t) abs(n));
  if (n < 0) {
    buffer[buffer_index(col, row)] = '-';
  } else {
    buffer[buffer_index(col, row)] = ' ';
  }
}

/**
 * Display a string left-to-right.
 */
void display_string(uint8_t col, uint8_t row, uint8_t size, char str[]) {
  uint8_t idx = buffer_index(col, row);
  for (uint8_t i; i < size; i++) {
    buffer[idx] = str[i];
    idx = idx + 1;
  }
}

/**
 * Display a ratio. No size needed because it's always 3. The numerator and
 * denominator must be single digits (will be taken modulo 10 anyway).
 */
void display_ratio(uint8_t col, uint8_t row, uint8_t num, uint8_t den) {
  uint8_t idx = buffer_index(col, row);
  buffer[idx] = (num % 10) + 48;
  buffer[idx+1] = '/';
  buffer[idx+2] = (den % 10) + 48;
}

/**
 * To be called as often as possible with the current time in microseconds.
 */
void display_draw(uint32_t now_us) {
  static uint8_t col = 0;
  static uint8_t row = 0;
  static uint8_t index = 0;
  static uint32_t accumulator_us = 0;
  static uint32_t last_us = 0;
  uint32_t delta_us = now_us - last_us;
  if (now_us < last_us) {
    // Could happen due to overflow. No big deal it will catch up eventually.
    delta_us = now_us;
  }
  last_us = now_us;
  accumulator_us += delta_us;
  if (accumulator_us < DRAW_RATE_LIMIT_US) {
    return;
  }
  // Forget about carry-forward and missed frames, no big deal since it's just
  // the display.
  accumulator_us = 0;
  // Draw the current row and column.
  display.setCursor(col, row);
  display.write(buffer[buffer_index(col, row)]);
  col++;
  if (col == LCD_COLUMNS) {
    col = 0;
    row++;
  }
  if (row == LCD_ROWS) {
    row = 0;
  }
}
