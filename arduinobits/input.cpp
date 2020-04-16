#include <Arduino.h>
#include <Encoder.h>
#include "input.h"

#define BUTTON_DEBOUNCE 25000
#define INPUT_POLL_RATE 1000

void (*buttonLeft)(bool);
void (*buttonRight)(bool);
void (*buttonMain)(bool);
void (*encoderPosition)(int32_t);

Encoder *enc;

/**
 * Set up all of the pins/interrupts with their callbacks.
 */
void input_setup(inputCallbacks cbs) {
  // Set up callbacks and then install interrupts.
  buttonLeft = cbs.buttonLeft;
  buttonRight = cbs.buttonRight;
  buttonMain = cbs.buttonMain;
  encoderPosition = cbs.encoderPosition;

  pinMode(BUTTON_LEFT,  INPUT);
  pinMode(BUTTON_RIGHT, INPUT);
  pinMode(BUTTON_MAIN,  INPUT);
  /*
   * Interrupt on the RIGHT button bugs out with the LCD.
   */
  //attachInterrupt(digitalPinToInterrupt(BUTTON_LEFT),  isrButtonLeft,  RISING);
  //attachInterrupt(digitalPinToInterrupt(BUTTON_RIGHT), isrButtonRight, RISING);
  //attachInterrupt(digitalPinToInterrupt(BUTTON_MAIN),  isrButtonMain,  RISING);

  enc = new Encoder(KNOB_ENC_B, KNOB_ENC_A);
  enc->write(0);
}

void button_left() {
  static bool current = false;
  static uint32_t timeout_us = 0;
  static uint32_t last_us = 0;
  uint32_t now_us = micros();
  uint32_t delta_us = now_us - last_us;
  last_us = now_us;
  if (timeout_us == 0) {
    bool pressed = digitalRead(BUTTON_LEFT) == HIGH;
    if ((pressed && !current) || (!pressed && current)) {
      buttonLeft(pressed);
      current = pressed;
      timeout_us = BUTTON_DEBOUNCE;
    }
  } else if (delta_us > timeout_us) {
    timeout_us = 0;
  } else {
    timeout_us = timeout_us - delta_us;
  }
}

void button_right() {
  static bool current = false;
  static uint32_t timeout_us = 0;
  static uint32_t last_us = 0;
  uint32_t now_us = micros();
  uint32_t delta_us = now_us - last_us;
  last_us = now_us;
  if (timeout_us == 0) {
    bool pressed = digitalRead(BUTTON_RIGHT) == HIGH;
    if ((pressed && !current) || (!pressed && current)) {
      buttonRight(pressed);
      current = pressed;
      timeout_us = BUTTON_DEBOUNCE;
    }
  } else if (delta_us > timeout_us) {
    timeout_us = 0;
  } else {
    timeout_us = timeout_us - delta_us;
  }
}

void button_main() {
  static bool current = false;
  static uint32_t timeout_us = 0;
  static uint32_t last_us = 0;
  uint32_t now_us = micros();
  uint32_t delta_us = now_us - last_us;
  last_us = now_us;
  if (timeout_us == 0) {
    bool pressed = digitalRead(BUTTON_MAIN) == HIGH;
    if ((pressed && !current) || (!pressed && current)) {
      buttonMain(pressed);
      current = pressed;
      timeout_us = BUTTON_DEBOUNCE;
    }
  } else if (delta_us > timeout_us) {
    timeout_us = 0;
  } else {
    timeout_us = timeout_us - delta_us;
  }
}

/**
 * Must call this frequently since we have to poll the encoder.
 * TODO how often? Currently 1KHz
 */
void input_poll(uint32_t now_us) {
  static uint32_t accumulator_us = 0;
  static uint32_t last_us = 0;
  uint32_t delta_us = now_us - last_us;
  accumulator_us += delta_us;
  if (accumulator_us >= INPUT_POLL_RATE) {
    int32_t ev = enc->read();
    // One tick is 4 pulses.
    // We want clockwise to be positive so we negate.
    encoderPosition(-ev / 4);
    button_left();
    button_right();
    button_main();
    accumulator_us = 0;
  }
  last_us = now_us;
}
