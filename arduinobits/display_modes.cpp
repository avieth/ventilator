#include "display.h"
#include "display_modes.h"

uint8_t mode_selected = 0;
void mode_highlight(bool x) {
  if (x) {
    display_write(0, 1, '[');
    display_write(8, 1, ']');
  } else {
    display_write(0, 1, '>');
  }
}
void mode_select(displayData *data, displayWriteData *dwd) {
  mode_selected = *(dwd->mode);
}
void mode_change(int32_t x) {
  if (x > 0) {
    mode_selected = 1; // SIMV
  } else if (x < 0) {
    mode_selected = 0; // CMV
  }
}
void mode_overlay() {
  if (mode_selected == 0) {
    display_string(4, 1, 4, "CMV ");
  } else if (mode_selected == 1) {
    display_string(4, 1, 4, "SIMV");
  } else {
    display_string(4, 1, 4, "????");
  }
}
void mode_deselect(bool commit, displayWriteData *dwd) {
  if (commit) {
    *(dwd->mode) = mode_selected;
  }
}
displayDataInput ddiMode = {
  .highlight = mode_highlight,
  .select = mode_select,
  .change = mode_change,
  .overlay = mode_overlay,
  .deselect = mode_deselect
};

uint8_t bpm_selected = 0;
void bpm_highlight(bool x) {
  if (x) {
    display_write(0, 2, '[');
    display_write(8, 2, ']');
  } else {
    display_write(0, 2, '>');
  }
}
void bpm_select(displayData *data, displayWriteData *dwd) {
  bpm_selected = *(dwd->bpm);
}
void bpm_change(int32_t x) {
  if (x > 0) {
    bpm_selected += 1;
  } else if (x < 0) {
    bpm_selected -= 1;
  }
  if (bpm_selected > 60) {
    bpm_selected = 60;
  } else if (bpm_selected < 6) {
    bpm_selected = 6;
  }
}
void bpm_overlay(void) {
  display_uint32(4, 2, 3, (uint32_t) bpm_selected);
}
void bpm_deselect(bool commit, displayWriteData *dwd) {
  if (commit) {
    *(dwd->bpm) = bpm_selected;
  }
}
displayDataInput ddiBpm = {
  .highlight = bpm_highlight,
  .select = bpm_select,
  .change = bpm_change,
  .overlay = bpm_overlay,
  .deselect = bpm_deselect
};

uint8_t ieInhale_selected = 0;
uint8_t ieExhale_selected = 0;
void ie_highlight(bool x) {
  if (x) {
    display_write(0, 3, '[');
    display_write(8, 3, ']');
  } else {
    display_write(0, 3, '>');
  }
}
void ie_select(displayData *data, displayWriteData *dwd) {
  ieInhale_selected = *(dwd->ieInhale);
  ieExhale_selected = *(dwd->ieExhale);
}
void ie_change(int32_t x) {
  if (ieInhale_selected == 1 && ieExhale_selected == 1 && x < 0) {
    ieInhale_selected = 2;
    ieExhale_selected = 3;
  } else if (ieInhale_selected == 2 && ieExhale_selected == 3) {
    if (x > 0) {
      ieInhale_selected = 1;
      ieExhale_selected = 1;
    } else if (x < 0) {
      ieInhale_selected = 1;
      ieExhale_selected = 2;
    }
  } else if (ieInhale_selected == 1 && ieExhale_selected == 2) {
    if (x > 0) {
      ieInhale_selected = 2;
      ieExhale_selected = 3;
    } else if (x < 0) {
      ieInhale_selected = 1;
      ieExhale_selected = 3;
    }
  } else if (ieInhale_selected == 1 && ieExhale_selected == 3) {
    if (x > 0) {
      ieInhale_selected = 1;
      ieExhale_selected = 2;
    } else if (x < 0) {
      ieInhale_selected = 1;
      ieExhale_selected = 4;
    }
  } else if (ieInhale_selected == 1 && ieExhale_selected == 4 && x > 0) {
    ieInhale_selected = 1;
    ieExhale_selected = 3;
  } else if (x < 0) {
    ieInhale_selected = 1;
    ieExhale_selected = 4;
  } else if (x > 0) {
    ieInhale_selected = 1;
    ieExhale_selected = 1;
  }
}
void ie_overlay(void) {
  display_ratio(4, 3, ieInhale_selected, ieExhale_selected);
}
void ie_deselect(bool commit, displayWriteData *dwd) {
  if (commit) {
    *(dwd->ieInhale) = ieInhale_selected;
    *(dwd->ieExhale) = ieExhale_selected;
  }
}
displayDataInput ddiIE = {
  .highlight = ie_highlight,
  .select = ie_select,
  .change = ie_change,
  .overlay = ie_overlay,
  .deselect = ie_deselect
};

// In mL.
uint32_t volume_selected = 0;
void volume_highlight(bool x) {
  if (x) {
    display_write(8, 0, '[');
    display_write(19, 0, ']');
  } else {
    display_write(8, 0, '>');
  }
}
void volume_select(displayData *data, displayWriteData *dwd) {
  volume_selected = *(dwd->volume);
}
void volume_change(int32_t x) {
  if (x > 0) {
    volume_selected += 10;
  } else if (x < 0) {
    volume_selected -= 10;
  }
  if (volume_selected < 0) {
    volume_selected = 0;
  } else if (volume_selected > 5000) {
    volume_selected = 5000;
  }
}
void volume_overlay(void) {
  display_uint32(15, 0, 4, volume_selected);
}
void volume_deselect(bool commit, displayWriteData *dwd) {
  if (commit) {
    *(dwd->volume) = volume_selected;
  }
}
displayDataInput ddiVolume = {
  .highlight = volume_highlight,
  .select = volume_select,
  .change = volume_change,
  .overlay = volume_overlay,
  .deselect = volume_deselect
};

// Pa
uint32_t pressure_selected = 0;
void pressure_highlight(bool x) {
  if (x) {
    display_write(8, 1, '[');
    display_write(19, 1, ']');
  } else {
    display_write(8, 1, '>');
  }
}
void pressure_select(displayData *data, displayWriteData *dwd) {
  pressure_selected = *(dwd->pressure);
}
void pressure_change(int32_t x) {
  if (x > 0) {
    pressure_selected += 100;
  } else if (x < 0) {
    pressure_selected -= 100;
  }
  if (pressure_selected > 40000) {
    pressure_selected = 40000;
  } else if (pressure_selected < 0) {
    pressure_selected = 0;
  }
}
void pressure_overlay(void) {
  display_uint32(15, 1, 4, pressure_selected / 98);
}
void pressure_deselect(bool commit, displayWriteData *dwd) {
  if (commit) {
    *(dwd->pressure) = pressure_selected;
  }
}
displayDataInput ddiPressure = {
  .highlight = pressure_highlight,
  .select = pressure_select,
  .change = pressure_change,
  .overlay = pressure_overlay,
  .deselect = pressure_deselect
};

uint32_t peep_selected = 0;
void peep_highlight(bool x) {
  if (x) {
    display_write(8, 2, '[');
    display_write(19, 2, ']');
  } else {
    display_write(8, 2, '>');
  }
}
void peep_select(displayData *data, displayWriteData *dwd) {
}
void peep_change(int32_t x) {
}
void peep_overlay(void) {
}
void peep_deselect(bool commit, displayWriteData *dwd) {
}
displayDataInput ddiPeep = {
  .highlight = peep_highlight,
  .select = peep_select,
  .change = peep_change,
  .overlay = peep_overlay,
  .deselect = peep_deselect
};

uint32_t oxygen_selected = 0;
void oxygen_highlight(bool x) {
  if (x) {
    display_write(8, 3, '[');
    display_write(19, 3, ']');
  } else {
    display_write(8, 3, '>');
  }
}
void oxygen_select(displayData *data, displayWriteData *dwd) {
}
void oxygen_change(int32_t x) {
}
void oxygen_overlay(void) {
}
void oxygen_deselect(bool commit, displayWriteData *dwd) {
}
displayDataInput ddiOxygen = {
  .highlight = oxygen_highlight,
  .select = oxygen_select,
  .change = oxygen_change,
  .overlay = oxygen_overlay,
  .deselect = oxygen_deselect
};

/**
 * Get the display data at a given undex, which will be taken modulo the
 * array lenght (7).
 *Order is key: must correspond to the display ordering or else it will
 * give a weird confusing UX.
 */
displayDataInput* get_display_data(uint8_t idx) {
  static displayDataInput* ddis[7] = {
    &ddiMode,
    &ddiBpm,
    &ddiIE,
    &ddiVolume,
    &ddiPressure,
    &ddiPeep,
    &ddiOxygen
  };
  return ddis[idx % 7];
}

/**
 * Some external UI claims that it needs to know the index of the field being
 * edited on screen on the device. Seems weird to me but oh well, I'm not
 * asking questions about it. Anyway that's why we have this top-level
 * index variable.
 */
uint8_t display_data_index = 0;

/**
 * Get the next display data input. The bool indicates direction. An array of
 * the 7 display data inputs is traversed in a circular fashion.
 */
displayDataInput* next_display_data(bool direction) {
  static uint8_t idx = 0;
  if (direction) {
    display_data_index = (display_data_index + 1) % 7;
  } else {
    display_data_index = (display_data_index == 0) ? 6 : (display_data_index - 1);
  }
  return get_display_data(display_data_index);
}

uint8_t current_display_data_index(void) {
  return display_data_index;
}

/**
 * Formats the display buffer for a displayData
 */
void display_format_running(displayData *data) {
  display_clear();
  /* Header */
  if (data->state == DISPLAY_STATE_CALIBRATING) {
    display_string(0, 0, 7, "CALIB  ");
  } else if (data->state == DISPLAY_STATE_READY) {
    display_string(0, 0, 7, "READY  ");
  } else if (data->state == DISPLAY_STATE_RUNNING) {
    display_string(0, 0, 7, "RUNNING");
  } else if (data->state == DISPLAY_STATE_STOPPED) {
    display_string(0, 0, 7, "STOPPED");
  } else if (data->state == DISPLAY_STATE_RESETTING) {
    display_string(0, 0, 7, "RESET  ");
  } else if (data->state == DISPLAY_STATE_ERROR) {
    display_string(0, 0, 7, "ERROR  ");
  }
  /* Mode */
  display_string(1, 1, 3, "Mo:");
  if (data->mode == 0) {
    display_string(4, 1, 4, "CMV ");
  } else if (data->mode == 1) {
    display_string(4, 1, 4, "SIMV");
  } else {
    display_string(4, 1, 4, "????");
  }
  /* BPM */
  display_string(1, 2, 3, "Hz:");
  display_uint32(4, 2, 3, (uint32_t) data->bpm);
  /* IE ratio */
  display_string(1, 3, 3, "IE:");
  display_ratio(4, 3, data->ieInhale, data->ieExhale);
  /* Right side column */
  /* Tidal volume */
  // TODO FIXME in ready mode, show goal; in running mode, show sensed.
  // How to implement? Need to records in the data.
  display_string(9, 0, 6, "mL   :");
  display_uint32(15, 0, 4, (data->state == DISPLAY_STATE_RUNNING) ? data->tidalVolume : data->volumeLimit);
  /* Pressure (peak) */
  display_string(9, 1, 6, "cmH2O:");
  display_uint32(15, 1, 4, ((data->state == DISPLAY_STATE_RUNNING) ? data->pressurePeak : data->pressureLimit) / 98);
  /* PEEP */
  display_string(9, 2, 6, "PEEP :");
  display_uint32(15, 2, 4, data->peep);
  /* Oxygen concentration */
  display_string(9, 3, 6, "FiO2 :");
  display_uint32(15, 3, 4, data->oxygen);
}
