#include "pins.h"
#include "pressure_sensors.h"
#include "median.h"

#define SENSOR_COUNT 4
/**
 * Indices in the offset and pin arrays.
 */
#define INSPPRESSURE 0
#define INSPFLOW 1
#define EXPFLOW 2
#define AIRINFLOW 3

uint16_t sensor_offsets[SENSOR_COUNT] = { 0, 0, 0, 0 };
uint8_t sensor_pins[SENSOR_COUNT] = { 
  PIN_INSP_PRESSURE,
  PIN_INSP_FLOW,
  PIN_EXP_FLOW,
  PIN_AIR_IN_FLOW
};

static struct median_16 insp_pressure_median = {
  .value = 0,
  .index = 0,
  .observation_order = { 0x00 },
  .magnitude_order = { 0x00 }
};
static struct median_16 insp_flow_median = {
  .value = 0,
  .index = 0,
  .observation_order = { 0x00 },
  .magnitude_order = { 0x00 }
};
static struct median_16 exp_flow_median = {
  .value = 0,
  .index = 0,
  .observation_order = { 0x00 },
  .magnitude_order = { 0x00 }
};
static struct median_16 air_in_flow_median = {
  .value = 0,
  .index = 0,
  .observation_order = { 0x00 },
  .magnitude_order = { 0x00 }
};

struct median_16 *sensor_medians[SENSOR_COUNT] = {
  &insp_pressure_median,
  &insp_flow_median,
  &exp_flow_median,
  &air_in_flow_median
};

/**
 * Only call with one of the defined sensor names above.
 * You get 0 otherwise.
 * 
 * The readings are corrected for the offset of that sensor (determined
 * by initializeSensors. If the reading is below the offset, you get a
 * negative number.
 */
uint16_t read_sensor_with_offset(uint8_t sensor_id) {
  if (sensor_id < SENSOR_COUNT) {
    uint16_t reading = analogRead(sensor_pins[sensor_id]);
    uint16_t offset = sensor_offsets[sensor_id];
    uint16_t corrected = (reading < offset) ? 0 : reading - offset;
    return corrected;
  } else {
    return 0;
  }
}

/**
 * Update the filter for this sensor, and return the filtered value.
 * Use the output of read_sensor_with_offset for the same sensor id.
 *
 * Gives 0 if the sensor id is invalid.
 *
 * Should call this regularly for all sensors, to keep the filters up to
 * date.
 * TODO could include timestamps in the filters.
 */
uint16_t get_update_filter(uint8_t sensor_id, uint16_t value) {
  if (sensor_id < SENSOR_COUNT) {
    median_16_step(sensor_medians[sensor_id], value);
    return sensor_medians[sensor_id]->value;
  } else {
    return 0;
  }
}

/**
 * This is 0.6x + 0.3y + 0.2z but without nasty fractional computation.
 */
int32_t smooth(int32_t x, int32_t y, int32_t z) {
  return (6*x + 3*y + 2*z) / 10;
}

//INITIALIZATION 

/**
 * Set the read resolution to 12 bits, set the analog reference to 3.3
 *
 * TODO can we detect when the offset readings are abnormally high? What
 * qualifies as too high? Probably 256 or greater I'd imagine...
 */
void initializeSensors(){
  /* DUE only supports 3.3V. Put this call here just to be sure: if you try
   * to build this for a non-DUE you will probably get an error because
   * AR_DEFAULT is not defined
   */
  analogReference(AR_DEFAULT);
  analogReadResolution(12);
  sensor_offsets[INSPPRESSURE] = sensor_offset(sensor_pins[INSPPRESSURE]);
  sensor_offsets[INSPFLOW]     = sensor_offset(sensor_pins[INSPFLOW]);
  sensor_offsets[EXPFLOW]      = sensor_offset(sensor_pins[EXPFLOW]);
  sensor_offsets[AIRINFLOW]    = sensor_offset(sensor_pins[AIRINFLOW]);
}

//SENSORS

/**
 * Inspiration pressure at this instant.
 * TODO make it integral.
 * TODO delete, we have an integral version.
 */
float get_insp_pressure() {
  uint16_t rawData = read_sensor_with_offset(INSPPRESSURE);
  uint16_t filtered = get_update_filter(INSPPRESSURE, rawData);
  return pressure_difference(filtered);
}

/**
 * Inspiration flow at this instant.
 * TODO make it integral.
 */
float get_insp_flow() {
  uint16_t rawData = read_sensor_with_offset(INSPFLOW);
  uint16_t filtered = get_update_filter(INSPFLOW, rawData);
  return flow_rate(filtered);
}

/**
 * Expiration flow at this instant.
 * TODO make it integral.
 */
float get_exp_flow() {
  uint16_t rawData = read_sensor_with_offset(EXPFLOW);
  uint16_t filtered = get_update_filter(EXPFLOW, rawData);
  return flow_rate(filtered);
}

/**
 * Air in flow at this instant.
 * TODO make it integral
 */
float get_air_in_flow() {
  uint16_t rawData = read_sensor_with_offset(AIRINFLOW);
  uint16_t filtered = get_update_filter(AIRINFLOW, rawData);
  return flow_rate(filtered);
}

//COMPUTED VOLUMES

/*bool isInspFlow = false;
float inspSensedVolume = 0.0f; 


float getInspSensedVolume(long sensorLoopTime){

  float f_sensorLoopTime = sensorLoopTime/1000000.0f; 

  if(isInspFlow){
    inspSensedVolume = inspSensedVolume + getInspFlow()*f_sensorLoopTime;
    
  }else{
    inspSensedVolume = 0.0f; 
  }

  
  return inspSensedVolume; 
}*/

/*bool isExpFlow = false;
float expSensedVolume = 0.0f; 

float getExpSensedVolume(long sensorLoopTime){
  
  float f_sensorLoopTime = sensorLoopTime/1000000.0f; 

  if(isExpFlow){
    expSensedVolume = expSensedVolume + getExpFlow()*f_sensorLoopTime;
    
  }else{
    expSensedVolume = 0.0f; 
  }

  
  return expSensedVolume; 
}*/


/*bool isAirInFlow = false; 
float airInSensedVolume = 0.0f; 

float getAirInSensedVolume(long sensorLoopTime){
  
  float f_sensorLoopTime = sensorLoopTime/1000000.0f; 

  if(isAirInFlow){
    airInSensedVolume = airInSensedVolume + getAirInFlow()*f_sensorLoopTime;
    
  }else{
    airInSensedVolume = 0.0f; 
  }

  
  return airInSensedVolume; 
}*/


//CONVERSIONS


float volumeToCubicMeters(float volume) {
  float convertedVolume = volume / 1000000.0f;
  return convertedVolume;
}

float pressureToPascals(float pressure) {
  float convertedPressure = pressure * 1.0f;
  return convertedPressure;
}
