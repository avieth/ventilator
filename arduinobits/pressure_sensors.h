/*
   20200328
   author: Colin Gallacher
   v0.1

*/


#define AREA1                0.0003976
#define AREA2                0.000044156
#define AIR_DENSITY          1.225
#define DENOMINATOR()        (((AREA1/AREA2) * (AREA1/AREA2)) - 1)

uint16_t sensor_offset(uint8_t analogPin);
uint16_t read_sensor(uint8_t pin);
float pressure_difference(uint16_t measurement);
float flow_rate(uint16_t measurement);

/**
 * Reads a pin 10 times, takes the integral average plus 1, to give a baseline reading.
 */
uint16_t sensor_offset(uint8_t analogPin){
  // int is wide enough, since we're only summing 10 measurements in [0,1024)
  // But oh well use 32 bits anyway.
  uint32_t sum = 0;
  
  for(int i = 0; i < 60; i++){ //blocking for initialization 
    sum = sum + (uint32_t) (analogRead(analogPin));
  }
  sum = sum / 60;
  sum += 4;
  
  return sum;
}

/**
 * Read the sensor on a given pin. Just a simple analogRead. No adjustment is done.
 * Resolution is hardware dependent. 10 bits on the arduino uno.
 */
uint16_t read_sensor(uint8_t pin){
  return analogRead(pin); 
}

/**
 * The measurement comes from a sensor pin and is assumed to already have been corrected for signal offset.
 * 
 * TODO do integral computation. Values of magnitude less than 0.01 are discarded anyway.
 * Given that the measurement is in [0,1024), we have a lot of room to do non-floating-point arithmetic.
 * Instead of multiplying by 4.88 and dividing by 450, for example, we can put the measurement into a
 * 32-bit unsigned integer and multiply by 1000. The returned value would be 1000 times the pressure difference.
 */
float pressure_difference(uint16_t measurement){
  float diff2mv = ((float) measurement) * 0.725;
  float pressureDiff = 11.11 * 2.97 * (diff2mv / 1000.0);
  // Unit is kPa.
  return ((pressureDiff < 0.005) ? 0.0 : pressureDiff);
}

/**
 * Liters per minute.
 *
 * FIXME can we get a good approx of this with only integers?
 * 
 */
float flow_rate(uint16_t measurement) {
  float pressure = pressure_difference(measurement);
  float numerator = 2000.0 * pressure / AIR_DENSITY;
  // numerator guaranteed to be positive since pressure is.
  float flow = AREA1 * sqrt(numerator/DENOMINATOR());
  // Litres per minute
  flow = flow * 60000.0;
  return flow;
}
