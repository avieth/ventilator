/*
   20200328
   author: Colin Gallacher
   v0.1

*/


#define AREA1                0.0003976//0.00031416 //m^2
#define AREA2                0.000044178//0.00007854 //m^2 
#define AIR_DENSITY          1.225 //kg/m^3 
#define DENOMINATOR()        (((AREA1/AREA2) * (AREA1/AREA2)) - 1)

/*
// Truncated the .6
// Could use um^2 instead
#define AREA1_MM_2 397 // mm^2
// Truncated the .178
// Could use um^2 instead but
#define AREA2_MM_2 44  // mm^2

*/

#define AREA1_MM_2 397 // mm^2
#define AREA2_MM_2 44  // mm^2
#define AIR_DENSITY_G_M_3 1225 // grams per cubic meter
#define DENOMINATOR_I 80


int16_t sensor_offset(uint8_t analogPin);
uint16_t read_sensor(uint8_t pin);
float pressure_difference(int16_t measurement);
float flow_rate(int16_t measurement);
int32_t flow_rate_i(int16_t measurement);

/**
 * Reads a pin 10 times, takes the integral average plus 1, to give a baseline reading.
 */
int16_t sensor_offset(uint8_t analogPin){
  // int is wide enough, since we're only summing 10 measurements in [0,1024)
  // But oh well use 32 bits anyway.
  int32_t sum = 0;
  
  for(int i = 0; i < 10; i++){ //blocking for initialization 
    sum = sum + (int32_t) (analogRead(analogPin));
  }
  
  sum = sum / 10;
  sum = sum + 1;
  
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
  float pressureDiff = (((float) measurement) * 4.88) / 450.0;
  // Unit is kPa.
  return pressureDiff;
  // Used to be we eliminated small readings. Any good reason to do that? I doubt it.
}

/**
 * TBD unit? Pa?
 */
int32_t pressure_difference_i(int16_t measurement) {
  int32_t wide_measurement = (int32_t) measurement;
  return (wide_measurement * 4880) / ((int32_t) 450);
}


/**
 * The measurement comes from a sensor pin and is assumed to already have been corrected for signal offset.
 * 
 * FIXME can we get a good approx of this with only integers?
 * 
 * FIXME is this even correct? I thought the venturi nozzle flow computation required two pressure
 * values. Here, pressure_difference seems to use only one pressure value.
 */
float flow_rate(int16_t measurement) {
  int32_t pressure = pressure_difference_i(measurement);
  float numerator = 2.0 * ((float) pressure) / AIR_DENSITY;
  // FIXME it can be negative, in which case you get NaN from the sqrt.
  if (numerator < 0) {
    return -60000 * AREA1 * sqrt(-numerator/DENOMINATOR());
  } else {
    return 60000 * AREA1 * sqrt(numerator/DENOMINATOR());
  }
}
/*
int32_t sqrt_i(int32_t n) {
  int32_t x = n;
  int32_t y = 1;
  while (x > y) {
    x = (x + y) / 2;
    y = n / x;
  }
  return x;
}

int32_t flow_rate_i(int16_t measurement) {
  int32_t pressure_d = pressure_difference_i(measurement);
  int32_t numerator = (2000 * pressure_d) / AIR_DENSITY_G_M_3;
  if (pressure_d < 0) {
    return -AREA1_MM_2 * sqrt_i((-numerator) / DENOMINATOR_I);
  } else {
    return AREA1_MM_2 * sqrt_i(numerator / DENOMINATOR_I);
  }
}
*/
