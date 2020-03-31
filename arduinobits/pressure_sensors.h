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
#define AIR_DENSITY_G_M_3 1225 // grams per cubic meter
#define DENOMINATOR 80
*/

uint16_t sensor_offset(uint8_t analogPin);
uint16_t read_sensor(uint8_t pin);
float pressure_difference(int16_t measurement);
float flow_rate(int32_t measurement);

/**
 * Reads a pin 10 times, takes the integral average plus 1, to give a baseline reading.
 */
uint16_t sensor_offset(uint8_t analogPin){
  // int is wide enough, since we're only summing 10 measurements in [0,1024)
  unsigned int sum = 0;
  
  for(int i = 0; i < 10; i++){ //blocking for initialization 
    sum = sum + analogRead(analogPin);
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
float pressure_difference(int16_t measurement){ 
  // But now the measurement is already offset-corrected so this is the same (but we correct using integer subtraction).
  float pressureDiff = (((float) measurement) * 4.88) / 450.0;
  pressureDiff = (pressureDiff < 0.01) ? 0.0 : pressureDiff; //pressure in kpa
  return pressureDiff;
}


/**
 * The measurement comes from a sensor pin and is assumed to already have been corrected for signal offset.
 * 
 * FIXME can we get a good approx of this with only integers?
 * 
 * FIXME is this even correct? I thought the venturi nozzle flow computation required two pressure
 * values. Here, pressure_difference seems to use only one pressure value.
 */
float flow_rate(int32_t measurement) {
  float pressure = pressure_difference(measurement);
  float numerator = 2.0 * pressure * 1000.0 / AIR_DENSITY;
  float flow = AREA1 * sqrt(numerator/DENOMINATOR());
  flow = flow * 1000 * 60; // L/minute
  return flow;
}
