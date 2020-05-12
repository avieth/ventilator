#define AREA1                0.0003976
#define AREA2                0.000044156
#define AIR_DENSITY          1.225
#define DENOMINATOR()        (((AREA1/AREA2) * (AREA1/AREA2)) - 1)

/* Remember to set the analog read resolution to 12 bits */
#define ADC_BITS 12
/* mV reference voltage */
#define ADC_REFERENCE 3300

uint16_t sensor_offset(uint8_t analogPin);

float pressure_difference(uint16_t measurement);
float flow_rate(uint16_t measurement);

uint32_t pressure_difference_i(uint16_t measurement);

/**
 * Take the maximum of 500 readings, to give the sensors "offset" which is
 * taken to be the baseline value for sensor readings. Anything less is
 * considered to be 0, anything greater is the difference.
 */
uint16_t sensor_offset(uint8_t analogPin){
  uint32_t max = 0;
  uint32_t reading = 0;

  for(int i = 0; i < 500; i++){
    reading = (uint32_t) (analogRead(analogPin));
    max = (reading > max) ? reading : max;
  }

  return max;
}

/**
 * The measurement comes from a sensor pin and is assumed to already have been corrected for signal offset.
 * 
 * TODO make it integral, pascals.
 * How?
 * 1. compute everything with 12 fractional parts.
 */
float pressure_difference(uint16_t measurement){
  /* Convert bits to volts. */
  float vs = (((float) measurement) / 4096.0) * 3.3;
  /* From the sensor data sheet: convert that voltage to mmh2o */
  //float diffmmh2o = vs * (1019.78 / 2.97);
  // Experimentally-determined it to be 2.673
  float diffmmh2o = vs * (1019.78 / 2.673);
  /* Give cm h2o */
  return (1.05 * diffmmh2o / 10.0);
}

/**
 * Approximate pressure difference in pascals, multiplied by 2^16.
 * It's derived from the floating point pressure_difference: compute the
 * coefficient manually, multiply it by 2^16 and drop the fractional part, to
 * get 207420.
 */
uint32_t pressure_difference_i(uint16_t measurement) {
  return 207420 * (uint32_t) measurement;
}

/**
 * Liters per second.
 *
 * FIXME can we get a good approx of this with only integers?
 *
 * Q = A_1 * sqrt((2 / ro) * (p / ((A_1/A_2)^2 - 1))
 * 
 * where ro is air density, p is pressure difference  in pascals.
 */
float flow_rate(uint16_t measurement) {
  float pressure = pressure_difference(measurement);
  // Got to make it pascals.
  pressure = pressure * 98.0665;
  float numerator = 2.0 * pressure / AIR_DENSITY;
  // numerator guaranteed to be positive since pressure is.
  // m^3 / s
  float flow = AREA1 * sqrt(numerator/DENOMINATOR());
  // Litres per second
  flow = flow * 1000.0;
  return flow;
}
