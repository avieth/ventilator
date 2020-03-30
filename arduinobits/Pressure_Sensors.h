/*
   20200328
   author: Colin Gallacher
   v0.1

*/


#define AREA1                0.0003976//0.00031416 //m^2
#define AREA2                0.000044178//0.00007854 //m^2 
#define AIR_DENSITY          1.225 //kg/m^3 
//#define DENOMINATOR()        (((AREA1/AREA2) * (AREA1/AREA2)) - 1)

// Truncated the .6
// Could use um^2 instead
#define AREA1_MM_2 397 // mm^2
// Truncated the .178
// Could use um^2 instead but
#define AREA2_MM_2 44  // mm^2
#define AIR_DENSITY_G_M_3 1225 // grams per cubic meter

/**
 * Approximate value of (AREA1_MM_2/AREA2_MM_2)^2 - 1)
 * Do not wish to make the weak little board compute this.
 */
#define DENOMINATOR 80

int sensor_offset(int analogPin);
int read_sensor(int pin);
float pressure_difference(int offSet, float measurement);
float flow_rate(int offSet, float measurement);

/**
 * Reads a pin 10 times, takes the integral average plus 1, to give a baseline reading.
 */
int sensor_offset(int analogPin){
  int sum = 0;
  
  for(int i = 0; i < 10; i++){ //blocking for initialization 
    sum = sum + analogRead(analogPin);
  }
  
  sum = sum / 10;
  sum = sum + 1;
  
  return sum;
}

/**
 * Read the sensor on a given pin. Just a simple analogRead. No adjustment is done.
 * TODO FIXME maybe this should factor in the offset? We could write the offsets
 * to globals and set them on init.
 */
int read_sensor(int pin){
  return analogRead(pin); 
}

/**
 * The measurement comes from a sensor pin and is assumed to already have been corrected for signal offset.
 */
float pressure_difference(int measurement){
  /*
   * Prior definition, with offset given, and measurement as float (was implicitly casting :O ).
   * 
   * There, any measurement which was less than 0.922 from its offset would become 0.
   * With the integral definition, we get 1000 times what the prior reading would be, keeping 3
   * decimal places, without resorting to floating point arithmetic.
   * 
  float offsetMv = offSet * 4.88; // mV per bit, 10bit ADC with 5V reference 
  float diff2mv = measurement * 4.88;
  float pressureDiff = (diff2mv - offsetMv) / 450.0;

  pressureDiff = (pressureDiff < 0.01) ? 0.0 : pressureDiff; //pressure in kpa
  
  return pressureDiff;
  */
  // First let's take 32 bits of room.
  long measurement_wide = (long) measurement;
  // We wish to multiply it by 4.88. Instead, let's multiply it by 1000.0 * 4.88 i.e. 488 and keep it integral.
  // We'll divide by 450, not 450000, leaving an extra 3 places for a fractional part.
  long mv = measurement_wide * 4880;
  long d_pressure = mv / 450;
}


/**
 * The measurement comes from a sensor pin and is assumed to already have been corrected for signal offset.
 * 
 * FIXME can we do this only with integers?
 */
float flow_rate(int measurement) {
  float pressure = (float) measurement;
  
  float numerator = 2 * pressure * 1000 / AIR_DENSITY;
  float flow = AREA1 * sqrt(numerator/DENOMINATOR);

  //flow = flow * 1000 * 60 
  flow = flow / AREA1; // m/s with division by area1
  
  return flow;
}
