/*
   20200328
   author: Colin Gallacher
   v0.1

*/


#define AREA1                0.0003976//0.00031416 //m^2
#define AREA2                0.000044178//0.00007854 //m^2 
#define AIR_DENSITY          1.225 //kg/m^3 
#define DENOMINATOR()        (((AREA1/AREA2) * (AREA1/AREA2)) - 1)

int sensor_offset(int analogPin);
int read_sensor(int pin);
float pressure_difference(int offSet, float measurement);
float flow_rate(int offSet, float measurement);
void sensor_control(long currentTime);


int sensor_offset(int analogPin){
  int sum = 0;
  
  for(int i = 0; i < 10; i++){ //blocking for initialization 
    sum = sum + analogRead(analogPin);
  }
  
  sum = sum / 10;
  sum = sum + 1;
  
  return sum;
}


int read_sensor(int pin){
 
  int measurement = analogRead(pin); 
  
}



float pressure_difference(int offSet, float measurement){
  float offsetMv = offSet * 4.88; // mV per bit, 10bit ADC with 5V reference 
  float diff2mv = measurement * 4.88;
  float pressureDiff = (diff2mv - offsetMv) / 450.0;
  
  pressureDiff = (pressureDiff < 0.01) ? 0.0 : pressureDiff; //pressure in kpa
  
  return pressureDiff;
}


float flow_rate(int offSet, float measurement){
  float pressure = pressure_difference(offSet, measurement);
  
  float numerator = 2 * pressure * 1000 / AIR_DENSITY;
  float flow = AREA1 * sqrt(numerator/DENOMINATOR());

  //flow = flow * 1000 * 60 
  flow = flow / AREA1; // m/s with division by area1
  
  return flow;
}
