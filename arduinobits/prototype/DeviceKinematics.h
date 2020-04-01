/*
   20200328
   author: Colin Gallacher
   v0.1

*/

#define DEVICESTARTPOSITION 0.06
#define BELLOWSDIAMETER 0.102 //millimeters

float h = 0.075;
float l = 0.085;
float L = 0.120;

float forwardKinematics(float theta);
float inverseKinematics(float x_desired);


//ROBOTICS

float forwardKinematics(float theta) {
  
  theta= theta * 3.14f / 180.0f;

  float x_a = l * cos(theta);
  float y_a = l * sin(theta);

  float s12 = (h - y_a) / L;
  float c12_squared = 1.0f - s12 * s12;
  float c12 = pow(c12_squared, 0.5);
  float x_b = x_a + L * c12;

  return  x_b;

}

float inverseKinematics(float x_desired) {

  float alpha, phi;
  float r_squared = x_desired * x_desired + h * h;

  float r = pow(r_squared, 0.5);

  if (r < (l + L)) {

    phi = acos((L * L - r_squared - l * l) / (-2 * l * r));

    alpha = atan2(h, x_desired);

    float gamma = alpha + phi;

    gamma = gamma* 180.0f / 3.14f; //rads to degrees

    return  gamma;

  }
  else {
    Serial.println("error: setpoint out of reach");
  }
}
