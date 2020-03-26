# Mechanical ventilator software

Find in this repository a high-level specification written in Haskell, and an
example of a simple C simulation and visualization built against the generated
C.

The idea is to factor the ventilator software into these parts:

- Input: sampling of sensors and user controls
- Output: driving the motor
- Everything else: determine the motor speed as a function of sensors and
  user controls

The Haskell program found in the `spec` directory deals exclusively with the
final point. It is expressed in terms of streams of values, using
[copilot](https://copilot-language.github.io/). Compared to an implementation
directly in C or C++, this expresses the logic and constraints of the
ventilator more directly. For example:

```Haskell
motor :: Stream Int8
motor = if critical_alarm
        then 0
        else if cmv_flow > observed_flow
        then 1
        else if cmv_flow < observed_flow
        then -1

cmv_flow :: Stream Int32
cmv_flow = if inhaling then inhale else exhale

inhale :: Stream Int32
inhale = if cmv_mode == volume_control then inhale_vc else inhale_pc
```

It is expected that writing the software in this style will give greater
assurance of correctness and safety.

## How to build

Generated files are included in [./gen](./gen) so that you don't have to
install the Haskell platform in order to get started.

Automated build coming soon, but for now it's simple enough to do by hand.

```sh
# Generate the C assets using the Haskell spec.
# Or, just use the files in ./gen
$ cd spec
$ cabal new-run

# ventilator.h and ventilator.c appear
$ cd ../cbits
$ cp ../spec/ventilator.c ../spec/ventilator.h ./
$ gcc -c ventilator.c
$ gcc main.c ventilator.o -lncurses -lm -o ventilator

# Run the simulation
./ventilator
```

![demo](demo.gif)

## How to run on a real device

The intended form of use is to call into `step()` from `ventilator.c` at every
loop iteration after setting all of the global sensor and control variables
according to hardware.

```C
/**
 * Called when there's a problem.
 */
void raise_alarm(void) {
  make_some_light_flash();
  make_some_buzzer_buzz();
}

/**
 * Called at every step to control the motor.
 */
void control_motor(int8_t v) {
  write_pin(... v ...);
}

/**
 * Called at most once every 10ms (configurable).
 * Argument list is quite long (see ventilator.h)
 */
void update_ui(...) {
  update_hex_display(...);
}

void main() {
  // See ventilator.h for the list of variables which must be set.
  update_sensors();
  update_controls();
  // Generated routine defined in ventilator.c
  // This will call into various trigger routines according to the
  // high-level ventilator spec.
  step();
  // Spin until enough time has passed.
  rate_limit();
}
```

## Discussion

This program is just a basic proof of concept. The Haskell spec implements
continuous mandatory ventilation (CMV) with volume or pressure control. The
simulation is simple and naive but good enough to demonstrate basic
functionality.

The motor control is also simple and naive and maybe not appropriate: the
acceleration is set to 1 if the flow is below desired, -1 if above desired,
and this is integrated to give the current "velocity" as a signed 8-bit
integer.

```Haskell
motor :: Stream Int8
motor =
  -- High and low sensors indicate that the piston cannot move any further, so
  -- motor velocity should be 0.
  if velocity > 0 && s_piston_high (s_piston sensors)
  then 0
  else if velocity < 0 && s_piston_low (s_piston sensors)
  then 0
  else velocity

  where

  velocity = integral 0 acceleration

  acceleration =
    -- If the maximum flow is observed, do not accelerate anymore.
    -- global max flow is unsigned, but surely will not be 2^31 or greater.
    if observed_flow >= unsafeCast global_max_flow
    then 0
    else if (observed_flow < desired_flow) && (velocity < 127)
    then  1
    else if (observed_flow > desired_flow) && (velocity > (-127))
    then -1
    else  0

  -- Must be set by the device driver.
  -- Probably derived from pressure sensors.
  observed_flow = Sensors.flow
```


