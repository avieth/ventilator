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

## Discussion

This program is just a basic proof of concept. The Haskell spec implements
continuous mandatory ventilation (CMV) with volume or pressure control. The
simulation is simple and naive but good enough to demonstrate basic
functionality.
