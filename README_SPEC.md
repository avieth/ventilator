# Mechanical ventilator software

Find in this repository a high-level specification written in Haskell, and an
arduino project which uses that spec.

The ventilator software is factored into these parts:

- Input: sampling of sensors and user controls
- Output: driving the motor
- Everything else: determine the motor speed as a function of sensors and
  user controls

The I/O portions are meant to kept as small and simple as possible, and are
expressed in C. Find these programs in `./arduinobits`. The files
`ventilator.h` and `ventilator.c` are generated from the Haskell
specification.

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

Generated files are included in [./arduinobits](./arduinobits) so that you don't
have to install the Haskell platform in order to get started.

For a quick and (hopefully) reproducible build, you can use
[nix](https://nixos.org/nix/). Install it and then run `./generate.sh`, which
will place freshly-generated `ventilator.h` and `ventilator.c` files in the
`./arduinobits` directory.
