# Software Manual and User Guide

This section contains an overview of the software design, instructions to build
and install it, and a brief operator's guide.

## Quickstart

The device ships with the software installed. The operator does not need to
build or install anything. 

At first startup from a powered-off state, the machine will self-calibrate by
moving the piston to the extremeties. The LCD display will show CALIB in the
status section (top-left) and will flash the green LED. The operator cannot
interact with the device during this process.

After calibration, the LCD will display READY in the status section. The
operator may change mode (Mo), breaths-per-minute (Hz), inhale-to-exhale
ratio (IE), and volume limit (mL). These are selected by moving the dial at
the top right, and edited by pressing the circular button at the bottom left.
The display will show square brackets around the field to be edited, and at
this time, movements of the dial will change the value. A change is committed
by pressing the circular button, and aborted by pressing either of the
triangular buttons.

To begin ventilation, press the right triangular button (which resembles a
play button). The status will display RUNNING and the green LED will be solid.
Parameters may be tuned while the machine is running.

To stop ventilation, press the left triangular button. The red LED will be
solid and an alarm will sound. This is to ensure that an operator must always
take notice when the machine is not running. At this point, any patient in the
circuit should be removed and, if necessary, given manual ventilation support.
The machine can be reset by pressing either of the triangular buttons. This
will bring the piston back to the start position and the machine will enter the
READY state again.

When the SIMV mode is selected, the machine will use the frequency,
breaths-per-minute, and inhale-to-exhale ratios to determine a minimally
acceptable breathing pattern. If the patient begins to inhale while the machine
is not delivering a breath, it will begin to assist.

## Build Instructions

A fully-automatic and reproducible build is expressed using
[nix](https://nixos.org/nix/). In the repository root directory, use
`nix-build` to generate the binary along with a script to upload it to the
microcontroller as follows:

```sh
$ nix-build default.nix --argstr port ttyACM0 --argstr usb false
```

where `ttyACM0` is replaced with the actual serial interface to use. Change
`false` to `true` if using the microcontroller's native USB port rather than
its programming port. When this command finishes, the directory `./result`
will appear, containing the binary and an upload script:

```sh
$ ls ./result/
build.bin
upload
$ ./result/upload
```

Running upload will stop the microcontroller, erasing its current program, and
upload the new one (`build.bin`). The microcontroller will restarts when this
process has completed. The machine may be powered on while this process is
carried out.

A precompiled binary is included in the root directory, called `rideau.bin`.
This can be uploaded manually to the microcontroller using the
[BOSSA](https://github.com/shumatech/BOSSA) tool, available for all common
end-user computing platforms.

## System Description

The software system is factored into two components:

- Low-level interfacing with sensors, switches, motor controls, human-input
  devices, etc.
- High-level definition of the ventilation system itself.

The former is written in C, targeting the Arduino Mega, but in principle is
appropriate for any board with similar or better capabilities in terms of I/O
pins and microprocessor.

The second is expressed in [Haskell](https://www.haskell.org/), by way of the
[copilot](https://copilot-language.github.io/) suite of tools (BSD3 licensed).
copilot was developed by Galois Inc. in partnership with NASA, to be applied in
the runtime verification of unmanned aerial vehicle software[1]. A copilot
specification is expressed in terms of pure functional streams of data--without
destructive updates, pointers, or implicit type casts--and complies to C99
suitable for execution in low-memory, real-time environments, without loops or
dynamic memory allocation.

In a software system where failure can lead to physical harm, high assurance is
of utmost importance. The decision to use a domain-specific specification
language, rather than a direct implementation in C, means it is easier to
characterize, understand, and mitigate failure modes. As an added benefit, bug
fixing, feature iteration, and general experimentation is much faster this way,
due to the modularity and safety intrinsic to a pure functional language with
a modern type system.

The `./spec` directory contains the Haskell source files expressing the
ventilator program abstract over hardware. The `./arduinobits/` directory
contains the C source files expressing the interface between hardware and
spec. At build time, the Haskell spec is used to generate the C source and
header files `ventilator.h` and `ventilator.c`, which are used to compile and
link the binary to be uploaded to the microcontroller.

[1](https://ntrs.nasa.gov/archive/nasa/casi.ntrs.nasa.gov/20120001989.pdf)

## Hazards and Limitations

### Positive Characterization of Safe Operation

Rather than attempting to imagine every possible failure mode of the controller
software, it would be more prudent to characterize exactly what is the
condition of non-failure, i.e. what must the controller observe in order to
judge that it is safe to move air to or from the patient. Whenever this
condition is not met, the controller must raise visual and audible alarms and
must not move any air until an operator (a medical professional) has diagnosed
and resolved the problem.

### Fractional Arithmetic and Use of Floating Point

Some software subsystems, notably kinematics and volume computation, pressure
sensing, and flow sensing, are implemented using standard IEE754 floating point
arithmetic. This is a safety hazard because errors may accumulate, leading to
the controller delivering too little or too much air to the patient. It is
essential that all such computations are meticulously audited to ensure that
they fall within safe error bounds. Even if they were to be replaced with
fixed point arithmetic, such an audit is required.

Unsafe casts from floating points to integers are present in the software and
are at the moment not checked for unacceptable loss of accuracy.

## API Description for Software Developers

There are [generated docs](./spec/docs/) for the high-level specification,
to be viewed in any web browser.
