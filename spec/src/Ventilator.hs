{-|
 - This is a [copilot](https://copilot-language.github.io/) program designed
 - to control a mechanical ventilatilation system.
 -
 - The program works with data streams (type `Stream Int32` for instance),
 - some of which are "extern" (set by a C program), and it defines "triggers",
 - which correspond to C functions which will be called whenever a given
 - condition (a `Stream Bool`) is true, and passed a set of values (sampled
 - from the specified `Stream`s).
 -
 - A program specification is a set of triggers, their condition signals, and
 - the arguments which they receive. This can be simulated in Haskell by giving
 - mock values for the "extern" signals. It can also be compiled to C with C
 - extern declarations for all "extern" signals as well as all declared
 - triggers.
 -
 - The specification for this program is called `spec` and is found near the
 - top of the program text.
 -}

{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE DataKinds #-}

module Ventilator where

import Language.Copilot
import Copilot.Compile.C99

-- Copilot redefines common notions to work over streams. We'll usually want
-- those.
import Prelude hiding ((++), (&&), (>), (>=), (<), (<=), (||), (==), div, drop, mod, not)

import Controls
import Sensors
import CMV
import Kinematics
import Time
import Util

import Motor

pulsedata :: PulseData
pulsedata = pulse_data_from_velocity_dps velocity

  where

  velocity = if calibrated then velocity_main else velocity_calibrate

  -- NB: we can't expect the actual kinematics stuff to work with arbitrary
  -- rotation. The arithmetic/trig breaks down if we rotate too far
  -- FIXME what to do about that, from a safety perspective?
  --
  -- Thresholds here are 5000uL, to avoid jerking back and forth around 0.
  velocity_main :: Stream Int32
  velocity_main =
    if (volume_goal - Kinematics.volume_f) > 5000.0
    then 45
    else if (Kinematics.volume_f - volume_goal) > 5000.0
    then -45
    else 0
  volume_goal :: Stream Double
  volume_goal = unsafeCast (1000 * vmode_volume_ml cmv)
  remaining_time_us :: Stream Word32
  remaining_time_us = vmode_interval_us cmv
  --theta_needed :: Stream Double
  --theta_needed = inverse_volume_delivered volume_goal
  --d_degree :: Stream Double
  --d_degree = radians_to_degrees (theta - theta_needed)
  -- How many degrees per second needed in order to reach the goal.
  --dps :: Stream Double
  --dps = (1000000.0 * d_degree) / unsafeCast remaining_time_us

  -- Device is calibrated once the low switch has been touched twice.
  calibrated :: Stream Bool
  calibrated = calibration_phase > 2

  velocity_calibrate :: Stream Int32
  velocity_calibrate =
    if calibration_phase == 0
    -- First step: go back to zero.
    then -45
    -- Second step: move forward slightly.
    else if calibration_phase == 1
    then 45
    -- Third step: go back to zero again.
    else if calibration_phase == 2
    then -45
    -- Fourth step: we're not in calibration mode anymore.
    else 0

  calibration_phase :: Stream Word8
  calibration_phase = [0] ++ calibration_phase_next

  -- Touch low, move back for 1 second, then touch it again.
  calibration_phase_next :: Stream Word8
  calibration_phase_next =
    if (calibration_phase == 0) && low_switch
    then 1
    else if (calibration_phase == 1) && (elapsed > 1000000)
    then 2
    else if (calibration_phase == 2) && low_switch
    then 3
    else calibration_phase

  elapsed = [0] ++ elapsed_next
  elapsed_next =
    if calibration_phase == 1
    then time_delta_us + elapsed
    else elapsed

-- | The spec for the ventilator program.
spec :: Spec
spec = do

  -- Zero the encoder value whenever the low switch is hit.
  -- This keeps the kinematics computations closer to reality.
  trigger "zero_encoder" low_switch []

  -- At every step call into
  --
  --   void control_motor(int32_t desired_flow, int8_t motor_velocity)
  --
  -- "every_us 0" as in "at least 0us pass between each trigger"
  trigger "control_motor" true
    [ arg_named "us_per_pulse"  (us_per_pulse pulsedata)
    , arg_named "motor_direction" (motor_direction pulsedata)
    ]

  -- At most once every 10ms call
  --
  --   void update_ui()
  --
  {-
  trigger "update_ui" (every_us 10000) [
      arg_named "desired_flow"   $ desired_flow
    , arg_named "motor_pulse"     $ fst motor_control
    , arg_named "motor_direction" $ snd motor_control
    , arg_named "volume" $ Control.volume
    , arg_named "s_piston_high"  $ s_piston_high (s_piston sensors)
    , arg_named "s_piston_low"   $ s_piston_low  (s_piston sensors)
    , arg_named "piston_position" $ Control.forward_kinematics_mm Control.theta
    , arg_named "bpm_limited"    $ bpm_limited
    , arg_named "volume_limit"   $ volume_limit_limited
    , arg_named "pressure_limit" $ pressure_limit_limited
    , arg_named "ie_inhale"      $ ie_inhale
    , arg_named "ie_exhale"      $ ie_exhale
    , arg_named "cmv_mode"       $ cmv_mode
    , arg_named "cmv_volume_goal"   $ cmv_volume_goal_limited
    , arg_named "cmv_pressure_goal" $ cmv_pressure_goal_limited
    , arg_named "global_volume_max" $ global_volume_max
    , arg_named "global_volume_min" $ global_volume_min
    , arg_named "global_pressure_max" $ global_pressure_max
    , arg_named "global_pressure_min" $ global_pressure_min
    ]
  -}

  -- Whenever the `alarm` signal is true, call into the C function
  --
  --   void raise_alarm(void)
  --
  -- with no arguments.
  -- TODO should have an alarm code.
  trigger "raise_alarm" alarm []

-- | Write out the spec to C in "ventilator.h" and "ventilator.c"
gen_c :: IO ()
gen_c = reify spec >>= compile "ventilator"

-- |
-- # Alarm signal

-- | Give True whenever human intervention is required.
--
-- As seen in `spec`, this signal controls when the C function raise_alarm will
-- be called. That function would probably sound a buzzer and flash some
-- lights.
--
-- TODO the alarm should give a code to indicate which signals are inconsistent,
-- so that the UI signal can use it.
-- Also should probably distinguish between critical alarms which imply motor
-- shutdown, and warnings which do not.
--
--
-- TODO an alarm in case the high sensor is true but desired volume or
-- pressure was not reached.
alarm :: Stream Bool
alarm = foldr (||) (constant False) checks
  where
  checks = []
  {-
  checks =
    [ all redundancy_check 
    , redundancy_check pressure
    , redundancy_check o2_concentration
    ]-}

-- | When the flow should be put to 0.
--
-- For now it's whenever there is an alarm. May need to be something nontrivial
-- moving forward.
--
-- TODO FIXME is it better to have the kill switch mean bring the piston back
-- to 0 and stay there until the operator intervenes? Would probably not be
-- good to kill the motor with a high volume and pressure in the system.
kill_flow :: Stream Bool
kill_flow = alarm
