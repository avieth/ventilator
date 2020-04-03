{-|
This is a [copilot](https://copilot-language.github.io/) program designed
to control a mechanical ventilatilation system.

The program works with data streams (type `Stream Int32` for instance),
some of which are "extern" (set by a C program), and it defines "triggers",
which correspond to C functions which will be called whenever a given
condition (a `Stream Bool`) is true, and passed a set of values (sampled
from the specified `Stream`s).

A program specification is a set of triggers, their condition signals, and
the arguments which they receive. This can be simulated in Haskell by giving
mock values for the "extern" signals. It can also be compiled to C with C
extern declarations for all "extern" signals as well as all declared
triggers.

The specification for this program is called `spec`.
-}

{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE DataKinds #-}

module Ventilator where

import Language.Copilot
import Copilot.Compile.C99

-- Copilot redefines common notions to work over streams. We'll usually want
-- those.
import Prelude hiding ((++), (&&), (>), (>=), (<), (<=), (||), (==), (/=), div,
  drop, mod, not)

import Controls
import Sensors
import Redundancy (principal)
import CMV
import Kinematics
import Time
import Util

import Motor

-- | Motor pulse data: converts the degrees-per-second velocity into the
-- time between pulses, using hard-coded properties of the particular motor
-- and driver.
pulsedata :: PulseData
pulsedata = pulse_data_from_velocity_dps motor_velocity

-- | Calculate the desired motor velocity at an instant (degrees-per-second).
--
-- This is the essence of the ventilator program.
--
motor_velocity :: Stream Int32
peak_pressure :: Stream Int32
(motor_velocity, peak_pressure) = (velocity, peak_pressure)

  where

  -- It's easy to see that the motor will not move unless it is safe to move.
  -- FIXME safe_to_move needs to be characterized.
  velocity =
    if not safe_to_move
    then 0
    else if calibrated
    then velocity_main
    -- Calibration brings the piston all the way forward and then back, to
    -- set the rotary encoder zero.
    else velocity_calibrate

  -- Mask CMV until we are calibrated.
  -- CMV will not count up while this is False.
  -- When it flips to True it will begin counting and running its cycles.
  -- It is durable against changes to this mask: any flip from False to True
  -- will gracefully reset by exhaling to the desired PEEP and then beginning
  -- again.
  cmv_control :: CMVControl
  cmv_control = calibrated

  peak_pressure :: Stream Int32
  peak_pressure = [0] ++
    if subcycle_change
    then 0
    else Util.max peak_pressure insp_pressure_accumulator
    where
    subcycle = [False] ++ cmv_subcycle (cmv_cycle cmv_control spontaneous_breath)
    subcycle_change = subcycle /= cmv_subcycle (cmv_cycle cmv_control spontaneous_breath)

  spontaneous_breath :: SpontaneousBreath
  spontaneous_breath = Sensors.inhale

  velocity_main :: Stream Int32
  velocity_main =
    local Kinematics.volume_f $ \vf ->
      local (unsafeCast (unsafeCast dps :: Stream Int64) :: Stream Int32) $ \dps_v ->
        -- TODO need to decide on a good decision for velocity. Cannot be too
        -- resource intensive (computing a derivative involing sin, cos, sqrt
        -- for instance).
        if (volume_goal - vf) >= 5000
        then dps_v
        else if (vf - volume_goal) >= 5000
        -- For exhale we'll go faster, subject to a minimum speed
        then if (dps_v * 4) > (-30)
          then -30
          else (dps_v * 4)
        else 0

  volume_goal :: Stream Double
  volume_goal = unsafeCast (1000 * vmode_volume_ml (cmv cmv_control spontaneous_breath))

  remaining_time_us :: Stream Word32
  remaining_time_us =
    if vmode_interval_us (cmv cmv_control spontaneous_breath) <= 1000
    then 1000
    else vmode_interval_us (cmv cmv_control spontaneous_breath)

  remaining_time_s :: Stream Double
  remaining_time_s = unsafeCast remaining_time_us / 1000000.0

  required_theta :: Stream Double
  required_theta = inverse_volume_delivered volume_goal

  d_theta :: Stream Double
  d_theta = theta - required_theta

  dps :: Stream Double
  dps = d_theta / remaining_time_s

  -- Device is calibrated once the low switch has been touched twice.
  calibrated :: Stream Bool
  calibrated = calibration_phase > 2

  velocity_calibrate :: Stream Int32
  velocity_calibrate =
    if calibration_phase == 0
    -- First step: go back to zero.
    then -45
    -- Second step: move forward. This will end once the maximal rotation
    -- has been reached.
    else if calibration_phase == 1
    then 45
    -- Third step: go back to zero again.
    else if calibration_phase == 2
    then -45
    -- Fourth step: we're not in calibration mode anymore.
    else 0

calibration_phase :: Stream Word8
calibration_phase = [0] ++ calibration_phase_next

-- | Is true whenever the current calibration phase differs from the
-- previous one. Used to trigger function. Useful for debugging, to check
-- the encoder value.
calibration_phase_change :: Stream Bool
calibration_phase_change = [False] ++ (calibration_phase /= calibration_phase_next)

-- Touch low, move back for 1 second, then touch it again.
calibration_phase_next :: Stream Word8
calibration_phase_next =
  if (calibration_phase == 0) && low_switch
  then 1
  -- Second phase: move until we cannot move any more (theta decreases as we
  -- move forward).
  else if (calibration_phase == 1) && high_switch
  then 2
  else if (calibration_phase == 2) && low_switch
  then 3
  else calibration_phase

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
-- TODO an alarm in case the high sensor is true but desired volume or
-- pressure was not reached.
alarm :: Stream Bool
alarm = foldr (||) (constant False) checks
  where
  checks = []

-- | The motor will not move unless this is true.
--
-- For now it's false whenever there is an alarm.
-- Will need something better moving forward.
--
-- TODO FIXME is it better to have the kill switch mean bring the piston back
-- to 0 and stay there until the operator intervenes? Would probably not be
-- good to kill the motor with a high volume and pressure in the system.
safe_to_move :: Stream Bool
safe_to_move = not alarm

-- | The spec for the ventilator program.
spec :: Spec
spec = do

  -- Zero the encoder value whenever the low switch is hit.
  -- This keeps the kinematics computations closer to reality.
  trigger "zero_encoder" low_switch []

  trigger "calibration_change" calibration_phase_change
    [ arg $ calibration_phase
    , arg $ theta
    , arg $ volume_f
    ]

  -- At every step call into
  --
  --   void control_motor(int32_t desired_flow, int8_t motor_velocity)
  --
  -- "every_us 0" as in "at least 0us pass between each trigger"
  trigger "control_motor" true
    [ arg_named "us_per_pulse" pulsedata
    ]

  -- Update the UI, subject to rate limiting because it may be too expensive.
  trigger "update_ui" (every_us 200000) [

    -- Key stats for the operator: flow, volume, and pressure.
    -- volume is computed as usual from the motor position.
    -- flow is taken to be the change in volume at the current motor
    -- velocity.
    -- TODO use sensors.
      arg_named "flow"      $ (unsafeCast (unsafeCast (flow_f_observed time_delta_us Kinematics.volume_f) :: Stream Int64) :: Stream Int32)
    , arg_named "volume_ml" $ (unsafeCast (unsafeCast (volume_f / 1000.0) :: Stream Int64) :: Stream Int32)
    , arg_named "pressure" $ peak_pressure

    , arg_named "bpm_limited"    $ bpm_limited
    , arg_named "ie_inhale"      $ ie_inhale_limited
    , arg_named "ie_exhale"      $ ie_exhale_limited

    , arg_named "cmv_mode"          $ cmv_mode
    , arg_named "cmv_volume_goal"   $ cmv_volume_goal_limited
    , arg_named "cmv_pressure_goal" $ cmv_pressure_goal_limited
    ]

  -- Whenever the `alarm` signal is true, call into the C function
  --
  --   void raise_alarm(void)
  --
  -- with no arguments.
  -- TODO should have an alarm code.
  trigger "raise_alarm" alarm []

-- | Write out the spec to C in @ventilator.h@ and @ventilator.c@
gen_c :: IO ()
gen_c = reify spec >>= compile "ventilator"
