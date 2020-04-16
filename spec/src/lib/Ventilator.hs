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
import Kinematics
import Time
import Util
import Motor

import qualified State as State

-- List of what we need in general
-- - The mode
-- - Whether the device has been successfully calibrated
-- - The encoder value at high
--

-- | Motor pulse data: converts the degrees-per-second velocity into the
-- time between pulses, using hard-coded properties of the particular motor
-- and driver.
pulsedata :: PulseData
pulsedata = pulse_data_from_velocity_dps motor_velocity

-- This is essentially the "main" routine: compute the motor velocity at
-- an instant.
motor_velocity :: Stream Int32
motor_velocity = State.motor_velocity

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

  -- At every step call into
  --
  --   void control_motor(uint32_t us_per_pulse)
  --
  trigger "control_motor" true
    [ arg_named "us_per_pulse" pulsedata
    ]

  -- Update the UI, subject to rate limiting because it may be too expensive.
  trigger "update_ui" (every_us 50000) [

      arg_named "state" State.state
    , arg_named "mode" Controls.mode

    -- Key stats for the operator: flow, volume, and pressure.
    -- volume is computed as usual from the motor position.
    -- flow is taken to be the change in volume at the current motor
    -- velocity.
    , arg_named "flow_insp" $ Sensors.flow_insp
    , arg_named "flow_exp"  $ Sensors.flow_exp
    , arg_named "volume_ml" $ (unsafeCast (unsafeCast (volume_f / 1000.0) :: Stream Int64) :: Stream Int32)
    , arg_named "pressure"  $ Sensors.pressure
    , arg_named "oxygen"    $ Sensors.oxygen

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
