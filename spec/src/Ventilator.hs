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
import Prelude hiding ((&&), (>), (<), (||))

import Controls
import Sensors
import CMV
import Time

-- | The spec for the ventilator program.
spec :: Spec
spec = do

  -- At every step call into
  --
  --   void control_motor(int32_t desired_flow, int8_t motor_velocity)
  --
  -- "every_us 0" as in "at least 0us pass between each trigger"
  trigger "control_motor" (every_us 0) [arg motor]

  -- At most once every 10ms call
  --
  --   void update_ui()
  --
  trigger "update_ui" (every_us 10000) [
      arg $ desired_flow
    , arg $ motor
    , arg $ s_piston_high (s_piston sensors)
    , arg $ s_piston_low  (s_piston sensors)
    , arg $ bpm_limited
    , arg $ volume_limit_limited
    , arg $ pressure_limit_limited
    , arg $ ie_inhale
    , arg $ ie_exhale
    , arg $ cmv_mode
    , arg $ cmv_volume_goal_limited
    , arg $ cmv_pressure_goal_limited
    , arg $ global_volume_max
    , arg $ global_volume_min
    , arg $ global_pressure_max
    , arg $ global_pressure_min
    ]

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

-- | TODO switch based on controls.
desired_flow :: Stream Int32
desired_flow = cmv_flow

-- | Here we use the desired and observed flow to determine motor velocity.
--
-- Currently it's very stupid: max if we need more flow, min if we need less.
-- FIXME make it better.
-- FIXME TODO it's essential that the flow is limited to eliminate the
-- possibility of too high an increase in pressure, since that could harm the
-- patient. Since all that we control is the motor speed, we do not limit
-- flow directly; we have to limit motor speed instead and have some sort of
-- idea about the relationship between motor speed and flow.
motor :: Stream Int8
motor =
  -- High and low sensors indicate that the piston cannot move any further, so
  -- motor velocity should be 0.
  if v > 0 && s_piston_high (s_piston sensors)
  then 0
  else if v < 0 && s_piston_low (s_piston sensors)
  then 0
  else v
  where
  -- This is not workable in a simulation where flow changes instantly, because
  -- it can hop down below the desired flow, causing it to bump back up, and
  -- so on ad infinitum. In the real world this _probably_ won't happen but
  -- it could.
  v = if observed_flow < desired_flow
      then 127
      else if observed_flow > desired_flow
      then -127
      else 0
  observed_flow = Sensors.flow
