{-# LANGUAGE RebindableSyntax #-}

module Motor where

import Language.Copilot
import Prelude hiding ((++), (<), (>), (>=), (&&), (/=), (==), div, drop, not,
  min, max)

import Sensors (high_switch, low_switch)

-- | How many steps per revolution. This will be known and static for the
-- device, so is not really a configuration variable.
steps_per_rotation :: Stream Word32
steps_per_rotation = constant 800

-- | How many millidegrees are traversed per step.
md_per_step :: Stream Word32
md_per_step = constant 360000 `div` steps_per_rotation

-- | How many encoder pulses per revolution.
pulses_per_rotation :: Stream Word32
pulses_per_rotation = constant 2048

-- | How many millidegrees per encoder pulse
md_per_pulse :: Stream Word32
md_per_pulse = constant 360000 `div` pulses_per_rotation

-- | Microseconds per pulse, where the sign gives direction. 0 means no
-- movement.
type PulseData = Stream Int32

-- | Given a desired degrees-per-second velocity, compute the microseconds
-- per pulse and direction (its sign).
--
-- Microseconds per pulse is 0 if the speed is 0.
--
-- Uses the high and low switches to clamp the rate so that it does not
-- move backwards when the low switch is on, for instance.
pulse_data_from_velocity_dps :: Stream Int32 -> PulseData
pulse_data_from_velocity_dps x_dps =
  if low_switch && (rate < 0)
  then 0
  else if high_switch && (rate > 0)
  then 0
  else rate

  where

  -- Must guard against 0 velocity, in which case the number of microseconds
  -- per pulse is infinite.
  -- Instead, we set it to 0us, where 0us is understood to mean "do not rotate
  -- at all" rather than "rotate infinitely fast".
  rate = if x_dps == 0 then constant 0 else us_per_pulse

  pulses_per_second :: Stream Int32
  pulses_per_second =
          (1000 * x_dps * unsafeCast steps_per_rotation)
    `div` (constant 360000)

  us_per_pulse :: Stream Int32
  us_per_pulse = 1000000 `div` pulses_per_second
