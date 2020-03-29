{-# LANGUAGE RebindableSyntax #-}

module Motor where

import Language.Copilot
import Prelude hiding ((++), (<), (>), (>=), (&&), (/=), (==), div, drop, not)

-- | How many steps per revolution. This will be known and static for the
-- device, so is not really a configuration variable.
steps_per_rotation :: Stream Word32
steps_per_rotation = constant 800

-- | How many degrees are traversed per step.
degrees_per_step :: Stream Double
degrees_per_step = constant 360.0 / unsafeCast steps_per_rotation

-- | Organizational record for stepper motor pulse data: how many microseconds
-- between each pulse, and which direction.
data PulseData = PulseData
  { us_per_pulse    :: Stream Word32
  , motor_direction :: Stream Bool
  }

-- | Given a desired degrees-per-second velocity, compute the microseconds
-- per pulse and direction bit.
--
-- Microseconds per pulse is 0 if the speed is 0.
--
pulse_data_from_velocity_dps :: Stream Int32 -> PulseData
pulse_data_from_velocity_dps x_dps = PulseData rate direction

  where

  -- Must guard against 0 velocity, in which case the number of microseconds
  -- per pulse is infinite.
  -- Instead, we set it to 0us, where 0us is understood to mean "do not rotate
  -- at all" rather than "rotate infinitely fast".

  rate = if x_dps == 0 then constant 0 else us_per_pulse
  direction = if x_dps >= 0 then true else false

  -- Unsafe cast should be fine: a degrees-per-second of higher than 2^31 is
  -- quite ridiculous.
  x_dps_unsigned :: Stream Word32
  x_dps_unsigned = unsafeCast (abs x_dps)

  -- TODO use configuration; 450 comes from 360000 / 800, for an 800 steps
  -- per revolution motor.
  pulses_per_second :: Stream Word32
  pulses_per_second =
          (1000 * x_dps_unsigned * steps_per_rotation)
    `div` (constant 360000)

  us_per_pulse :: Stream Word32
  us_per_pulse = 1000000 `div` pulses_per_second
