{-# LANGUAGE RebindableSyntax #-}

{-|
Definitions related to the motor, its hardware driver, and rotary encoder.
-}

module Motor where

import Language.Copilot
import Prelude hiding ((++), (<), (>), (<=), (>=), (&&), (/=), (==), div, drop, not,
  min, max)

import Sensors

-- TODO must reset the position on the high switch as well as the low switch.
-- At low we already do this: set it to 0. But what to set at high? We don't
-- know a priori what the encoder value should be, do we? The obvious idea is
-- to pick some sensible maximum value such that we have continuity: we know
-- that _theta_ will tend to 0 by definition as the bellows reaches the
-- high position. What does it tend to toward low? We can figure that out from
-- the system measurements.
-- So how about that? Low and high switches reset the _angle_ rather than the
-- encoder counter.
-- Problem is of course that we really do need some correspondence with the
-- encoder counter. That's what calibration can take care of?
-- 1. Go to high
-- 2. Set to zero
-- 3. Go to low
-- 4. Remember the count at low
-- Right. In calibration mode the machine continuously moves the piston up and
-- down until the switches hit. At each switch hit it sets the given encoder
-- value (reset at high, hold at low). In normal mode, the calibration values
-- are used: at high it sets to 0, at low it sets to whatever the calibrated
-- low value is.
--
-- So calibration is itself divided into subcycles: push and pull. 
-- - On push we
--   - Give a constant forward (positive) velocity
--   - Check that we see positive insp flow and that it is somewhat in line
--     with the kinematics computation
--   - Change to pull when the high switch is hit (trigger to reset the encoder)
-- - On pull we
--   - Give a constant backward (negative) velocity
--   - Change to push when the low switch is hit, also triggering some C
--     function but not restting the encoder. Instead, holding the encoder
--     value in a copilot stream for use in normal modes.
--




-- | How many steps per revolution. This will be known and static for the
-- device, so is not really a configuration variable.
--
-- Found that 3200 does not work; the motor can't be made to move even at 90
-- degrees per second with this configuration. 1600 is fine though.
steps_per_rotation :: Stream Word32
steps_per_rotation = constant 3200

-- | How many encoder pulses per revolution.
pulses_per_rotation :: Stream Word32
pulses_per_rotation = constant 2048

-- | How many millidegrees between two encoder positions, with a sign
-- according to the relative magnitudes (positive if the first is greater
-- than the second).
--
-- TODO don't use millidegrees or radians, use instead subdivisions of Pi.
-- The encoder divides 2Pi into 2048 equal arc lengths. If the encoder
-- position is n greater than the low encoder position, then we have moved
-- nPi/2048 radians. Ideally our trig computations can work in terms of
-- Pi/2048 rather than degrees or radians.
md_encoder_position :: Stream Int32 -> Stream Int32 -> Stream Int32
md_encoder_position a b = ((a - b) * 360000) `div` unsafeCast pulses_per_rotation

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
--
-- Enforces limits on speed according to the encoder, so that the piston
-- assembly will not slam the endpoints and risk damage.
-- Since the low and high switches set the encoder offsets, this also enforces
-- 0 speed at the endpoints.
pulse_data_from_velocity_dps :: Stream Int32 -> PulseData
pulse_data_from_velocity_dps x_dps = rate

  where
  -- TODO audit and improve motor control w.r.t. integral resolution.

  -- Must guard against 0 velocity, in which case the number of microseconds
  -- per pulse is infinite.
  -- Instead, we set it to 0us, where 0us is understood to mean "do not rotate
  -- at all" rather than "rotate infinitely fast".
  rate = if x_dps == 0 then constant 0 else us_per_step

  us_per_step = 360000000 `div` (unsafeCast steps_per_rotation * x_dps)

{-
  -- Millidegrees per second
  mdps = 1000 * x_dps

  steps_per_second = mdps `div` md_per_step

  md_per_step = 360000 `div` unsafeCast steps_per_rotation

  us_per_step :: Stream Int32
  us_per_step = 1000000 `div` steps_per_second
-}

-- | If the machine is calibrated for the motor (high and low switches used
-- to set encoder offsets high and low), then this function may be used to
-- give a limit on the velocity, so as to ensure the motor does not slam the
-- piston against the endpoints.
--
-- TODO better name. There are other things which will limit the motor
-- velocity, they should be named based on why they limit / what they protect.
limit_protect_endpoints :: Stream Int32 -> Stream Int32
limit_protect_endpoints dps = limited_dps
  where
  -- Problem with this: can't use it until it's calibrated :)
  limited_dps :: Stream Int32
  limited_dps =
    if dps > 0
    then positive_limit
    else if dps < 0
    then negative_limit
    else 0

  -- How many millidegrees below the high position
  high_delta :: Stream Int32
  high_delta = md_encoder_position encoder_position_high encoder_position

  -- How many millidegrees above the low position
  low_delta :: Stream Int32
  low_delta = md_encoder_position encoder_position encoder_position_low

  positive_limit =
    if high_delta <= 0
    then 0
    else if high_delta < 1000
    then 10
    else dps

  negative_limit =
    if low_delta <= 0
    then 0
    else if low_delta < 1000
    then -10
    else dps
