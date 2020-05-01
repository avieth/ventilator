{-# LANGUAGE RebindableSyntax #-}

module Sensors
  ( sensors
  , Sensors (..)
  , MotorSensors (..)
  , low_switch
  , high_switch
  , low_switch_on
  , high_switch_on
  , encoder_position
  , encoder_position_low
  , encoder_position_high
  , FlowSensors (..)
  , inhale_accumulator
  , exhale_accumulator
  , PressureSensors (..)
  , insp_pressure_accumulator
  , inhale
  , exhale
  , accumulator
  , OxygenSensors (..)

  , flow_insp
  , flow_exp
  , pressure
  , oxygen
  , volume
  , inhale_time_us
  ) where

import Language.Copilot hiding (max)
import Redundancy
import Util (max, integral, controlled_integral)
import Time (time_delta_us)
import Controls (cmv_volume_goal_limited, air_in_threshold)

import Prelude hiding ((++), (&&), (||), (>=), (<), div, drop, max, not)

-- | Organizational record for sensors relating to the piston and motor.
data MotorSensors = MotorSensors
  { -- | The piston is at its lowest position (cannot pull any more air from
    -- the patient).
    -- Used for calibration (to 0 the encoder).
    s_limit_low :: Stream Bool
    -- | The piston is at its highest position (cannot push any more air into
    -- the patient).
    -- TODO needed? Will we have this?
  , s_limit_high :: Stream Bool
    -- | Signal for the encoder which tracks rotational position, from which
    -- the position of the piston can be inferred.
  , s_encoder_position :: Stream Int32
  }

motor_sensors :: MotorSensors
motor_sensors = MotorSensors
  { s_limit_low        = extern "s_limit_low"  Nothing
  , s_limit_high       = extern "s_limit_high" Nothing
  , s_encoder_position = extern "s_encoder_position" Nothing
  }

-- | Organizational record for sensors relating to oxygen concentration.
data OxygenSensors = OxygenSensors
  { s_oxygen_concentration :: WithRedundancy Word32
  }

oxygen_sensors :: OxygenSensors
oxygen_sensors = OxygenSensors
  { s_oxygen_concentration = WithRedundancy
      { principal = extern "s_oxygen_concentration_1" Nothing
      , redundant = extern "s_oxygen_concentration_2" Nothing
      , threshold = extern "s_oxygen_concentration_t" Nothing
      }
  }

-- | Organization record for sensors relating to pressure.
--
-- TODO units?
data PressureSensors = PressureSensors
  { s_insp_pressure :: WithRedundancy Word32
  }

pressure_sensors :: PressureSensors
pressure_sensors = PressureSensors
  { s_insp_pressure = WithRedundancy
      { principal = extern "s_insp_pressure_1" Nothing
      , redundant = extern "s_insp_pressure_2" Nothing
      , threshold = extern "s_insp_pressure_t" Nothing
      }
  }

-- | Organizational record for sensors relating to flow, for patient-initiated
-- (spontaneous) breaths.
data FlowSensors = FlowSensors
  { s_insp_flow   :: WithRedundancy Word32
  , s_exp_flow    :: WithRedundancy Word32
  , s_air_in_flow :: WithRedundancy Word32
  }

flow_sensors :: FlowSensors
flow_sensors = FlowSensors
  { s_insp_flow = WithRedundancy
    { principal = extern "s_insp_flow_1" Nothing
    , redundant = extern "s_insp_flow_2" Nothing
    , threshold = extern "s_insp_flow_t" Nothing
    }
  , s_exp_flow = WithRedundancy
    { principal = extern "s_exp_flow_1" Nothing
    , redundant = extern "s_exp_flow_2" Nothing
    , threshold = extern "s_exp_flow_t" Nothing
    }
  , s_air_in_flow = WithRedundancy
    { principal = extern "s_air_in_flow_1" Nothing
    , redundant = extern "s_air_in_flow_2" Nothing
    , threshold = extern "s_air_in_flow_t" Nothing
    }
  }

-- | Organizational record for all sensor streams.
data Sensors = Sensors
  { s_motor    :: MotorSensors
  , s_pressure :: PressureSensors
  , s_oxygen   :: OxygenSensors
  , s_flow     :: FlowSensors
  }

-- | Sensor streams, all of which are "external", i.e. must be provided by
-- the hardware/firmware.
sensors :: Sensors
sensors = Sensors
  { s_motor    = motor_sensors
  , s_pressure = pressure_sensors
  , s_oxygen   = oxygen_sensors
  , s_flow     = flow_sensors
  }

{-
-- | Take the average of a stream over 8 samples.
accumulator :: Stream Int32 -> Stream Int32
accumulator x = sum `div` constant 8
  where
  stream = [0,0,0,0,0,0,0,0] ++ x
  sum :: Stream Int32
  sum =        stream + drop 1 stream + drop 2 stream + drop 3 stream
      + drop 4 stream + drop 5 stream + drop 6 stream + drop 7 stream
-}
accumulator :: (Num a, Eq a, Integral a, Typed a) => Stream a -> Stream a
accumulator x = sum `div` constant 4
  where
  stream = [0,0,0,0] ++ x
  sum = stream + drop 1 stream + drop 2 stream + drop 3 stream
        --       + drop 4 stream + drop 5 stream + drop 6 stream + drop 7 stream

-- | Accumulated (averaged) inhale flow sensors.
inhale_accumulator :: Stream Word32
inhale_accumulator = accumulator . principal . s_insp_flow . s_flow $ sensors
--inhale_accumulator = principal . s_insp_flow . s_flow $ sensors

-- | Accumulated (averaged) exhale flow sensors.
exhale_accumulator :: Stream Word32
exhale_accumulator = accumulator . principal . s_exp_flow . s_flow $ sensors
--exhale_accumulator = principal . s_exp_flow . s_flow $ sensors

-- | How to filter the insp pressure sensor?
-- Let's try smoothing it by taking the average of the next and the last
-- signal.
insp_pressure_accumulator :: Stream Word32
insp_pressure_accumulator = accumulator sensor_data
  where
  --stream = [0] ++ if low_switch then 0 else stream + sensor_data
  {-stream = integral 0 sensor_data-}
  sensor_data = principal . s_insp_pressure . s_pressure $ sensors

-- | True when an inhale has been detected: whenever `inhale_accumulator`
-- is greater or equal to 8.
--
-- TODO good threshold
inhale :: Stream Bool
inhale = inhale_accumulator >= threshold
  where
  threshold :: Stream Word32
  threshold = constant 8

-- | True when an exhale has been detected: whenever `exhale_accumulator`
-- is greater or equal to 8.
exhale :: Stream Bool
exhale = exhale_accumulator >= threshold
  where
  threshold :: Stream Word32
  threshold = constant 8

-- | The low switch for the motor must read True twice in a row in order
-- to give True.
low_switch :: Stream Bool
low_switch = sensor && drop 1 sensor
  where
  sensor :: Stream Bool
  sensor = [False, False] ++ s_limit_low (s_motor sensors)

-- | Like `low_switch`.
high_switch :: Stream Bool
high_switch = sensor && drop 1 sensor
  where
  sensor :: Stream Bool
  sensor = [False, False] ++ s_limit_high (s_motor sensors)

{-
-- | True when the low switch just changed to on.
low_switch_on :: Stream Bool
low_switch_on = not low_switch_ && drop 1 low_switch_
  where
  low_switch_ = [False, False] ++ low_switch
-}

low_switch_on :: Stream Bool
low_switch_on = not below && drop 1 below
  where
  below :: Stream Bool
  below = [False, False] ++ ((encoder_position - encoder_position_low) < 10)

-- | True when the high switch just changed to on.
high_switch_on :: Stream Bool
high_switch_on = not high_switch_ && drop 1 high_switch_
  where
  high_switch_ = [False, False] ++ high_switch

-- | The encoder position at the low switch. Updated whenever the low switch
-- is hit.
--
-- An initial calibration routine will be sure to hit the low switch and
-- the high switch so as to set these streams to proper values.
encoder_position_low :: Stream Int32
encoder_position_low = stream
  where
  stream = [0] ++
    if low_switch
    then s_encoder_position (s_motor sensors)
    else stream

-- TODO sanity check on high and low positions: given the number of encoder
-- pulses per revolution, we know what the difference between low and high
-- should be.

encoder_position_high :: Stream Int32
encoder_position_high = stream
  where
  stream = [0] ++
    if high_switch
    then s_encoder_position (s_motor sensors)
    else stream

-- | Absolute encoder position.
-- This is meaningless without reference. Use 'encoder_position_low'
-- and 'encoder_position_high', which are set when the low and high switches
-- are triggered.
encoder_position :: Stream Int32
encoder_position = s_encoder_position (s_motor sensors)

flow_insp :: Stream Word32
flow_insp = principal . s_insp_flow . s_flow $ sensors

-- | If this is non-zero, it gives the time in microseconds over which we have
-- observed continuous positive inspiratory flow. It goes back to 0 whenever
-- the inspiratory flow is 0.
-- TODO may want to require it to be 0 for a given amount of time?
inhale_time_us :: Stream Word32
inhale_time_us = stream

  where

  stream :: Stream Word32
  stream = [0] ++ next

  next :: Stream Word32
  next = if flow_insp >= threshold then time_delta_us + stream else 0

  -- Flow is in litres per minute. We choose 5 as a lower bound.
  threshold :: Stream Word32
  threshold = constant 5

flow_exp :: Stream Word32
flow_exp = principal . s_exp_flow . s_flow $ sensors

-- TODO FIXME probably won't remain accurate for long...
-- Also it's not even right: must know the time unit and multiply using
-- t_delta_us.
volume :: Stream Word32
volume = constant 0 -- integral 0 (flow_insp - flow_exp)

pressure :: Stream Word32
pressure = principal . s_insp_pressure . s_pressure $ sensors

-- | The oxygen concentration is inferred by integrating air in flow through
-- a pipe that presumably comes from a pure oxygen tank. Given that the volume
-- of the bellows is known, we can compute from this the approximate
-- percentage of oxygen in the bellows.
--
-- NB: this gives the ratio of pure oxygen to normal air, which of course also
-- contains oxygen. It is _not_ really an estimation of oxygen concentration
-- itself (but it a lower bound?)
--
-- This value is changed only when the bellows reaches the fully retracted
-- point. The integral is taken over the retraction time, as this is when we
-- expect to observe air in flow.
-- What we need:
-- - "Controlled" integral, with signals to indicate when it should be added
--   to, held, or reset to 0.
-- - Indication of change to the 0 state (resting at low)
--
-- Ah, more complicated than we thought: must know how far we actually
-- retracted, not just the total volume.
-- Worth pausing on that: suppose we bring in 100% O2 on the calibration
-- retraction, then we push down half way and then pull in 50% O2. What's
-- the concentration now? 75% right? The air left over on the first push was
-- 100%, then we filled in the remaining air with 50% O2.
--
-- Ok, so we could show instead, as the FiO2 reading, how much oxygen was
-- taken in on the last retraction, as a share of the total volume brought in?
--
-- So we also need
-- - A signal giving the current modelled volume remaining in the bellows.
--   We have that already, it's forward kinematics.
--   `volume_delivered_mm_3`
--   Divide by 1000 to get mL.
-- - We must sample that at the "changed to retracting state"
--
oxygen
  :: Stream Bool -- True when in retracting state.
  -> Stream Bool -- True when just changed to retracting state
  -> Stream Bool -- True when just changed to resting state
  -> Stream Word32 -- Volume "missing" from the bellows (increases as air is pushed out) in mL.
  -> Stream Word32
oxygen in_retract_state changed_to_retract_state changed_to_rest_state bellows_volume_ml = stream

  where

  stream :: Stream Word32
  stream = [0] ++ next
  next :: Stream Word32
  next =
    if changed_to_rest_state
    then (integrated_in_flow_ml) `div` volume_taken_in_ml
    else stream

  integrated_in_flow_ml :: Stream Word32
  integrated_in_flow_ml = controlled_integral
    -- When to reset to 0
    changed_to_retract_state
    -- When to add to the integral
    in_retract_state
    ((sensed_ul_s * time_delta_us) `div` 1000000)

  -- The modelled volume missing from the bellows at the end of the inhale
  -- phase.
  volume_taken_in_ml :: Stream Word32
  volume_taken_in_ml = cmv_volume_goal_limited
 {- [0] ++
    if changed_to_retract_state
    then bellows_volume_ml
    else volume_taken_in_ml -}

  -- The sensed flow from the O2 source, in uL/s
  sensed_ul_s :: Stream Word32
  sensed_ul_s = local (principal . s_air_in_flow . s_flow $ sensors) $ \inflow ->
    if inflow < air_in_threshold then 0 else inflow

{-

  stream :: Stream Word32
  stream = [0] ++ next
  next :: Stream Word32
  next =
    if changed_to_rest_state
    then unsafeCast (unsafeCast (unsafeCast ((constant 100.0 * integrated_in_flow_ml) / unsafeCast volume_taken_in_ml) :: Stream Int64) :: Stream Int32)
    else stream

  integrated_in_flow_ml :: Stream Double
  integrated_in_flow_ml = controlled_integral
    -- When to reset to 0
    changed_to_retract_state
    -- When to add to the integral
    in_retract_state
    ((unsafeCast sensed_ul_s * unsafeCast time_delta_us) / 1000000.0)

  -- The modelled volume missing from the bellows at the end of the inhale
  -- phase.
  volume_taken_in_ml :: Stream Word32
  volume_taken_in_ml = constant 500 {- [1] ++
    if changed_to_retract_state
    then bellows_volume_ml
    else volume_taken_in_ml -}

  -- The sensed flow from the O2 source, in uL/s
  sensed_ul_s :: Stream Word32
  sensed_ul_s = principal . s_air_in_flow . s_flow $ sensors
  -}
