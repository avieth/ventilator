{-# LANGUAGE RebindableSyntax #-}

module Sensors
  ( sensors
  , Sensors (..)
  , MotorSensors (..)
  , low_switch
  , high_switch
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
import Util (max, integral)
import Time (time_delta_us)

import Prelude hiding ((++), (&&), (>=), div, drop, max)

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

-- | TODO infer from air in flow...
oxygen :: Stream Word32
oxygen = principal . s_air_in_flow . s_flow $ sensors
