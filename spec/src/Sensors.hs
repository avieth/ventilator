{-# LANGUAGE RebindableSyntax #-}

module Sensors where

import Language.Copilot
import Redundancy

import Prelude hiding ((++), (&&), (>=), div, drop)

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
  { s_insp_pressure :: WithRedundancy Int32
  , s_exp_pressure :: WithRedundancy Int32
  }

pressure_sensors :: PressureSensors
pressure_sensors = PressureSensors
  { s_insp_pressure = WithRedundancy
      { principal = extern "s_insp_pressure_1" Nothing
      , redundant = extern "s_insp_pressure_2" Nothing
      , threshold = extern "s_insp_pressure_t" Nothing
      }
  , s_exp_pressure = WithRedundancy
      { principal = extern "s_exp_pressure_1" Nothing
      , redundant = extern "s_exp_pressure_2" Nothing
      , threshold = extern "s_exp_pressure_t" Nothing
      }
  }

-- | Organizational record for sensors relating to flow, for patient-initiated
-- (spontaneous) breaths.
data FlowSensors = FlowSensors
  { s_insp_flow :: WithRedundancy Int32
  , s_exp_flow :: WithRedundancy Int32
  , s_air_in_flow :: WithRedundancy Int32
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

sensors :: Sensors
sensors = Sensors
  { s_motor    = motor_sensors
  , s_pressure = pressure_sensors
  , s_oxygen   = oxygen_sensors
  , s_flow     = flow_sensors
  }


-- | TODO a good way to process a stream of noisy flow sensor values.
inhale_accumulator :: Stream Int32
inhale_accumulator = sum `div` n
  where
  -- Sensors values with 8 initial values, so we can compute the average of
  -- the last 8.
  stream :: Stream Int32
  stream = [0,0,0,0,0,0,0,0] ++ next
  next :: Stream Int32
  next = principal (s_insp_flow (s_flow sensors))
  sum :: Stream Int32
  sum =        stream + drop 1 stream + drop 2 stream + drop 3 stream
      + drop 4 stream + drop 5 stream + drop 6 stream + drop 7 stream
  n :: Stream Int32
  n = constant 8

exhale_accumulator :: Stream Int32
exhale_accumulator = sum `div` n
  where
  stream :: Stream Int32
  stream = [0,0,0,0,0,0,0,0] ++ next
  next :: Stream Int32
  next = principal (s_exp_flow (s_flow sensors))
  sum :: Stream Int32
  sum =        stream + drop 1 stream + drop 2 stream + drop 3 stream
      + drop 4 stream + drop 5 stream + drop 6 stream + drop 7 stream
  n :: Stream Int32
  n = constant 8


-- | Use the inspiration flow to determine when an inhale happens.
--
-- Initial idea: the average of the last 4 samples must be above a threshold?
inhale :: Stream Bool
inhale = inhale_accumulator >= threshold
  where
  -- TODO good threshold?
  threshold :: Stream Int32
  threshold = constant 8

exhale :: Stream Bool
exhale = exhale_accumulator >= threshold
  where
  threshold :: Stream Int32
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
