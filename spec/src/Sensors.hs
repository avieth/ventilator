module Sensors where

import Language.Copilot
import Redundancy

-- | Organizational record for sensors relating to the piston and motor.
data PistonSensors = PistonSensors
  { -- | The piston is at its lowest position (cannot pull any more air from
    -- the patient).
    s_piston_low  :: Stream Bool
    -- | The piston is at its highest position (cannot push any more air into
    -- the patient).
  , s_piston_high :: Stream Bool
  }

piston_sensors :: PistonSensors
piston_sensors = PistonSensors
  { s_piston_low  = extern "s_piston_low"  Nothing
  , s_piston_high = extern "s_piston_high" Nothing
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
-- TBD what will these be exactly and what are the units.
data PressureSensors = PressureSensors
  { s_internal_pressure :: WithRedundancy Int32
  }

pressure_sensors :: PressureSensors
pressure_sensors = PressureSensors
  { s_internal_pressure = WithRedundancy
      { principal = extern "s_internal_pressure_1" Nothing
      , redundant = extern "s_internal_pressure_2" Nothing
      , threshold = extern "s_internal_pressure_t" Nothing
      }
  }

-- | Organizational record for all sensor streams.
data Sensors = Sensors
  { s_piston   :: PistonSensors
  , s_pressure :: PressureSensors
  , s_oxygen   :: OxygenSensors
  }

sensors :: Sensors
sensors = Sensors
  { s_piston   = piston_sensors
  , s_pressure = pressure_sensors
  , s_oxygen   = oxygen_sensors
  }

-- | TODO compute from sensors? Or leave external?
flow :: Stream Int32
flow = extern "s_flow" Nothing

-- | TODO compute from sensors? Or leave external?
volume :: Stream Int32
volume = extern "s_volume" Nothing
