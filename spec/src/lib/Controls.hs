{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE DataKinds #-}

module Controls
  ( Controls
  , controls

  , Mode
  , mode
  , mCMV
  , mSIMV
  , button_start_high_edge
  , button_stop_high_edge

  , error_controls

  , bpm_limited
  , ie_inhale_limited
  , ie_exhale_limited

  , volume_limit_limited
  , pressure_limit_limited
  , peep_limited

  , cmv_mode
  , cmvVC
  , cmvPC
  , cmv_volume_goal_limited
  , cmv_pressure_goal_limited

  -- TODO move these to a different module, they are not controls.
  , global_max_flow
  , global_volume_min
  , global_volume_max
  , global_pressure_min
  , global_pressure_max
  ) where

import Prelude hiding ((++), (&&), (==), (>), (>=), (<=), div, drop, mod, not)
import Language.Copilot
import qualified Util as Util (clamp)

-- | "Macro" ventilation mode (CMV, SIMV, etc.)
type Mode = Word8

mCMV :: Stream Mode
mCMV = constant 0

mSIMV :: Stream Mode
mSIMV = constant 1

mode :: Stream Mode
mode = c_mode controls

-- | Organizational record for all operator control signals (user input).
data Controls = Controls
  { -- | Mode of operation (CMV, SIMV, etc.)
    c_mode      :: Stream Mode

    -- | Start button. True when pressed down.
  , c_button_start :: Stream Bool
    -- | Stop button. True when pressed down.
  , c_button_stop  :: Stream Bool

    -- | Breaths per minute.
  , c_bpm       :: Stream Word8

    -- | I:E ratio numerator: inhale
  , c_ie_inhale :: Stream Word8
    -- | I:E ratio denominator: exhale
  , c_ie_exhale :: Stream Word8

    -- | Global limit on the volume in the lungs (mL).
  , c_volume_limit       :: Stream Word32
    -- | Global limit on the pressure in the lungs (Pa).
  , c_pressure_limit     :: Stream Word32
    -- | Positive end-expiratory pressure setting (Pa).
  , c_peep               :: Stream Word32

    -- | CMV mode: True means VC, False means PC.
  , c_cmv_mode           :: Stream Bool
    -- | mL
  , c_cmv_volume_goal    :: Stream Word32
    -- | Pa
  , c_cmv_pressure_goal  :: Stream Word32
  }

-- | All controls are external (not derived).
controls :: Controls
controls = Controls
  { c_mode              = extern "c_mode"              Nothing
  , c_button_start      = extern "c_button_start"      Nothing
  , c_button_stop       = extern "c_button_stop"       Nothing
  , c_bpm               = extern "c_bpm"               Nothing
  , c_ie_inhale         = extern "c_ie_inhale"         Nothing
  , c_ie_exhale         = extern "c_ie_exhale"         Nothing
  , c_volume_limit      = extern "c_volume_limit"      Nothing
  , c_pressure_limit    = extern "c_pressure_limit"    Nothing
  , c_peep              = extern "c_peep"              Nothing
  , c_cmv_mode          = extern "c_cmv_mode"          Nothing
  , c_cmv_volume_goal   = extern "c_cmv_volume_goal"   Nothing
  , c_cmv_pressure_goal = extern "c_cmv_pressure_goal" Nothing
  }

-- | True whenever the start button becomes pressed (changed, high leading
-- edge).
button_start_high_edge :: Stream Bool
button_start_high_edge = a && not b
  where
  a = c_button_start controls
  b = [False] ++ c_button_start controls

-- | Like 'button_start_high_edge' but for the stop button.
button_stop_high_edge :: Stream Bool
button_stop_high_edge = a && not b
  where
  a = c_button_stop controls
  b = [False] ++ c_button_stop controls

-- | True if there is an error in the control values.
-- For instance, a ridiculously high or low volume limit, or a 0 in the
-- I:E ratio.
--
-- TODO use an error code.
error_controls :: Controls -> Stream Bool
error_controls _ = constant False

-- | Limit the breaths per minute to [1,60]
--
-- FIXME TODO determine whether these are sensible BPM limits.
bpm_limited :: Stream Word8
bpm_limited = if bpm <= 1 then 1 else if bpm >= 60 then 60 else bpm
  where
  bpm = c_bpm controls

-- Global static limits which
-- TODO must be set well with consultation.
--
-- There are user-controlled limits, but those limits must be limited to values
-- which are highly unlikely to be harmful.

-- | Max safe flow in uL/ms
-- 90 L/minute
-- TODO sensible value?
-- This is also limited by hardware of course.
global_max_flow :: Stream Word32
global_max_flow = constant 1500

global_pressure_max :: Stream Word32
global_pressure_max = constant 5000

-- TODO are we dealing with absolute or relative pressure? If relative we'll
-- need a signed integer.
-- Probably should deal in relative, since that's what the PEEP value is:
-- pressure above ambient.
global_pressure_min :: Stream Word32
global_pressure_min = constant 100

-- | mL
global_volume_max :: Stream Word32
global_volume_max = constant 1000

-- | mL
global_volume_min :: Stream Word32
global_volume_min = constant 100

-- | Limits on the global volume limit. Unit is mL.
--
-- FIXME sensible limits. Currently it's 1L max and 100mL min
volume_limit_limited :: Stream Word32
volume_limit_limited =
  if limit <= minL then minL else if limit >= maxL then maxL else limit
  where
  maxL  = global_volume_max
  minL  = global_volume_min
  limit = c_volume_limit controls

-- | Limits on the global pressure limit. Unit is Pa.
--
-- FIXME sensible limits. Currently it's 5000Pa max and 100Pa min.
pressure_limit_limited :: Stream Word32
pressure_limit_limited =
  if limit <= minL then minL else if limit >= maxL then maxL else limit
  where
  maxL  = global_pressure_max
  minL  = global_pressure_min
  limit = c_pressure_limit controls

-- | Positive end-expiratory pressure, in pascals.
--
-- Sane values for PEEP? It is relative to ambient pressure, and is positive
-- by definition/name, so 0 is the obvious lower bound. The idea is that PEEP
-- is always "on" but 0 means no PEEP.
--
-- TODO decide on a sensible upper bound. Apparently 10cmH2O is reasonable.
--
-- TODO use Int32 and guarantee it's positive and within bounds, so that other
-- parts of the program do not need to unsafeCast.
peep_limited :: Stream Word32
peep_limited =
  if peep <= minP then minP else if peep >= maxP then maxP else peep
  where
  -- Roughly 10 cmH2O
  maxP = constant 980
  minP = 0
  peep = c_peep controls

-- | The inhale portion of the I:E ratio is constrained by the exhale
-- portion, and may not be 0.
ie_inhale_limited :: Stream Word8
ie_inhale_limited =
  if ie_exhale_limited <= (4 * operator_choice)
  then operator_choice
  else if (ie_exhale_limited `mod` 4) == 0
  then ie_exhale_limited `div` 4
  else (ie_exhale_limited `div` 4) + 1
  where
  operator_choice = c_ie_inhale controls

-- | The exhale part of the I:E ratio may be anything other than 0.
-- The inhale portion will be constrained by this.
ie_exhale_limited :: Stream Word8
ie_exhale_limited = if operator_choice <= 0 then 1 else operator_choice
  where
  operator_choice = c_ie_exhale controls

cmvVC :: Bool
cmvVC = False

cmvPC :: Bool
cmvPC = True

-- | No sanity checking to do here, just give the control stream.
cmv_mode :: Stream Bool
cmv_mode = c_cmv_mode controls

-- | In milliliters
--
-- One recommendation says that 6-8mL/kg ideal body weight is healthy.
-- Since patient weights may vary, we'll allow for anywhere from 40-200kg.
-- TODO check this
--
-- Does not use the global limited; those will be checked at all times and used
-- to determine alarms.
cmv_volume_goal_limited :: Stream Word32
cmv_volume_goal_limited = Util.clamp lower upper (c_cmv_volume_goal controls)
  where
  lower = constant 0
  -- uL for 8mL/kg of a 200kg patient
  upper = constant 1600

-- | Limited to 0-40 cmH2O
--
-- Does not use the global limited; those will be checked at all times and used
-- to determine alarms.
cmv_pressure_goal_limited :: Stream Word32
cmv_pressure_goal_limited = Util.clamp lower upper (c_cmv_pressure_goal controls)
  where
  lower = constant 0
  upper = constant 4000
