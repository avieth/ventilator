{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE DataKinds #-}

module Controls
  ( Controls
  , controls

  , error_controls

  , bpm_limited
  , ie_ratio_limited
  , ie_inhale
  , ie_exhale

  , volume_limit_limited
  , pressure_limit_limited
  , peep_limited

  , cmv_mode
  , cmv_volume_goal_limited
  , cmv_pressure_goal_limited

  -- TODO move these to a different module, they are not controls.
  , global_max_flow
  , global_volume_min
  , global_volume_max
  , global_pressure_min
  , global_pressure_max
  ) where

import Prelude hiding ((==), (>), (>=), (<=), div)
import Language.Copilot

import qualified Util as Util (clamp)

-- | Organizational record for all operator control signals (user input).
data Controls = Controls
  { -- | Breaths per minute.
    c_bpm                :: Stream Word8
    -- | Ratio of inhale time to exhale time.
    --
    -- Ideally we would use a struct for this, but copilot support for
    -- structs seems lacking: you cannot really do
    --
    --   Stream SomeStruct -> Stream SomeStruct
    --
    -- so we'll use a uint16_t where the high bits are inhale, low bits are
    -- exhale.
    --
    -- Should not use this directy, but rather ie_ratio_limited, because this
    -- represents the raw user control choice which may not make any sense (0s
    -- for example).
  , c_ie_ratio           :: Stream IERatio

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
  { c_bpm               = extern "c_bpm"               Nothing
  , c_ie_ratio          = extern "c_ie_ratio"          Nothing
  , c_volume_limit      = extern "c_volume_limit"      Nothing
  , c_pressure_limit    = extern "c_pressure_limit"    Nothing
  , c_peep              = extern "c_peep"              Nothing
  , c_cmv_mode          = extern "c_cmv_mode"          Nothing
  , c_cmv_volume_goal   = extern "c_cmv_volume_goal"   Nothing
  , c_cmv_pressure_goal = extern "c_cmv_pressure_goal" Nothing
  }

-- | True if there is an error in the control values.
-- For instance, a ridiculously high or low volume limit, or a 0 in the
-- I:E ratio.
--
-- TODO use an error code.
error_controls :: Controls -> Stream Bool
error_controls _ = constant False

-- | Limit the breaths per minute to [6,40]
--
-- FIXME TODO determine whether these are sensible BPM limits.
bpm_limited :: Stream Word8
bpm_limited = if bpm <= 6 then 6 else if bpm >= 40 then 40 else bpm
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

-- | Inhale/exhale ratio: numerator (inhale) in high 8 bits, denominator
-- (exhale) in low 8 bits.
--
-- Would like to use a struct but copilot doesn't work so well with those.
type IERatio = Word16

-- | Gives the numerator and denominator of the I:E ratio subject to limits:
--
-- - neither is 0. If either is 0 it's set to 1.
-- - either can be greater (it is indeed possible to do longer inhales) but
--   the greater one is not more than 4 times the lesser one.
--
-- TODO revisit this. What are sensible constraints?
ie_ratio_limited :: Stream IERatio
ie_ratio_limited = ier_limited

  where

  ier :: Stream IERatio
  ier = c_ie_ratio controls

  ier_limited :: Stream IERatio
  ier_limited =
        ((cast inhale :: Stream Word16) .<<. constant (8 :: Word8))
    .|. ((cast exhale :: Stream Word16) .&. 0x00FF)

  inhale :: Stream Word8
  inhale =
    if numerator_nz > denominator_nz
    then if numerator_nz > (constant 4 * denominator_nz)
      then constant 4 * denominator_nz
      else numerator_nz
    else numerator_nz

  exhale :: Stream Word8
  exhale =
    if denominator_nz > numerator_nz
    then if denominator_nz > (constant 4 * numerator_nz)
      then constant 4 * numerator_nz
      else denominator_nz
    else denominator_nz

  numerator_nz :: Stream Word8
  numerator_nz = if numerator == 0 then 1 else numerator

  denominator_nz :: Stream Word8
  denominator_nz = if denominator == 0 then 1 else denominator

  -- Cast is unsafe because it's Word16 -> Word8, but we right-shifted so it's
  -- all good.
  numerator :: Stream Word8
  numerator = unsafeCast (ier .>>. constant (8 :: Word8))

  denominator :: Stream Word8
  denominator = unsafeCast (ier .&. 0x00FF)

ie_inhale :: Stream Word8
ie_inhale = unsafeCast (ie_ratio_limited .>>. constant (8 :: Word8))

ie_exhale :: Stream Word8
ie_exhale = unsafeCast (ie_ratio_limited .&. 0x00FF)

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

-- | Limited to 8-20 cmH2O
--
-- Does not use the global limited; those will be checked at all times and used
-- to determine alarms.
cmv_pressure_goal_limited :: Stream Word32
cmv_pressure_goal_limited = Util.clamp lower upper (c_cmv_pressure_goal controls)
  where
  -- ~ 8 cmH2O in Pa
  lower = constant 784
  -- ~ 20 cmH2O in Pa
  upper = constant 1960
