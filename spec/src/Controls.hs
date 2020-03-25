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
  , global_volume_min
  , global_volume_max
  , global_pressure_min
  , global_pressure_max
  ) where

import Prelude hiding ((==), (>), (>=), (<=), div)
import Language.Copilot

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

    -- | Global limit on the volume in the lungs (uL).
  , c_volume_limit       :: Stream Word32
    -- | Global limit on the pressure in the lungs (Pa).
  , c_pressure_limit     :: Stream Word32
    -- | Positive end-expiratory pressure setting (Pa).
  , c_peep               :: Stream Word32

    -- | CMV mode: True means VC, False means PC.
  , c_cmv_mode           :: Stream Bool
    -- | uL
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

-- FIXME TODO determine sensible BPM limits.
bpm_limited :: Stream Word8
bpm_limited = if bpm <= 6 then 6 else if bpm >= 60 then 60 else bpm
  where
  bpm = c_bpm controls

-- Global static limits which
-- TODO must be set well with consultation.
--
-- There are user-controlled limits, but those limits must be limited to values
-- which are highly unlikely to be harmful.

global_pressure_max :: Stream Word32
global_pressure_max = constant 5000

global_pressure_min :: Stream Word32
global_pressure_min = constant 100

global_volume_max :: Stream Word32
global_volume_max = constant 1000000

global_volume_min :: Stream Word32
global_volume_min = constant 100000

-- | Limits on the global volume limit. Unit is uL.
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

-- | Sane values for PEEP? It is relative to ambient pressure, and is positive
-- by definition/name, so 0 is the obvious lower bound.
-- TODO decide on a sensible upper bound.
peep_limited :: Stream Word32
peep_limited =
  if peep <= minP then minP else if peep >= maxP then maxP else peep
  where
  -- Roughly 10 cmH2O
  maxP = constant 500
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
-- - inhale is not greater than exhale. If it is, it's set to 1:1
-- - exhale is at most 4 times inhale.
--
-- TODO revisit this. What are sensible constraints?
ie_ratio_limited :: Stream IERatio
ie_ratio_limited = ier_limited

  where

  ier :: Stream IERatio
  ier = c_ie_ratio controls

  ier_limited :: Stream IERatio
  ier_limited =
        ((cast limited :: Stream Word16) .<<. constant (8 :: Word8))
    .|. ((cast denominator_nz :: Stream Word16) .&. 0x00FF)

  limited :: Stream Word8
  limited =
    if numerator_nz > denominator_nz
    then denominator_nz
    else if (cast denominator_nz :: Stream Word16) > ((cast numerator_nz :: Stream Word16) * constant 4)
    then (denominator_nz `div` 4) + 1
    else numerator_nz

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

cmv_volume_goal_limited :: Stream Word32
cmv_volume_goal_limited =
  if goal <= global_volume_min
  then global_volume_min
  else if goal >= volume_limit_limited
  then volume_limit_limited
  else goal
  where
  goal = c_cmv_volume_goal controls

cmv_pressure_goal_limited :: Stream Word32
cmv_pressure_goal_limited =
  if goal <= global_pressure_min
  then global_pressure_min
  else if goal >= pressure_limit_limited
  then pressure_limit_limited
  else goal
  where
  goal = c_cmv_pressure_goal controls
