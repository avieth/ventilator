{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE DataKinds #-}

module CMV
  ( VMode (..)
  , cmv
  ) where

import Language.Copilot
import Prelude hiding ((++), (>=), (<), (<=), (||), div, not)
import Controls
import Cycle
import Time

-- | Organizational record for a ventilation mode: it says what the volume
-- goal is and how much time is left to reach it.
data VMode = VMode
  { vmode_volume_ml   :: Stream Word32
  , vmode_interval_us :: Stream Word32
  }

-- | Streams associated with continuous mandatory ventilation:
--
-- - Which subcycle are we in (inhale or exhale)
-- - How long have we been in this subcycle
-- - How long does this subcycle last
--
-- This Haskell record is just an organizational tool, it has no effect on
-- the generated C.
data CMVCycle = CMVCycle
  { -- | Which subcycle are we in: true for inhale, false for exhale.
    cmv_subcycle        :: Stream Bool
    -- | How long is the current subcycle (microseconds).
  , cmv_current_length  :: Stream Word32
    -- | How long has the current cycle been active (microseconds).
    -- Always less than `cmv_current_length`.
  , cmv_current_elapsed :: Stream Word32
  }

-- | Continuous mandatory ventilation according to the user input BPM and I:E
-- parameters. 
--
-- Input params do not affect the ongoing cycle. That's to say, when the
-- operator changes the parameters, they will not affect the breathing cycle
-- until the next inhale cycle begins.
--
-- FIXME this is probably bad UX.
-- It'd be better to have a visualization of the breathing pattern under new
-- settings, and a commit button to start it on the next cycle.
-- We also need sensible minimums: a BPM of less than 6 is probably not a great
-- idea. Not all I:E ratios are sensible either.
cmv_cycle :: CMVCycle
cmv_cycle = CMVCycle
  { cmv_subcycle = subcycle
  , cmv_current_length = current_length
  , cmv_current_elapsed = current_elapsed
  }

  where

  -- Initial values for this triple are
  --   Exhale cycle (False)
  --   0 length
  --   0 time elapsed
  -- and the system will immediately flip into the inhale subcycle using the
  -- BPM and I:E parameters.

  -- True whenever the subcycle should change.
  change :: Stream Bool
  change = current_elapsed >= current_length

  -- Hold the previous value unless the most recent elapsed time exceeds the
  -- length of the subcycle.
  subcycle = [False] ++ if change then not subcycle else subcycle

  -- Length of the current subcycle, determined by the UI parameters whenever
  -- it changes to inhale cycle.
  current_length = [0] ++ current_length'
  current_length' =
    if change
    then if not subcycle
      -- it's going to change to the inhale cycle.
      then inhale_duration_us
      else exhale_duration_us
    else current_length

  current_elapsed = [0] ++ current_elapsed'
  current_elapsed' =
    if change
    then 0
    else current_elapsed + time_delta_us

-- | Continuous mandatory ventilation: fixed BPM and I:E ratio with either
-- a pressure or volume goal.
--
-- TODO take streams as parameters rather than using "globals".
--
-- TODO pressure control support.
-- TODO should respect PEEP, whether in VC or PC mode.
--
cmv :: VMode
cmv = VMode { vmode_volume_ml = volume_ml, vmode_interval_us = remaining_us }

  where

  volume_ml :: Stream Word32
  volume_ml = if inhaling then cmv_volume_goal_limited else 0

  inhaling = cmv_subcycle cmv_cycle

  duration_us :: Stream Word32
  duration_us = cmv_current_length cmv_cycle

  elapsed_us :: Stream Word32
  elapsed_us = cmv_current_elapsed cmv_cycle

  remaining_us :: Stream Word32
  remaining_us = duration_us - elapsed_us
