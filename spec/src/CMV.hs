{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE DataKinds #-}

module CMV
  ( CMVControl
  , SpontaneousBreath
  , VMode (..)
  , cmv
  ) where

import Language.Copilot
import Prelude hiding ((++), (>=), (<), (<=), (||), (/=), (&&), div, not)
import Controls
import Cycle
import Time

-- | CMV control stream.
-- When this is False, CMV does not count.
-- When it goes to True, CMV exhales until the lower limit, then begins counting
-- and runnings its cycle according to BPM and I:E ratio.
type CMVControl = Stream Bool

-- | True when the patient has tried to inhale.
--
-- FIXME this is not CMV. Factor this better to be more clear.
type SpontaneousBreath = Stream Bool

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
cmv_cycle :: CMVControl -> SpontaneousBreath -> CMVCycle
cmv_cycle control spontaneous_breath = CMVCycle
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

  -- True whenever the subcycle should change due to time.
  subcycle_change :: Stream Bool
  subcycle_change = current_elapsed >= current_length

  -- The subcycle: true means inhaling, false means exhaling.
  -- Hold the previous value unless there is a reason to change:
  -- - the subcycle time is over
  -- - the control mask changed from false to true
  -- - the spontaneous breath stream is true and we're currently exhaling
  subcycle = [False] ++
    if control_change && control
    then false
    -- If the patient takes a breath and we're in the exhale phase, change
    -- to inhale.
    else if spontaneous_breath && not subcycle
    then not subcycle
    else if subcycle_change
    then not subcycle
    else subcycle

  -- Length of the current subcycle, determined by the UI parameters whenever
  -- it changes to inhale cycle.
  current_length = [0] ++ current_length'
  current_length' =
    if control_change && control
    -- FIXME appropriate? CMV just came on, so we always start with exhale?
    then exhale_duration_us
    else if subcycle_change
    then if not subcycle
      -- it's going to change to the inhale cycle.
      then inhale_duration_us
      else exhale_duration_us
    else current_length

  current_elapsed = [0] ++ current_elapsed'
  current_elapsed' =
    if control_change && control
    then 0
    else if not control
    then current_elapsed
    else if subcycle_change
    then 0
    else current_elapsed + time_delta_us

  -- The control stream with an initial value, so that we can detect a change.
  control_last :: Stream Bool
  control_last = [False] ++ control

  -- True when the control stream changed.
  control_change :: Stream Bool
  control_change = control /= control_last

-- | Continuous mandatory ventilation: fixed BPM and I:E ratio with either
-- a pressure or volume goal.
--
-- TODO take streams as parameters rather than using "globals".
--
-- TODO pressure control support.
-- TODO should respect PEEP, whether in VC or PC mode.
--
-- FIXME handles spontaneous breaths, so it's more general than CMV.
-- Can specialize to CMV by giving constant False for the spontaneous breath
-- stream.
--
cmv :: CMVControl -> SpontaneousBreath -> VMode
cmv control spontaneous_breath = VMode { vmode_volume_ml = volume_ml, vmode_interval_us = remaining_us }

  where

  volume_ml :: Stream Word32
  volume_ml = if inhaling then cmv_volume_goal_limited else 0

  inhaling = cmv_subcycle (cmv_cycle control spontaneous_breath)

  duration_us :: Stream Word32
  duration_us = cmv_current_length (cmv_cycle control spontaneous_breath)

  elapsed_us :: Stream Word32
  elapsed_us = cmv_current_elapsed (cmv_cycle control spontaneous_breath)

  remaining_us :: Stream Word32
  remaining_us = duration_us - elapsed_us
