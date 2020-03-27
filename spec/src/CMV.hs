{-# LANGUAGE RebindableSyntax #-}

module CMV
  ( cmv_flow
  ) where

import Language.Copilot
import Prelude hiding ((++), (>=), (<), (<=), (||), div, not)
import Controls
import Cycle
import Sensors
import Redundancy
import Time

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

-- | Flow signal under CMV (`cmv_cycle`), i.e. desired change in volume at
-- an instant.
--
-- Property: the system volume must begin at 0 and increase during the inhale
-- phase, and return to 0 by the end of the exhale phase (begin with no flow,
-- end with no flow).
--
-- According to the CMV mode (volume or pressure control) this will give
-- positive flow until the relevant goal has been approximately reached.
--
-- TODO should respect PEEP, whether in VC or PC mode.
--
cmv_flow :: Stream Int32
cmv_flow =
  -- Check this so that we can safely divide by it otherwise.
  -- This _shouldn't_ happen in normal operation: UI controls should rule this
  -- out (BPM and I:E ratios are sensible values).
  -- However the initial value of the duration computed in `cmv_cycle` is
  -- indeed 0, although it will never appear here (it's the inital value to
  -- latch but the subcycle changes immediately).
  -- In fact, duration is in microseconds but we operate in milliseconds here,
  -- so we'll demand it's at least 1 millisecond.
  if duration < 1000
  then 0
  else if inhaling
    then inhale
    else exhale

  where

  -- Choose the inhale mode based on the CMV mode.
  -- Exhale is the same for either mode: just try to lower pressure.
  -- `user_cmv_mode` is true to indicate volume-control, false to indicate
  -- pressure-control.
  inhale :: Stream Int32
  inhale = if cmv_mode then inhale_vc else inhale_pc

  inhaling :: Stream Bool
  inhaling = cmv_subcycle cmv_cycle

  -- FIXME
  -- Must guard against duration every being 0.
  duration :: Stream Word32
  duration = cmv_current_length cmv_cycle

  -- Get the duration in milliseconds as a signed integer.
  -- Cast should be fine since we won't have a duration of 2^31 microseconds
  -- (35 minutes).
  --
  -- This is guaranteed to not be 0 since CMV is skipped when the duration is
  -- less than 1ms.
  duration_ms :: Stream Int32
  duration_ms = unsafeCast (duration `div` 1000)

  elapsed_ms :: Stream Int32
  elapsed_ms = unsafeCast (cmv_current_elapsed cmv_cycle `div` 1000)

  remaining_ms :: Stream Int32
  remaining_ms = duration_ms - elapsed_ms

  -- Target tidal volume (microlitres).
  --
  -- TODO must hold this value for the entire cycle; operator changes should
  -- not take effect mid-cycle.
  --
  -- unsafeCast is OK because the limit is within bounds.
  desired_volume :: Stream Int32
  desired_volume = unsafeCast cmv_volume_goal_limited

  -- Unsafe cast Word32 -> Int32 is OK because the unsigned is bounded well
  -- below 2^31
  desired_pressure :: Stream Int32
  desired_pressure = unsafeCast cmv_pressure_goal_limited

  observed_pressure :: Stream Int32
  observed_pressure = principal (s_internal_pressure (s_pressure sensors))

  observed_volume :: Stream Int32
  observed_volume = Sensors.volume

  -- The PEEP setting gives the pressure we want to maintain at the end of
  -- the cycle. We cast it to Int32; safe unless the PEEP is > 2^31 but that
  -- is ruled out by limits.
  end_pressure :: Stream Int32
  end_pressure = unsafeCast peep_limited

  -- VC inhale subcycle.
  -- Currently it's the simplest thing (probably too simple): linearly
  -- increase until the desired volume is reached.
  inhale_vc :: Stream Int32
  inhale_vc =
    if observed_volume >= desired_volume
    then 0
    else (desired_volume `div` duration_ms) + 1

  -- PC inhale subcycle.
  --
  -- The rate of pressure increase w.r.t. flow depends upon the patient (their
  -- lung compliance). This crude approximation of a sensible algorithm simply
  -- increases flow until pressure is reached, but does not do a good job of
  -- ensuring it ever is reached.
  --
  -- It also respects the operator-controlled tidal volume limit.
  inhale_pc :: Stream Int32
  inhale_pc =
    if (observed_pressure >= desired_pressure) || (observed_volume >= unsafeCast volume_limit_limited)
    then 0
    else 300

  -- The exhale subcycle is the same for VC and PC: decrease the flow until
  -- we reach the PEEP setting (positive end expiratory pressure).
  --
  -- TODO this will certainly need to be more clever, and respond to input
  -- signals.
  exhale :: Stream Int32
  exhale =
    if abs (end_pressure - observed_pressure) <= 10
    then 0
    -- TODO sound an alarm if we didn't reach the PEEP.
    else if remaining_ms <= 0
    then 0
    else -4 * ((observed_volume `div` remaining_ms) + 1)
