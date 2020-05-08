{-|
Definition of the continuous mandatory ventilation mode.

This requires a lot of refinement.
1. How do we decide how fast to go?
2. Should we deal in acceleration rather than velocity?
3. Wbat happens if the piston fails to retract far enough during exhale?
4. How to integrate PEEP?
5. How to do pressure control?
6. How should the observed flow affect this?
-}

{-# LANGUAGE RebindableSyntax #-}

module Mode.Mandatory where

import Language.Copilot
import Prelude hiding ((++), (&&), (>=), (<=), (>=), (==), (<), (>), div, not)

import qualified Controls
import Cycle
import Time
import qualified Kinematics as Kin
import Sensors (encoder_position, encoder_position_low, low_switch_on,
  pressure, volume_integral)
import Motor (md_encoder_position)

-- | Gives the motor velocity (degrees per second) in CMV mode.
--
-- When inhaling, it uses the current volume estimate and the volume goal
-- in order to determine speed
cmv :: Stream Bool -> Stream Bool -> Stream Bool -> Stream Int32
cmv is_running spontaneous_in spontaneous_ex = local (subcycle_time_remaining is_running spontaneous_in spontaneous_ex) $ \t_remaining_us ->
  local (time_since_goal_reached_us is_running spontaneous_in spontaneous_ex) $ \t_goal_reached ->
    -- > 0 means inhaling.
    if t_remaining_us > 0
    then
      if Controls.cmv_mode == constant Controls.cmvVC
        then if t_goal_reached > 0 && t_goal_reached < 200000
        -- Pull back a bit once we reach the goal.
        then -15
        else if t_goal_reached >= 200000
              -- Then pull back all the way, even if the cycle doesn't say we should
               -- exhale yet.
        then -170
        else unsafeCast Controls.inhale_speed_dps
        
        -- Here we use a coarse estimate. It takes roughly 90 degrees to
        -- move 1000mL, so f := 1000/90 ~ 11
        -- We want a dps x such that
        --   x * (t_remaining_us / 1e6) * f = v
        -- where v is the remaining volume that we must move
        -- So, x = (1e6 * v) / (f * t_remaining_us)
        -- It's not exact but may be good enough.
        --else (constant 1000000 * unsafeCast (Controls.cmv_volume_goal_limited - v_now)) `div` (constant 11 * t_remaining_us)
      else if Controls.cmv_mode == constant Controls.cmvPC
        -- TODO use observed pressure and pressure goal.
        -- This is not actually settable at the moment so no big deal.
        then 0
      else 0
    -- < 0 means exhaling.
    else if t_remaining_us < 0
    -- Exhale is the same for VC and PC
    -- For exhale, we know how fast we have to go in order to get back to 0,
    -- so let's take that.
    -- FIXME what's a proper definition of exhale speed? What happens if we
    -- don't actually reach 0 on exhale?
    then if t_remaining_us > (constant (-10))
        then 0
        -- Always retract at a rate that would get us back to 0 in time, at
        -- the shortest exhale duration (60Hz, 1:1 ratio, meaning we have half
        -- a second to traverse the roughly 90 degrees).
        else -170

    else 0

-- | The time elapsed in microseconds since the pressure or volume goal was
-- reached during this inhale cycle, or 0 if it was not yet reached.
time_since_goal_reached_us
  :: Stream Bool
  -> Stream Bool
  -> Stream Bool
  -> Stream Word32
time_since_goal_reached_us is_running spontaneous_in spontaneous_ex = stream
  where
  stream = [0] ++ next
  next =
    if subcycle_time_remaining is_running spontaneous_in spontaneous_ex < 0
    -- This means exhale phase
    then 0
    -- Must reset state so that, for instance, volume or pressure goals reached
    -- during calibration mode don't infect state in running mode.
    else if reset is_running
    then 0
    --else if (stream == 0) && (Kin.volume_i >= Controls.cmv_volume_goal_limited)
    --Goal is multiplied by 1000 because the integral is in microlitres
    else if (stream == 0) && ((volume_integral (subcycle_time_remaining is_running spontaneous_in spontaneous_ex > 0) low_switch_on) >= (Controls.cmv_volume_goal_limited * constant 1000))
    then time_delta_us
    else if (stream == 0) && (Sensors.pressure >= Controls.cmv_pressure_goal_limited)
    then time_delta_us
    else if (stream > 0)
    then stream + time_delta_us
    else 0

-- | The subcycle of CMV: true means inhaling, false means exhaling.
type Subcycle = Bool

-- | Positive means inhaling and that much time is left (us).
-- Negative means exhaling and the negation is the time left (us).
-- 0 means neither inhaling nor exhaling...
-- Problem: if either the inhale or exhale duration is 0, we would get stuck in
-- a non-moving state. But that would be a problem case anyway...
subcycle_time_remaining
  :: Stream Bool
  -> Stream Bool
  -> Stream Bool
  -> Stream Int32
subcycle_time_remaining is_running spontaneous_in spontaneous_ex = stream

  where

  stream :: Stream Int32
  stream = [0] ++ next

  next :: Stream Int32
  next =
    if not is_running
    then stream
    else if reset is_running
    then unsafeCast inhale_duration_us
    -- If we're inhaling ...
    else if (stream > 0)
      then (if (stream <= unsafeCast time_delta_us)
      -- ... and the duration has ended, go to exhale ...
      then -(unsafeCast exhale_duration_us)
      -- ... or if there is sufficient expiratory flow, go to exhale, but only
      -- if the piston is not too near 0.
      else if spontaneous_ex && ((encoder_position - encoder_position_low) > 20)
      then -(unsafeCast exhale_duration_us)
      else stream - unsafeCast time_delta_us)
    -- If we're exhaling ...
    else if (stream < 0)
      then (if (stream >= (-(unsafeCast time_delta_us)))
      -- ... and the duration has ended, go to inhale ...
      then unsafeCast inhale_duration_us
      -- ... or if there's a spontaneous inhale, go to inhale, but only if the
      -- piston is near 0.
      else if spontaneous_in && ((encoder_position - encoder_position_low) < 20)
      then unsafeCast inhale_duration_us
      else stream + unsafeCast time_delta_us)
    else 0

reset :: Stream Bool -> Stream Bool
reset is_running = is_running && not ([False] ++ is_running)

inhaling :: Stream Bool -> Stream Bool -> Stream Bool -> Stream Bool
inhaling is_running spontaneous_in spontaneous_ex = subcycle_time_remaining is_running spontaneous_in spontaneous_ex > 0

exhaling :: Stream Bool -> Stream Bool -> Stream Bool -> Stream Bool
exhaling is_running spontaneous_in spontaneous_ex = subcycle_time_remaining is_running spontaneous_in spontaneous_ex < 0
