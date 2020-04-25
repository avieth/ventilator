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
import Sensors (encoder_position, encoder_position_low)
import Motor (md_encoder_position)

-- | Gives the motor velocity (degrees per second) in CMV mode.
--
-- When inhaling, it uses the current volume estimate and the volume goal
-- in order to determine speed
cmv :: Stream Bool -> Stream Bool -> Stream Int32
cmv is_running spontaneous = local (subcycle_time_remaining is_running spontaneous) $ \t_remaining_us ->
  local Kin.volume_i $ \v_now ->
    -- > 0 means inhaling.
    if t_remaining_us > 0
    then
      if Controls.cmv_mode == constant Controls.cmvVC
        -- Kin.volume_i is in millimeters, as is the CMV volume goal.
        then if v_now >= (unsafeCast Controls.cmv_volume_goal_limited)
          then 0
          -- Must determine how fast we need to go in order to reach the goal.
          -- But that requires reverse kinematics: need to know the required
          -- angular delta. Reverse kinematics is expensive though.
          -- WHAT IF we compute that only at te beginning of the cycle?
          else if (unsafeCast Controls.cmv_volume_goal_limited - v_now) > 100
          then 55
          else 25
      else if Controls.cmv_mode == constant Controls.cmvPC
        -- TODO use observed pressure and pressure goal.
        then 25
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
        else -180

    else 0

-- | The subcycle of CMV: true means inhaling, false means exhaling.
type Subcycle = Bool

-- | Positive means inhaling and that much time is left (us).
-- Negative means exhaling and the negation is the time left (us).
-- 0 means neither inhaling nor exhaling...
-- Problem: if either the inhale or exhale duration is 0, we would get stuck in
-- a non-moving state. But that would be a problem case anyway...
subcycle_time_remaining :: Stream Bool -> Stream Bool -> Stream Int32
subcycle_time_remaining is_running spontaneous = stream

  where

  stream :: Stream Int32
  stream = [0] ++ next

  next :: Stream Int32
  next =
    if not is_running
    then stream
    else if reset is_running
    then unsafeCast inhale_duration_us
    else if (stream > 0)
      then (if (stream <= unsafeCast time_delta_us)
      -- Go to exhale
      then -(unsafeCast exhale_duration_us)
      else stream - unsafeCast time_delta_us)
    else if (stream < 0)
      then (if (stream >= (-(unsafeCast time_delta_us)))
      -- Go to inhale
      then unsafeCast inhale_duration_us
      else if spontaneous && ((encoder_position - encoder_position_low) < 100)
      then unsafeCast inhale_duration_us
      else stream + unsafeCast time_delta_us)
    else 0

reset :: Stream Bool -> Stream Bool
reset is_running = is_running && not ([False] ++ is_running)

inhaling :: Stream Bool -> Stream Bool -> Stream Bool
inhaling is_running spontaneous = subcycle_time_remaining is_running spontaneous > 0

exhaling :: Stream Bool -> Stream Bool -> Stream Bool
exhaling is_running spontaneous = subcycle_time_remaining is_running spontaneous < 0
