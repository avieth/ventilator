{-# LANGUAGE RebindableSyntax #-}

module Mode.Spontaneous where

import Language.Copilot
import Prelude hiding ((++), (&&), (>=), (<=), (>), (<), (==), div, drop, not)

import qualified Controls
import Cycle
import qualified Kinematics as Kin
import qualified Mode.Mandatory as Mandatory
import Motor (md_encoder_position)
import Sensors
import Time

simv :: Stream Bool -> Stream Int32
simv is_running = local (subcycle is_running) $ \sc ->
  local (Mandatory.subcycle_time_remaining is_running) $ \tr_us ->
  local Kin.volume_i $ \v_now ->
    if sc == 0
    -- mandatory inhale
    then if Controls.cmv_mode == constant Controls.cmvVC
      then if v_now >= (unsafeCast Controls.cmv_volume_goal_limited)
      then 0
      else if (unsafeCast Controls.cmv_volume_goal_limited - v_now) > 100
      then 90
      else 45
      else if Controls.cmv_mode == constant Controls.cmvPC
      then 45
      else 0
    else if sc == 1
    -- exhale
    then if tr_us > (constant (-10))
      then 0
      else ((md_encoder_position encoder_position_low encoder_position) * 2000) `div` (-tr_us)
    else if sc == 2
    -- spontaneous inhale
    then 30
    else 0

-- Subcycle codes:
-- 0 : mandatory inhale
-- 1 : exhale
-- 2 : spontaneous inhale
subcycle :: Stream Bool -> Stream Word8
subcycle is_running = stream

  where

  stream :: Stream Word8
  stream = [0] ++ next

  -- Problem: the subcycle time remaining stream from CMV is used here but
  -- it must not be, because it doesn't reset the time for exhale after a
  -- spontaneous breath. How to fix this?
  next :: Stream Word8
  next =
    if Mandatory.reset is_running
    then 0
    else if stream == constant 0
    then if Mandatory.subcycle_time_remaining is_running <= 0
      then constant 1
      else constant 0
    else if stream == constant 1
    then
      if inhale_accumulator > 10
      -- Spontaneous inhale on exhale cycle
      then constant 2
      -- Exhale phase expired
      -- Problem: this timer is not correct, if there's a spontaneous inhale.
      else if Mandatory.subcycle_time_remaining is_running >= 0
      then constant 0
      else constant 1
    else if stream == constant 2
    then if inhale_accumulator < 10
      -- Inhale stopped, go back to exhale.
      then constant 1
      else constant 2
    else stream
