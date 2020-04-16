{-# LANGUAGE RebindableSyntax #-}

module State where

import Language.Copilot
import Prelude hiding ((++), (&&), (||), (==), (/=), (<=), (>), drop, not)

import Controls
import Sensors (encoder_position, encoder_position_low, low_switch, high_switch)
import qualified Sensors (inhale_accumulator)
import Motor (limit_motor_velocity)

import Mode.Mandatory (cmv)
--import Mode.Spontaneous (simv)

-- Due to the nature of copilot (streams must be defined _for all time_ and
-- there is no way to define a finite-time stream fragment) it seems to me we're
-- forced to define everything about the machine operating state together in
-- one module.

-- | The state of the ventilator:
-- - Calibrating
-- - Ready
-- - Running
-- - Sopped
-- - Resetting
-- - Failed
type State = Word8

sCALIBRATING :: State
sCALIBRATING = 0x00

sREADY :: State
sREADY = 0x01

sRUNNING :: State
sRUNNING = 0x02

sSTOPPED :: State
sSTOPPED = 0x03

sRESETTING :: State
sRESETTING = 0x04

sFAILED :: State
sFAILED = 0x80

state :: Stream State
state = current
  where
  current = [sCALIBRATING] ++ next
  next =
    if failure
    then constant sFAILED
    else if current == constant sCALIBRATING
    then
      if end_calibration
      then constant sREADY
      else constant sCALIBRATING
    else if current == constant sREADY
    then
      if button_start_high_edge
      then constant sRUNNING
      else constant sREADY
    else if current == constant sRUNNING
    then
      if button_stop_high_edge
      then constant sSTOPPED
      else constant sRUNNING
    else if current == constant sSTOPPED
    then
      if button_start_high_edge
      then constant sRESETTING
      else if button_stop_high_edge
      then constant sRESETTING
      else constant sSTOPPED
    else if current == constant sRESETTING
    then
      if end_reset
      then constant sREADY
      else constant sRESETTING
    else current

is_running :: Stream Bool
is_running = state == constant sRUNNING

motor_velocity :: Stream Int32
motor_velocity =
  if state == constant sFAILED
  then 0
  else if state == constant sCALIBRATING
  then velocity_calibrating
  else if state == constant sREADY
  then velocity_ready
  else if state == constant sRUNNING
  -- How to do this? We don't want to have to write out CMV and SIMV in this
  -- module. What do they need from us? Simply a time-reset signal I think.
  -- Yeah, should be enough to give a signal which is True whenever in running
  -- mode
  --
  --   mask = state == sRUNNING
  --
  -- CMV and SIMV can use that to reset their clocks.
  --
  -- NB: SIMV and SMV are  in fact fundamentally different; we can't just
  -- take CMV and throw in some signal which gives a spontaneous breath.
  -- SIMV does not have a strict cycle. It only has a max time since last
  -- breath.
  then
    if Controls.mode == Controls.mCMV
    then limit_motor_velocity (cmv is_running (constant False))
    else if Controls.mode == Controls.mSIMV
    then limit_motor_velocity (cmv is_running (Sensors.inhale_accumulator > constant 50))
    else constant 0
  else if state == constant sSTOPPED
  then 0
  else if state == constant sRESETTING
  then velocity_resetting
  else 0

-- | TODO define this.
failure :: Stream Bool
failure = constant False

-- | CALIBRATING

-- | True whenever the calibration phase needs to go back to 0.
-- We'll say that's whenever the machine enters the reset phase.
-- TODO better characterization.
must_recalibrate :: Stream Bool
must_recalibrate = state == constant sRESETTING

end_calibration :: Stream Bool
end_calibration = calibration_phase == constant 3

calibrated :: Stream Bool
calibrated = end_calibration

calibration_phase :: Stream Word8
calibration_phase = stream
  where
  stream = [0] ++ next
  next =
    if (stream /= 0) && must_recalibrate
    then 0
    else if (stream == 0) && low_switch
    then 1
    else if (stream == 1) && high_switch
    then 2
    else if (stream == 2) && low_switch
    then 3
    else stream

velocity_calibrating :: Stream Int32
velocity_calibrating =
  if calibration_direction
  then unsafeCast calibration_speed
  else -(unsafeCast calibration_speed)

-- | True means forward from low to high.
calibration_direction :: Stream Bool
calibration_direction = calibration_phase == 1 || calibration_phase == 3

calibration_speed :: Stream Word32
calibration_speed = if not calibrated then constant 30 else constant 0

-- | READY

-- | Hold the piston at low
velocity_ready :: Stream Int32
velocity_ready =
  if (encoder_position - encoder_position_low) <= 2
  then constant 0
  else constant (-15)

-- | RESET

velocity_resetting :: Stream Int32
velocity_resetting = constant (-15)

end_reset :: Stream Bool
end_reset = low_switch
