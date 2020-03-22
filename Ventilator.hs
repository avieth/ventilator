{-|
 - This is a [copilot](https://copilot-language.github.io/) program designed
 - to control a mechanical ventilatilation system.
 -
 - The program works with data streams (type `Stream Int32` for instance),
 - some of which are "extern" (set by a C program), and it defines "triggers",
 - which correspond to C functions which will be called whenever a given
 - condition (a `Stream Bool`) is true, and passed a set of values (sampled
 - from the specified `Stream`s).
 -
 - A program specification is a set of triggers, their condition signals, and
 - the arguments which they receive. This can be simulated in Haskell by giving
 - mock values for the "extern" signals. It can also be compiled to C with C
 - extern declarations for all "extern" signals as well as all declared
 - triggers.
 -
 - The specification for this program is called `spec` and is found near the
 - top of the program text.
 -}

{-# LANGUAGE RebindableSyntax #-}

module Ventilator where

import Language.Copilot
import Copilot.Compile.C99

-- Copilot redefines common notions to work over streams. We'll usually want
-- those.
import Prelude hiding ((++), (>), (>=), (<=), (<), (||), (&&), (/=), (==), (^),
  div, drop, not, min, max)
import qualified Prelude

-- | The spec for the ventilator program.
spec :: Spec
spec = do

  -- At every step (true :: Signal Bool) call into
  --
  --   void set_flow(int32_t flow)
  --
  -- where flow is the amount of flow which should be delivered to the
  -- patient.
  --
  -- TODO moving forward, we could instead compute the speed of the motor
  -- directly.
  trigger "set_flow" true [arg $ cmv_flow]

  -- Whenever the `alarm` signal is true, call into the C function
  --
  --   void raise_alarm(void)
  --
  -- with no arguments.
  -- TODO should have an alarm code.
  trigger "raise_alarm" alarm []

-- | Write out the spec to C in "ventilator.h" and "ventilator.c"
gen_c :: IO ()
gen_c = reify spec >>= compile "ventilator"

-- The rest of this document deals with the definition of the two streams
-- which are referenced in the triggers of `spec`.
--
--   cmv_flow :: Stream Int32
--   alarm :: Stream Bool
--
-- where `cmv_flow` is a simple continuous mandatory ventilation regime,
-- in which a breath is delivered n times per minute (BPM) with a given
-- inhale-to-exhale ratio, reaching a given desired pressure or volume.

-- |
-- # Sensor inputs
--
-- For each logic sensor we define two streams--one principal and one
-- redundant--corresponding to the two actual sensors. Deviation by some
-- threshold will cause an alarm.
--
-- In this definition we have sensors for
-- - flow
-- - pressure
-- - oxygen concentration
-- but the actual device does not correspond directly to this.
-- TODO define using the actual sensor configuration.

-- | Two sensors which estimate the same physical quantity, and an
-- acceptable threshold for their difference.
data WithRedundancy t = WithRedundancy
  { principal :: Stream t
  , check     :: Stream t
  , threshold :: t
  }

-- | Is True whenever the principal and check signals differ by at least the
-- threshold.
redundancy_check :: (Typed t, Num t, Ord t) => WithRedundancy t -> Stream Bool
redundancy_check wr = delta > constant (threshold wr)
  where
  delta = if p >= c then p - c else c - p
  p = principal wr
  c = check wr

-- | Declares two external signals, "name" and "name_check", of the same
-- type, with a given threshold for agreement.
with_redundancy :: Typed t => t -> String -> WithRedundancy t
with_redundancy t name = WithRedundancy
  { principal = extern name Nothing
  , check     = extern (name Prelude.++ "_check") Nothing
  , threshold = t
  }

-- | Flow in/out of the tube assembly.
-- TBD unit. Is 32-bit signed appropriate?
-- 1 mL/ms is pretty high. Let's use microlitres per millisecond.
-- Thershold is 42 because I don't know what a sensible threshold is; we'll
-- figure that out later.
flow :: WithRedundancy Int32
flow = with_redundancy 42 "flow"

-- | Volume of air delivered the lungs. We do not define it here as an integral
-- of flow, but leave it to the discretion of the system programmer. Obviously
-- it must maintain consistency with "flow".
volume :: Stream Int32
volume = extern "volume" Nothing

-- | Pressure in the lungs.
-- TBD unit? Pascals?
pressure :: WithRedundancy Int32
pressure = with_redundancy 42 "pressure"

-- | Concentration of oxygen to be delivered to the patient.
-- TBD unit? ppm?
o2_concentration :: WithRedundancy Word32
o2_concentration = with_redundancy 42 "o2_concentration"

-- |
-- # User/operator inputs
--
-- As with sensors, user inputs also determine streams. The user inputs are
-- listed below.

-- | Operator-controlled breaths-per-minute.
--
-- Mock stream is included with constant 12 BPM.
user_bpm :: Stream Word8
user_bpm = extern "user_bpm" (Just (Prelude.repeat 12))

-- | Operator-controlled inhale/exhale ratio. High 8 bits are inhale, low 8
-- bits are exhale, both Word8. Important that neither is ever 0.
--
-- A value of 0x0102, for example, means the inhale duration is half as long
-- as the exhale duration.
--
-- Mock stream is included with constant 1:2.
user_ie_ratio :: Stream Word16
user_ie_ratio = extern "user_ie_ratio" (Just (Prelude.repeat 0x0102))

-- | Tidal volume limit in microlitres.
user_volume_limit :: Stream Int32
user_volume_limit = extern "user_volume_limit" Nothing

-- | Desired pressure for CMV (inside the lungs).
--
-- TBD sensible unit? Pascals?
-- Actual ventilator displays seem to prefer cmH2O as their unit of
-- pressure. That's probably what the operator wants to input.
user_cmv_pressure_goal :: Stream Int32
user_cmv_pressure_goal = extern "user_cmv_pressure_goal" Nothing

-- | CMV mode: volume control (true) or pressure control (false).
user_cmv_mode :: Stream Bool
user_cmv_mode = extern "cmv_mode" Nothing

-- | Derived from the I:E ratio user input stream.
ie_ratio_numerator :: Stream Word16
ie_ratio_numerator = user_ie_ratio .>>. (constant (8 :: Word8))

-- | Derived from the I:E ratio user input stream.
ie_ratio_denominator :: Stream Word16
ie_ratio_denominator = user_ie_ratio .&. 0x00FF

-- | Length of a breath cycle in milliseconds. Derived from the BPM user
-- input stream.
cycle_duration_ms :: Stream Word32
cycle_duration_ms = constant 60000 `div` cast user_bpm

-- |
-- # Alarm signal

-- | Give True whenever human intervention is required.
--
-- As seen in `spec`, this signal controls when the C function raise_alarm will
-- be called. That function would probably sound a buzzer and flash some
-- lights.
--
-- TODO the alarm should give a code to indicate which signals are inconsistent,
-- so that the UI signal can use it.
-- Also should probably distinguish between critical alarms which imply motor
-- shutdown, and warnings which do not.
alarm :: Stream Bool
alarm = foldr (||) (constant False) checks
  where
  checks =
    [ redundancy_check flow
    , redundancy_check pressure
    , redundancy_check o2_concentration
    ]

-- | When the flow should be put to 0.
--
-- For now it's whenever there is an alarm. May need to be something nontrivial
-- moving forward.
kill_flow :: Stream Bool
kill_flow = alarm

-- |
-- # CMV signal
--
-- CMV is the simplest mode of ventilation to implement, because it is driven
-- only by time.
--
-- The goal is to define `cmv_flow :: Stream Int32` which gives the flow
-- rate at every instant.
--
-- It works by defining a `Signal Bool` which is true when we're inhaling, and
-- false when we're exhaling. The BPM determines the length of this cycle, and
-- the I:E ratio determines the length of the 2 subcycles.
--
-- To compute the flow, the signal switches depending upon the subcycle bool,
-- attempting to reach the volume or pressure goal when it's true (inhale) and
-- attempting to return to 0 while it's false (exhale).

-- | Length of an inhale in milliseconds. Derived from the BPM and I:E ratio.
--
-- Divide the cycle duration (derived from BPM) into I + E parts. The sum of
-- I of these gives the inhale duration. The exhale duration is taken to be
-- the difference between the cycle duration and this value, so that loss due
-- to integral division is accounted for in exhale.
inhale_duration_ms :: Stream Word32
inhale_duration_ms = x * cast ie_ratio_numerator
  where
  x :: Stream Word32
  x = cycle_duration_ms `div` cast (ie_ratio_numerator + ie_ratio_denominator)

-- | Exhale duration. Sum of this and inhale_duration is cycle_duration_ms.
-- Could also use a similar computation to inhale_duration_ms, but we'd rather
-- have any loss in the integral division of inhale_duration be compensated for
-- here. i.e. we always have
--
--   exhale_duration_ms + inhale_duration_ms = cycle_duration_ms
--
exhale_duration_ms :: Stream Word32
exhale_duration_ms = cycle_duration_ms - inhale_duration_ms

-- | How many milliseconds have passed since the last step.
--
-- This is left to be filled in by the C programmer. A variable
--
--   extern uint32_t time_delta_ms
--
-- will be declared and it must be set before each step, to say how many
-- milliseconds have passed since the last step.
--
-- In order for the software to properly function, this must be sufficiently
-- small.
--
-- TODO TBD is there a more sensible way to deal with time?
time_delta_ms :: Stream Word32
time_delta_ms = extern "time_delta_ms" (Just (Prelude.repeat 10))

-- | Total time elapsed. Overflow around 50 days.
-- Not actually needed at the moment.
time_ms :: Stream Word32
time_ms = integral 0 time_delta_ms

integral :: (Typed a, Num a, Eq a) => a -> Stream a -> Stream a
integral c f = stream
  where
  nexts = f + stream
  stream = [c] ++ nexts

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
    -- | How long is the current subcycle.
  , cmv_current_length  :: Stream Word32
    -- | How long has the current cycle been active.
    -- Always less than cmv_current_length.
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
      then inhale_duration_ms
      else exhale_duration_ms
    else current_length

  current_elapsed = [0] ++ current_elapsed'
  current_elapsed' =
    if change
    then 0
    else current_elapsed + time_delta_ms

-- | Flow signal under CMV (`cmv_cycle`), i.e. desired change in volume at
-- an instant.
--
-- Property: the system volume must begin at 0 and increase during the inhale
-- phase, and return to 0 by the end of the exhale phase.
--
-- A simple model: go up to a given maximum flow for inhale phase, then
-- drop to a minimum negative flow and move towards 0 for exhale.
cmv_flow :: Stream Int32
cmv_flow = if inhaling then inhale else exhale

  where

  -- Choose the inhale mode based on the CMV mode.
  -- Exhale is the same for either mode: just try to lower pressure.
  -- `user_cmv_mode` is true to indicate volume-control, false to indicate
  -- pressure-control.
  inhale :: Stream Int32
  inhale = if user_cmv_mode then inhale_vc else inhale_pc

  inhaling :: Stream Bool
  inhaling = cmv_subcycle cmv_cycle

  -- Must guard against duration every being 0.
  duration :: Stream Word32
  duration = cmv_current_length cmv_cycle

  {-
  elapsed :: Stream Word32
  elapsed = cmv_current_elapsed cmv_cycle

  -- Cast Word32 -> Int32 is unsafe because yes it may overflow. But that
  -- would imply that the time remaining is greater than 2^31 which is quite
  -- a long time for a breath.
  remaining :: Stream Int32
  remaining = unsafeCast (duration - elapsed)
  -}

  -- Target tidal volume (microlitres).
  -- TODO should be allowed to be different from the global limit, which also
  -- affects PC.
  desired_volume :: Stream Int32
  desired_volume = user_volume_limit

  desired_pressure :: Stream Int32
  desired_pressure = user_cmv_pressure_goal

  -- VC inhale subcycle.
  -- Currently it's the simplest thing (probably too simple): linearly
  -- increase until the desired volume is reached.
  inhale_vc :: Stream Int32
  inhale_vc =
    if volume >= desired_volume
    then 0
    else (desired_volume `div` unsafeCast duration) + 1

  -- PC inhale subcycle.
  --
  -- The rate of pressure increase w.r.t. flow depends upon the patient (their
  -- lung compliance). This crude approximation of a sensible algorithm simply
  -- increases flow until pressure is reached, but does not do a good job of
  -- insuring it ever is reached.
  --
  -- It also respects the operator-controlled tidal volume limit.
  inhale_pc :: Stream Int32
  inhale_pc =
    if (principal pressure >= desired_pressure) || (volume >= user_volume_limit)
    then 0
    else (user_volume_limit `div` unsafeCast duration) + 1

  -- The exhale subcycle is the same for VC and PC: decrease flow linearly
  -- in such a way that we expect volume to return to 0.
  -- TODO this will certainly need to be more clever, and respond to input
  -- signals.
  exhale :: Stream Int32
  exhale =
    if volume <= 0
    then 0
    else -((user_volume_limit `div` unsafeCast duration) + 1)
