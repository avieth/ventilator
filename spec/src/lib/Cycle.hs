{-# LANGUAGE RebindableSyntax #-}

module Cycle
  ( cycle_duration_us
  , inhale_duration_us
  , exhale_duration_us
  ) where

import Prelude hiding ((==), div)
import Language.Copilot
import Controls

-- | Length of a breath cycle in microseconds. Derived from the BPM user
-- input stream with limits.
cycle_duration_us :: Stream Word32
cycle_duration_us = constant 60000000 `div` bpm
  where
  -- Cast is safe: Word8 -> Word32
  bpm :: Stream Word32
  bpm = cast bpm_limited

-- | Length of an inhale in microseconds. Derived from the BPM and I:E ratio.
inhale_duration_us :: Stream Word32
inhale_duration_us = (if x == 0 then 1 else x) * (cast ie_inhale_limited :: Stream Word32)
  where
  -- This division is OK. ie_inhale + ie_exhale is at most 383 (255 + 128)
  -- and the cycle duration has to be significantly larger than that.
  -- There is a lower bound on the BPM, giving a lower bound on the microseconds
  -- per cycle, which is probably at least 1e6 (1 breath per second).
  -- FIXME get advice of professionals on that limit.
  x :: Stream Word32
  x = cycle_duration_us `div` (cast (ie_inhale_limited + ie_exhale_limited) :: Stream Word32)

-- | Exhale duration. Sum of this and 'inhale_duration_us' is
-- 'cycle_duration_us'.
--
-- Could also use a similar computation to 'inhale_duration_us', but we'd rather
-- have any loss in the integral division of 'inhale_duration_us' be compensated
-- for here. i.e. we always have
--
-- > exhale_duration_us + inhale_duration_us = cycle_duration_us
--
exhale_duration_us :: Stream Word32
exhale_duration_us = cycle_duration_us - inhale_duration_us
