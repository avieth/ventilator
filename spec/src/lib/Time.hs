{-# LANGUAGE RebindableSyntax #-}

module Time
 ( time_delta_us
 , every_us
 ) where

import Language.Copilot
import Prelude hiding ((++), (>=), div)

data Time = Time
  { -- | How many microseconds have passed since the last step of the program.
    c_time_delta_us :: Stream Word32
  }

-- | External declaration of time, left to be filled in by the C programmer
-- according to some accurate clock or simulation.
--
--   extern uint32_t t_delta_us
--
-- In order for the software to properly function, this must be sufficiently
-- small. FIXME the program must enter a failure mode if the delta is too large.
--
-- As an example, we have essentially the Nyquist sampling problem: the program
-- must step at least twice as often as the frequency of breathing, or else it
-- may/will miss breaths.
--
-- TODO TBD is there a more sensible way to deal with time?
time :: Time
time = Time
  { c_time_delta_us = extern "t_delta_us" (Just (Prelude.repeat 10))
  }

-- | Change in time in microseconds since the last frame.
time_delta_us :: Stream Word32
time_delta_us = [0] ++ c_time_delta_us time

-- | Is True whenever a given number of microseconds has passsed since the last
-- time it was True (or since 0 at the beginning).
every_us :: Word32 -> Stream Bool
every_us us = stream
  where
  stream = elapsed >= constant us
  elapsed = [0] ++ (if stream then constant 0 else elapsed + time_delta_us)
