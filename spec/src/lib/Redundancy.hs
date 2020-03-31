{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Redundancy where

import Language.Copilot

-- | A principal stream and a redundant stream, with a stream of tolerances
-- (probably constant, but allows for dynamic recalibration).
data WithRedundancy t = WithRedundancy
  { principal :: Stream t
  , redundant :: Stream t
  , threshold :: Stream t
  }

-- | A stream which is true whenever the principal and redundant streams
-- differ by strictly more than the threshold.
redundancy_check :: forall t . (Typed t, Num t, Ord t) => WithRedundancy t -> Stream Bool
redundancy_check wr = delta > threshold wr

  where

  delta :: Stream t
  delta = if p >= r then p - r else r - p

  p :: Stream t
  p = principal wr

  r :: Stream t
  r = redundant wr
