{-# LANGUAGE RebindableSyntax #-}

module Util where

import Language.Copilot
import Prelude hiding ((++), (<=), (>=))

-- | Ensure that a given stream is within the given bounds.
-- Lower bound is checked first so if the lower bound exceeds the upper bound
-- then the lower is given.
clamp
  :: (Ord a, Typed a)
  => Stream a -- ^ Lower bound
  -> Stream a -- ^ Upper bound
  -> Stream a
  -> Stream a
clamp lower upper x =
  if x <= lower
  then lower
  else if x >= upper
  then upper
  else x

-- | Integrate a stream with a given initial value.
integral :: (Num a, Eq a, Typed a) => a -> Stream a -> Stream a
integral c s = stream
  where
  stream = [c] ++ next
  next = s + stream