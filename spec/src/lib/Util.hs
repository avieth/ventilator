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

max :: (Ord a, Typed a) => Stream a -> Stream a -> Stream a
max a b = if a >= b then a else b

min :: (Ord a, Typed a) => Stream a -> Stream a -> Stream a
min a b = if a <= b then a else b

-- | Integrate a stream with a given initial value.
integral :: (Num a, Eq a, Typed a) => a -> Stream a -> Stream a
integral c s = stream
  where
  stream = [c] ++ next
  next = s + stream

-- | 'integral' but there's a Bool stream which resets the integral to a
-- given value whenever it is true.
integral_
  :: (Num a, Eq a, Typed a)
  => Stream Bool
  -> Stream a
  -> Stream a
  -> Stream a
integral_ reset c x = sums
  where
  sums = [0] ++ next_sums
  next_sums = if reset then c else x + sums

controlled_integral
  :: (Num a, Eq a, Typed a)
  => Stream Bool -- Reset to 0 when true
  -> Stream Bool -- Add only when this is true
  -> Stream a
  -> Stream a
controlled_integral reset active x = sums
  where
  sums = [0] ++ next_sums
  next_sums =
    if reset
    then 0
    else if active
    then x + sums
    else sums

