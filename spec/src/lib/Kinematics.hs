{-# LANGUAGE RebindableSyntax #-}

{-|
Definitions related to the physical properties of the assembly which this
software controls.
-}

module Kinematics
  ( forward_kinematics_mm
  , l
  , big_l
  , h
  , bellows_length_mm
  , bellows_diameter_mm
  , bellows_xsection_area_mm_2
  , start_position_mm
  , theta_max
  , theta_min
  , theta
  , volume_f
  , volume_i
  ) where

import Language.Copilot

import Motor
import Sensors

-- Transposed from Colin's arduino sketch.
--
-- FIXME can we make all of this integral?
-- The idea is that all of the floating point stuff can be pre-computed, from
-- the measurements of the system. A lookup table may be feasible...
-- Leaving it in floating point is a safety hazard. Errors may accumulate over
-- the course of the program and lead to too much or too little volume being
-- delivered.
-- One mitigation strategy is to set the volume to 0 whenever the low pin is
-- hit.

-- | The bellows is the thing that holds the air. This is its diameter in
-- millimeters.
bellows_diameter_mm :: Stream Double
bellows_diameter_mm = constant 102

-- | Area of a circular cross section of the bellows.
-- Will be used to compute volume.
-- Pre-computed value is given.
bellows_xsection_area_mm_2 :: Stream Double
bellows_xsection_area_mm_2 = 8171.282492
{-
bellows_xsection_area_mm_2 = pi * r * r
  where
  r = bellows_diameter_mm / 2.0
-}

-- | How long is the bellows: how far can the piston move.
-- theta ~= 30 should be the forward extremity.
bellows_length_mm :: Double
bellows_length_mm = 125.0

-- | The distance in millimeters from the motor shaft to the piston when
-- the piston is fully retracted
start_position_mm :: Double
start_position_mm = 90.120

-- min/max angles in the conventional ring-hand meaning: a lower angle means the
-- piston is farther from the shaft.
-- FIXME cleanup and clarify use; call one standard theta and the other
-- phi

-- | Max is when fully exhaled.
theta_max :: Stream Double
theta_max = theta_low

-- | Min is when fully inhaled (cannot push any more air).
theta_min :: Stream Double
theta_min = theta_high

-- | Precomputed highest angle (degrees).
theta_low :: Stream Double
theta_low = constant 110.51730062094212

-- | Precomputed lowest angle (degrees).
theta_high :: Stream Double
theta_high = constant 30.67270509390627

-- | Length of the arm piece attached to the motor, in millimeters.
l :: Stream Double
l = constant 85.0

-- | Length of the arm piece attached to the bellows and to the other
-- arm piece, in millimeters.
big_l :: Stream Double
big_l = constant 140.0

-- | How far above the rail is the joint between the bellows and the long
-- arm (big_l), in millimeters
h :: Stream Double
h = 75.0

l_squared :: Stream Double
l_squared = constant 7225.0
{- l_squared = l * l -}

big_l_squared :: Stream Double
big_l_squared = constant 14400.0
{- big_l_squared = big_l * big_l -}

-- | The volume delivered to the patient in cubic millimeters, given a
-- standard right-hand-rule angle (see forward_kinematics_mm_3)
--
-- For example: this would be maximal at 0
volume_delivered_mm_3 :: Stream Double -> Stream Double
volume_delivered_mm_3 theta =
  bellows_xsection_area_mm_2 * (forward_kinematics_mm theta - constant start_position_mm)

-- acos approximation
-- Stolen from stack overflow.
-- TODO FIXME analyze this and determine if it's suitable given our range of
-- angles.
-- https://stackoverflow.com/questions/3380628/fast-arc-cos-algorithm
fast_acos :: Stream Double -> Stream Double
fast_acos x =
  1.570796326794866
  + (-0.69813170079773212 * x * x - 0.87266462599716477) * x

-- | FIXME TODO analyze this cosine approximation. It's probably not
-- appropriate, I just took the easiest thing (3 terms of the taylor series)
-- and did not think about it.
fast_cos :: Stream Double -> Stream Double
fast_cos x = local (x * x) $ \x_square -> 
  1.0 - x_square * (0.25 + 0.04167 * x_square)

-- | FIXME TODO also check whether this sine approx makes any sense.
fast_sin :: Stream Double -> Stream Double
fast_sin x = local (x * x) $ \x_square ->
    x * ( 1 - (0.16666666 * x)
            + (0.00833333 * x_square * x_square)
          -- + (0.00019841 * x_square * x_square * x_square)
        )

-- TODO give a faster sqrt approximation if we think it's important.
fast_sqrt :: Stream Double -> Stream Double
fast_sqrt = sqrt

degrees_to_radians :: Stream Double -> Stream Double
degrees_to_radians d = pi * (d / 180.0)

radians_to_degrees :: Stream Double -> Stream Double
radians_to_degrees r = 180.0 * (r / pi)

-- | Given an angle, determine how far the piston has moved from zero,
-- in millimeters.
--
-- The angle is in standard right-hand-rule style: 0 would mean the motor has
-- pushed the piston all the way forward.
--
-- @
--          / ---
--        /  |    -----  L        ___________
--   l  /    |y_a        ------  |
--    /      |                 H |
--  /_ _ _ _ | _ _ _ _ _ _ _ _ _ |  bellows
--     x_a         x_b           |
--                               |___________
-- @
--
-- The input angle theta is the one on the left bottom.
forward_kinematics_mm :: Stream Double -> Stream Double
forward_kinematics_mm theta = x_a + x_b

  where

  theta_rad :: Stream Double
  theta_rad = degrees_to_radians theta

  x_a :: Stream Double
  x_a = l * cos theta_rad

  y_a :: Stream Double
  y_a = l * sin theta_rad

  -- Hopefully the terrible picture helps a bit to explain.
  -- We want x_a plus x_b where x_b is detremined by a trianglle not shown:
  -- the one with L and the hypoteneuse and y_a - h as the other side.

  x_b :: Stream Double
  x_b = fast_sqrt (big_l_squared - ((h - y_a) ** 2))

-- | The current right-hand-rule angle, determined by way of the motor position
-- encoder.
--
-- Will calculate often so this must be fast as possible.
--
-- We take the reverse kinematics at the start position, and then subtract
-- the number of degrees the motor has turned, according to the encoder.
--
-- When the encoder is at 0, we want theta to be such that
--   forward_kinematics_mm theta = 0
-- that's just to say,
--   theta = reverse_kinematics 0
theta :: Stream Double
theta = theta_low - unsafeCast offset
  where
  offset :: Stream Int32
  offset = md_encoder_position encoder_position encoder_position_low `div` 1000

-- | Volume as an integer in millilitres.
volume_i :: Stream Int32
volume_i = unsafeCast (unsafeCast (volume_f / 1000.0) :: Stream Int64)

-- | Volume in cubic millimiters.
volume_f :: Stream Double
volume_f = volume_delivered_mm_3 theta
