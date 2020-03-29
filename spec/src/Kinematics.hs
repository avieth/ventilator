{-# LANGUAGE RebindableSyntax #-}

module Kinematics where

import Language.Copilot

import Sensors
import Motor (degrees_per_step)

-- Transposed from Colin's arduino sketch.

-- TODO can we make all of this integral?
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
bellows_xsection_area_mm_2 :: Stream Double
bellows_xsection_area_mm_2 = pi * r * r
  where
  r = bellows_diameter_mm / 2.0

-- | Length of the arm piece attached to the motor, in millimeters.
l :: Stream Double
l = constant 85.0

-- | Length of the arm piece attached to the bellows and to the other
-- arm piece, in millimeters.
big_l :: Stream Double
big_l = constant 120.0

l_squared :: Stream Double
l_squared = l * l

big_l_squared :: Stream Double
big_l_squared = big_l * big_l

-- | The distance in millimeters from the motor shaft to the piston when
-- the piston is fully retracted
start_position :: Double
start_position = 60

-- | Compute the right-hand-rule angle from the encoder, suitable for use in
-- `volume_delivered_mm_3` and `forward_kinematics_mm`.
-- Input is the degrees from 0 reference, i.e. degrees traversed by the motor
-- in the "pushing air" direction.
theta_from_encoder :: Stream Double -> Stream Double
theta_from_encoder offset = 180.0 - (base + offset)
  where
  base :: Stream Double
  base = reverse_kinematics (constant start_position)

-- | The volume delivered to the patient in cubic millimeters, given a
-- standard right-hand-rule angle (see forward_kinematics_mm_3)
--
-- For example: this would be maximal at 0
volume_delivered_mm_3 :: Stream Double -> Stream Double
volume_delivered_mm_3 theta =
  bellows_xsection_area_mm_2 * (forward_kinematics_mm theta - constant start_position)

-- | Invert volume_delivered_mm_3
-- TODO document and deal with edge cases. What if the input is more than
-- could possibly be delivered?
inverse_volume_delivered :: Stream Double -> Stream Double
inverse_volume_delivered mm3 = reverse_kinematics (x_mm + constant start_position)
  where
  x_mm = mm3 / bellows_xsection_area_mm_2

-- | Given how far the piston lies to the right of the motor shaft, determine
-- the angle in standard right-hand-rule (see `forward_kinematics_mm` which
-- should invert this).
-- TODO property tests for inversion (with suitable FP error theshold).
--
--
--   \  ----
--  |  \     ----   L
-- h|  l \         ----
--  |      \  theta      ----
--  |________\_________________----
--     x_a          x_b
--               x
--
-- We're interested in the angle to the left of theta, call it phi. We'll
-- compute that and subtract it from pi to get the radians, then convert to
-- degrees.
--
-- The input is the length of the bottom edge to the right of the motor
-- shaft, x_b. x_a is the length of the bottom of the triangle with h and l as
-- other side lengths. x = x_a + x_b
--
-- We have
--
--   1. phi = acos(x_a/l)
--   2. (x_a + x_b)^2 + h^2 = L^2
--   3. h^2 = l^2 - x_a^2
--
-- From 2 and 3 we get
--
--   x_a = (L^2 - l^2 - x_b^2) / 2*x_b
--
-- then we take the arccos and convert to degrees.
--
-- CAREFUL: this can be NaN if your start position does not make any sense
-- given the other physical values l and big_l
reverse_kinematics :: Stream Double -> Stream Double
reverse_kinematics start_position = radians_to_degrees (pi - phi)

  where

  phi = acos (x_a / l)

  x_a :: Stream Double
  x_a = (big_l_squared - l_squared - x_b * x_b) / (2 * x_b)

  x_b :: Stream Double
  x_b = start_position


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
--          / ---
--        /  |    ---  L          ___________
--   l  /    |y        ---       |
--    /      |            ---    |
--  /________|_______________ ---|  bellows
--     x_a         x_b           |
--                               |___________
-- 
-- The input angle theta is the one on the left bottom.
forward_kinematics_mm :: Stream Double -> Stream Double
forward_kinematics_mm theta = x_a + x_b

  where

  theta_rad :: Stream Double
  theta_rad = degrees_to_radians theta

  x_a :: Stream Double
  x_a = l * cos theta_rad

  -- We know that
  --
  --   y^2 + x_b^2 = L^2
  --   y^2 + x_a^2 = l^2
  --
  -- so we can solve for x_b
  --
  --   x_b^2 = L^2 - y^2
  --   x_b^2 = L^2 - (l^2 - x_a^2)
  --   x_b   = sqrt (L^2 - l^2 + x_a^2)
  --
  x_b :: Stream Double
  x_b = sqrt (big_l_squared - l_squared + x_a * x_a)

  y :: Stream Double
  y = l * sin theta_rad

-- | The current right-hand-rule angle, determined by way of the motor position
-- encoder.
--
-- When the encoder is at 0, we want theta to be such that
--   forward_kinematics_mm theta = 0
-- that's just to say,
--   theta = reverse_kinematics 0
theta :: Stream Double
theta = reverse_kinematics (constant start_position + degrees_per_step * full_steps)
  where
  encoder_steps = s_encoder_position (s_motor sensors)
  -- Encoder bumps 4 times per step I think...
  full_steps = unsafeCast (encoder_steps `div` 4)

volume_f :: Stream Double
volume_f = volume_delivered_mm_3 theta
