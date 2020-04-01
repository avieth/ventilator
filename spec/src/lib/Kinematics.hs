{-# LANGUAGE RebindableSyntax #-}

{-|
Definitions related to the physical properties of the assembly which this
software controls.
-}

module Kinematics
  ( forward_kinematics_mm
  , reverse_kinematics
  , l
  , big_l
  , bellows_length_mm
  , bellows_diameter_mm
  , bellows_xsection_area_mm_2
  , start_position_mm
  , theta_max
  , theta_min
  , theta
  , inverse_volume_delivered
  , volume_f
  , flow_f
  , flow_f_observed
  ) where

import Language.Copilot

import Sensors
import Motor (md_per_pulse)

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
start_position_mm = 60.0

-- min/max angles in the conventional ring-hand meaning: a lower angle means the
-- piston is farther from the shaft.
-- FIXME cleanup and clarify use; call one standard theta and the other
-- phi

-- | Max is when fully exhaled.
theta_max :: Stream Double
theta_max = reverse_kinematics_at_start

-- | Min is when fully inhaled (cannot push any more air).
theta_min :: Stream Double
theta_min = reverse_kinematics_at_end

-- | Precomputed highest angle (degrees). Could also use reverse_kinematics to
-- get this but that is not cheap.
reverse_kinematics_at_start :: Stream Double
reverse_kinematics_at_start = constant 110.51730062094212

-- | Precomputed lowest angle (degrees). Could also use reverse_kinematics to
-- get this but that is not cheap.
reverse_kinematics_at_end :: Stream Double
reverse_kinematics_at_end = constant 30.67270509390627

-- | Length of the arm piece attached to the motor, in millimeters.
l :: Stream Double
l = constant 85.0

-- | Length of the arm piece attached to the bellows and to the other
-- arm piece, in millimeters.
big_l :: Stream Double
big_l = constant 120.0

l_squared :: Stream Double
l_squared = constant 7225.0
{- l_squared = l * l -}

big_l_squared :: Stream Double
big_l_squared = constant 14400.0
{- big_l_squared = big_l * big_l -}

-- | Precompute this as it appears in forward and reverse kinematics
-- computations.
big_l_squared_minus_l_squared :: Stream Double
big_l_squared_minus_l_squared = constant 7175.0

-- | Compute the right-hand-rule angle from the encoder, suitable for use in
-- `volume_delivered_mm_3` and `forward_kinematics_mm`.
-- Input is the degrees from 0 reference, i.e. degrees traversed by the motor
-- in the "pushing air" direction.
theta_from_encoder :: Stream Double -> Stream Double
theta_from_encoder offset = 180.0 - (reverse_kinematics_at_start + offset)

-- | The volume delivered to the patient in cubic millimeters, given a
-- standard right-hand-rule angle (see forward_kinematics_mm_3)
--
-- For example: this would be maximal at 0
volume_delivered_mm_3 :: Stream Double -> Stream Double
volume_delivered_mm_3 theta =
  bellows_xsection_area_mm_2 * (forward_kinematics_mm theta - constant start_position_mm)

-- | Invert volume_delivered_mm_3: which theta would give that much volume.
--
-- TODO document and deal with edge cases. What if the input is more than
-- could possibly be delivered?
inverse_volume_delivered :: Stream Double -> Stream Double
inverse_volume_delivered mm3 = reverse_kinematics (x_mm + constant start_position_mm)
  where
  x_mm = mm3 / bellows_xsection_area_mm_2

-- | Given how far the piston lies to the right of the motor shaft, determine
-- the angle in standard right-hand-rule (see `forward_kinematics_mm` which
-- should invert this).
--
-- @
--
--   \\  ----
--  |  \\     ----   L
-- h|  l \\         ----
--  |      \\  theta      ----      |
--  |_ _ _ _ \\ _ _ _ _ _ _ _ _ ----|
--     x_a          x_b             |
--               x
-- @
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
--   1. @phi = acos(x_a/l)@
--   2. @(x_a + x_b)^2 + h^2 = L^2@
--   3. @h^2 = l^2 - x_a^2@
--
-- From 2 and 3 we get
--
-- @
--   x_a = (L^2 - l^2 - x_b^2) / 2*x_b
-- @
--
-- then we take the arccos and convert to degrees.
--
-- CAREFUL: this can be NaN if your start position does not make any sense
-- given the other physical values l and big_l
reverse_kinematics :: Stream Double -> Stream Double
reverse_kinematics start_position = radians_to_degrees (pi - phi)

  where

  phi = fast_acos (x_a / l)

  x_a :: Stream Double
  x_a = (big_l_squared_minus_l_squared - x_b * x_b) / (2 * x_b)

  x_b :: Stream Double
  x_b = start_position

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
--        /  |    ---  L          ___________
--   l  /    |y        ---       |
--    /      |            ---    |
--  /_ _ _ _ | _ _ _ _ _ _ _ _---|  bellows
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
  x_b = fast_sqrt (big_l_squared_minus_l_squared + x_a * x_a)

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
theta = reverse_kinematics_at_start - unsafeCast offset
  where
  offset :: Stream Int32
  offset = ((unsafeCast md_per_pulse) * encoder_pulses) `div` 1000
  encoder_pulses :: Stream Int32
  encoder_pulses = s_encoder_position (s_motor sensors)

volume_f :: Stream Double
volume_f = volume_delivered_mm_3 theta

-- | Estimate the rate of change in volume at an instant for a given motor
-- velocity (L/s)
--
-- That's to say, the derivative w.r.t. time of
--
--   volume_delivered_mm_3 . theta
--
-- (theta is, though not explicitly written as such in this program text, a
-- function of time, as all streams are)
--
-- For the sake of speed, we precompute the derivative of
-- volume_delivered_mm_3 w.r.t. theta.
--
--
-- Alternatively, we could just compute the volume from theta every n
-- milisecond and take the difference.
flow_f :: Stream Int32 -> Stream Double
flow_f v = rad_theta_over_time * v_over_rad_theta

  where

  v_over_rad_theta :: Stream Double
  v_over_rad_theta = local (degrees_to_radians theta) $ \rad_theta ->
    local (fast_sin rad_theta) $ \sin_theta ->
    local (fast_cos rad_theta) $ \cos_theta ->
      bellows_xsection_area_mm_2 * l *
        (sin_theta + ((- l * sin_theta * cos_theta) / (sqrt (big_l_squared_minus_l_squared + (l * cos_theta) * (l * cos_theta)))))

  -- Change in theta w.r.t. time (seconds). That's exactly the velocity,
  -- but we must put it into radians per second because that's what we have
  -- computed the derivative of forward kinematics in.
  rad_theta_over_time :: Stream Double
  rad_theta_over_time = degrees_to_radians (unsafeCast v)

flow_f_observed :: Stream Word32 -> Stream Double -> Stream Double
flow_f_observed t_d_us vol = deltas

  where

  deltas :: Stream Double
  deltas = [0] ++ deltas_next

  deltas_next :: Stream Double
  deltas_next = if should_sample then vol - last_observed else deltas

  last_observed :: Stream Double
  last_observed = [0] ++ if should_sample then vol else last_observed

  should_sample :: Stream Bool
  should_sample = if elapsed >= 100000 then true else false

  elapsed :: Stream Word32
  elapsed = [0] ++ if elapsed >= 100000
                   then elapsed + t_d_us - 100000
                   else elapsed + t_d_us
