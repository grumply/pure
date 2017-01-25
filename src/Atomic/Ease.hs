module Atomic.Ease where

-- Easings take a total duration (partial application can simplify use)
-- and an elapsed duration and return a scalar for muliplying against the desired
-- total change. For bounded easings, this is a value between 0 and 1. For some
-- easings, this value may pass below 0 or above 1 before eventually settling
-- at 1.

-- As an example, if we wanted to scroll from a current position p to a new
-- position p' over 600 milliseconds starting at time t, we would create a loop
-- that runs, likely in an animation frame, and calculate the current value of
-- the easing function as c = 'e 600 (currentTime - t)' and then get the change
-- delta as d = c * (p' - p). The delta can be applied to the original p to get
-- our new offset n = p + d that we can use to set our scroll position as
-- 'setScrollY n'.

import Debug.Trace

type TotalDuration = Double
type Elapsed = Double
type Scalar = Double

type Ease = TotalDuration -> Elapsed -> Scalar

easeInQuad :: Ease
easeInQuad d t =
  let t' = t / d
  in
    t' * t'

easeOutQuad :: Ease
easeOutQuad d t =
  let t' = t / d
  in
    (-1) * (t' * (t' - 2))

easeInOutQuad :: Ease
easeInOutQuad d t =
  let t' = t / (d / 2)
  in
    if t' < 1 then
      0.5 * (t' * t')
    else
      (-0.5) * ((t' - 1) * (t' - 3) - 1)

easeInCubic :: Ease
easeInCubic d t =
  let t' = t / d
  in
    t' * t' * t'

easeOutCubic :: Ease
easeOutCubic d t =
  let t' = t / (d - 1)
  in
    t' * t' * t' + 1

easeInOutCubic :: Ease
easeInOutCubic d t =
  let t' = t / (d / 2)
      halve = (0.5 *)
  in
    halve $
      if t' < 1 then
        t' * t' * t'
      else
        let t'' = t' - 2
        in
          t'' * t'' * t'' + 2

easeInQuart :: Ease
easeInQuart d t =
  let t' = t / d
  in
    t' * t' * t' * t'

easeOutQuart :: Ease
easeOutQuart d t =
  let t' = t / (d  - 1)
  in
    (-1) * (t' * t' * t' * t' - 1)

easeInOutQuart :: Ease
easeInOutQuart d t =
  let t' = t / (d / 2)
  in
    if t' < 1 then
      0.5 * t' * t' * t' * t'
    else
      let t'' = t' - 2
      in
        (-0.5) * (t'' * t'' * t'' * t'' - 2)

easeInQuint :: Ease
easeInQuint d t =
  let t' = t / d
  in
    t' * t' * t' * t' * t'

easeOutQuint :: Ease
easeOutQuint d t =
  let t' = t / (d - 1)
  in
    t' * t' * t' * t' * t' + 1

easeInOutQuint :: Ease
easeInOutQuint d t =
  let t' = t / (d / 2)
      halve = (0.5 *)
  in
    halve $
      if t' < 1 then
        t' * t' * t' * t' * t'
      else
        let t'' = t' - 2
        in
          t'' * t'' * t'' * t'' * t'' + 2

easeInSine :: Ease
easeInSine d t =
  (-1) * cos(t / d * (pi / 2)) + 1

easeOutSine :: Ease
easeOutSine d t =
  sin(t / d * (pi / 2))

easeInOutSine :: Ease
easeInOutSine d t =
  (-0.5) * (cos (pi * t / d) - 1)

easeInExpo :: Ease
easeInExpo d t =
  if t == 0 then
    0
  else
    2 ** (10 * (t / d - 1))

easeOutExpo :: Ease
easeOutExpo d t =
  if t >= d then
    1
  else
    negate ((2 ** ((-10) * t / d)) + 1)

easeInOutExpo :: Ease
easeInOutExpo d t
  | t == 0 = 0
  | t >= d = 1
  | otherwise =
      let t' = t / (d / 2)
      in
        (0.5 *) $
          if t' < 1 then
            2 ** (10 * (t' - 1))
          else
            negate (2 ** ((-10) * (t' - 1))) + 2

easeInCirc :: Ease
easeInCirc d t =
  let t' = t / d
  in
    (-1) * (sqrt(1 - t' * t') - 1)

easeOutCirc :: Ease
easeOutCirc d t =
  let t' = t / d - 1
  in
    sqrt (1 - t' * t')

easeInOutCirc :: Ease
easeInOutCirc d t =
  let t' = t / (d / 2)
  in
    if t' < 1 then
      (-0.5) * (sqrt (1 - t' * t') - 1)
    else
      let t'' = t' - 2
      in
        0.5 * (sqrt (1 - t'' * t'') + 1)

easeInElastic :: Ease
easeInElastic _ 0 = 0
easeInElastic d t =
  let t' = t / d
  in
    if t' >= 1 then
      1
    else
      let p = d * 0.3
          s = p / (2 * pi) * asin 1
          t'' = t' - 1
      in
        negate (2 ** (10 * t'') * sin ((t'' * d - s) * (2 * pi) / p))

easeOutElastic :: Ease
easeOutElastic _ 0 = 0
easeOutElastic d t =
  let t' = t / d
  in
    if t' >= 1 then
      1
    else
      let p = d * 0.3
          s = p / (2 * pi) * asin 1
      in
        2 ** ((-10) * t') * sin ((t' * d - s) * (2 * pi) / p) + 1

easeInOutElastic :: Ease
easeInOutElastic _ 0 = 0
easeInOutElastic d t =
  let t' = t / (d / 2)
  in
    if t' >= 2 then
      1
    else
      let p = d * 0.3 * 1.5
          s = p / (2 * pi) * asin 1
          t'' = t' - 1
          halve = (0.5 *)
          delta b = 2 ** ((b * 10) * t'') * sin ((t'' * d - s) * (2 * pi) / p)
      in
        if t'' < 0 then
          halve $ negate $ delta 1
        else
          delta (-1) + 1

easeInBack :: Ease
easeInBack d t =
  let s = 1.70158
      t' = t / d
  in t' * t' * ((s + 1) * t' - s)

easeOutBack :: Ease
easeOutBack d t =
  let s = 1.70158
      t' = t / (d - 1)
  in
    t' * t' * ((s + 1) * t' + s) + 1

easeInOutBack :: Ease
easeInOutBack d t =
  let s = 1.70158 * 1.525
      t' = t / (d / 2)
      halve = (0.5 *)
  in
    halve $
      if t' < 1 then
        t' * t' * ((s + 1) * t' - s)
      else
        let t'' = t' - 2
        in
          t'' * t'' * ((s + 1) * t'' + s) + 2

easeInBounce :: Ease
easeInBounce d t =
  1 - easeOutBounce d (d - t)

easeOutBounce :: Ease
easeOutBounce d t =
  let t' = t / d
  in
    if t' < (1/2.75) then
      7.6725 * t' * t'
    else
      if t' < (2/2.75) then
        let t'' = t' - (1.5/2.75)
        in
          7.5625 * t'' * t'' + 0.75
      else
        if t' < (2.25/2.75) then
          let t'' = t' - (2.25/2.75)
          in
            7.5625 * t'' * t'' + 0.9375
        else
          let t'' = t' - (2.625/2.75)
          in
            7.5625 * t'' * t'' + 0.984375

easeInOutBounce :: Ease
easeInOutBounce d t =
  if t < d / 2 then
    easeInBounce d (t * 2) * 0.5
  else
    easeOutBounce d (t * 2 - d) * 0.5 + 0.5
