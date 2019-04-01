module Main where

import Effect.Console
import Prelude

import Math (pi, sqrt)

diagonal w h = sqrt (w*w + h*h)
circleArea r = pi * r * r

main = logShow (circleArea 3.0)

