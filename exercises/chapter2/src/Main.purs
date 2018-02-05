module Main where

import Prelude (Unit, (+), (*))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Math (sqrt, pi)

diagonal :: Number -> Number -> Number
diagonal w h = sqrt (w * w + h * h)

circleArea :: Number -> Number
circleArea r = pi * r * r

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = logShow (circleArea (diagonal 3.0 4.0))
