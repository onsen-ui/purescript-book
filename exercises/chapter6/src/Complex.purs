module Complex where

import Prelude
import Data.Foldable (foldl)
import Data.Monoid (mempty)

data Complex = Complex
  { real      :: Number
  , imaginary :: Number
  }

instance showComplex :: Show Complex where
  show (Complex { real, imaginary }) = foldl (<>) mempty [show real, " + ", show imaginary, " i"]
instance eqComplex :: Eq Complex where
  eq (Complex { real: r1, imaginary: i1 }) (Complex { real: r2, imaginary: i2 }) = r1 == r2 && i1 == i2
