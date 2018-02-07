module Extended where

import Prelude

data Extended a = Finite a | Infinite

instance showExtended :: Show a => Show (Extended a) where
  show (Finite a) = "Finite " <> show a
  show Infinite = "Infinite"

instance eqExtended :: Eq a => Eq (Extended a) where
  eq (Finite a) (Finite b) = eq a b
  eq Infinite Infinite = true
  eq _ _ = false

instance ordExtended :: Ord a => Ord (Extended a) where
  compare (Finite a) (Finite b) = compare a b
  compare (Finite _) Infinite = LT
  compare Infinite (Finite _) = GT
  compare Infinite Infinite = EQ
