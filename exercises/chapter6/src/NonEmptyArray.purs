module NonEmptyArray where

import Prelude

import Data.Array ((:))
import Data.Maybe (Maybe(..))

data NonEmpty a = NonEmpty a (Array a)
instance showNonEmpty :: Show a => Show (NonEmpty a) where
  show (NonEmpty x xs) = "NonEmpty " <> show (x:xs)
instance eqNonEmpty :: Eq a => Eq (NonEmpty a) where
  eq (NonEmpty x xs) (NonEmpty y ys) = x == y && xs == ys
instance semigroupNonEmpty :: Semigroup (NonEmpty a) where
  append (NonEmpty x xs) (NonEmpty y ys) = NonEmpty x (xs <> [y] <> ys)
instance functorNonEmpty :: Functor NonEmpty where
  map f (NonEmpty x xs) = NonEmpty (f x) (f <$> xs)
