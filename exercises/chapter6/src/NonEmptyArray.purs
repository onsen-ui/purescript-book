module NonEmptyArray where

import Prelude

import Data.Array (foldMap, foldl, foldr, (:))
import Data.Foldable (class Foldable)

data NonEmpty a = NonEmpty a (Array a)

instance showNonEmpty :: Show a => Show (NonEmpty a) where
  show (NonEmpty x xs) = "NonEmpty " <> show (x:xs)

instance eqNonEmpty :: Eq a => Eq (NonEmpty a) where
  eq (NonEmpty x xs) (NonEmpty y ys) = x == y && xs == ys

instance semigroupNonEmpty :: Semigroup (NonEmpty a) where
  append (NonEmpty x xs) (NonEmpty y ys) = NonEmpty x (xs <> [y] <> ys)

instance functorNonEmpty :: Functor NonEmpty where
  map f (NonEmpty x xs) = NonEmpty (f x) (f <$> xs)

instance foldableNonEmpty :: Foldable NonEmpty where
  foldl f acc (NonEmpty x xs) = foldl f acc ([x] <> xs)
  foldr f acc (NonEmpty x xs) = foldr f acc ([x] <> xs)
  foldMap f (NonEmpty x xs) = foldMap f ([x] <> xs)
