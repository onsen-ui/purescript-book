module OneMore where

import Prelude

import Data.Foldable (class Foldable, foldMap, foldl, foldr)

data OneMore f a = OneMore a (f a)

instance showOneMore :: (Show a, Show (f a)) => Show (OneMore f a) where
  show (OneMore x ys) = "OneMore " <> show x <> " " <> show ys

instance foldableOneMore :: Foldable f => Foldable (OneMore f) where
  foldr f z (OneMore x ys) = f x (foldr f z ys)
  foldl f z (OneMore x ys) = foldl f (f z x) ys
  foldMap f (OneMore x ys) = (f x) <> (foldMap f ys)
