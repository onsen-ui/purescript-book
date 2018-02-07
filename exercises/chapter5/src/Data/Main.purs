module Main where

import Prelude

gcd' :: Int -> Int -> Int
gcd' n 0 = n
gcd' 0 m = m
gcd' n m = if n > m
  then gcd' (n - m) m
  else gcd' n (m - n)
