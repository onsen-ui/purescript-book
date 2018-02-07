module Main where

import Prelude

gcd' :: Int -> Int -> Int
gcd' n 0 = n
gcd' 0 m = m
gcd' n m = if n > m
  then gcd' (n - m) m
  else gcd' n (m - n)

fromString :: String -> Boolean
fromString "true" = true
fromString _      = false

toString :: Boolean -> String
toString true = "true"
toString false = "false"
