module Main where

import Prelude

gcd' :: Int -> Int -> Int
gcd' n 0 = n
gcd' 0 m = m
gcd' n m | n > m     = gcd' (n - m) m
         | otherwise = gcd' n (m - n)

fromString :: String -> Boolean
fromString "true" = true
fromString _      = false

toString :: Boolean -> String
toString true = "true"
toString false = "false"

factorial :: Int -> Int
factorial = factorial' 1
  where
    factorial' :: Int -> Int -> Int
    factorial' acc 0 = acc
    factorial' acc n = factorial' (acc * n) (n - 1)

binomial :: Int -> Int -> Int
binomial n 0 = 1
binomial n k | n < k     = 0
             | k < 0     = 0
             | otherwise = binomial (n - 1) (k - 1) + binomial (n - 1) k
