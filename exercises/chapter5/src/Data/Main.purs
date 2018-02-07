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

isEmpty :: forall a. Array a -> Boolean
isEmpty [] = true
isEmpty _  = false

takeFive :: Array Int -> Int
takeFive [0, 1, a, b, _] = a * b
takeFive _ = 0

showPerson :: { first :: String, last :: String } -> String
showPerson { first: x, last: y } = y <> ", " <> x

type Address = { street :: String, city :: String }

type Person = { name :: String, address :: Address }

livesInLA :: Person -> Boolean
livesInLA { address: { city: "Los Angeles" } } = true
livesInLA _ = false

sortPair :: Array Int -> Array Int
sortPair arr@[x, y]
  | x <= y = arr
  | otherwise = [y, x]
sortPair arr = arr
