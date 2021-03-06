module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Control.MonadZero (guard)
import Data.Array (concat, filter, foldl, foldr, null, (..))
import Data.Array.Partial (tail, head)
import Partial.Unsafe (unsafePartial)

fact :: Int -> Int
fact 0 = 1
fact n = n * fact(n - 1)

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib(n - 1) + fib(n - 2)

length :: forall a. Array a -> Int
length arr =
  if null arr
    then 0
    else 1 + length(unsafePartial tail arr)

isEven :: Int -> Boolean
isEven 0 = true
isEven 1 = false
isEven n = isEven (n - 2)

countEven :: Array Int -> Int
countEven arr =
  if null arr
    then 0
    else if isEven(unsafePartial head arr)
      then 1 + countEven(unsafePartial tail arr)
      else countEven(unsafePartial tail arr)

square :: Array Number -> Array Number
square xs = (\n -> n * n) <$> xs

infix 5 filter as <$?>

removeNegatives :: Array Number -> Array Number
removeNegatives xs = (\n -> n >= 0.0) <$?> xs

factors :: Int -> Array (Array Int)
factors n = do
  i <- 1 .. n
  j <- i .. n
  guard $ i * j == n
  pure [i, j]

isPrime :: Int -> Boolean
isPrime 0 = false
isPrime 1 = false
isPrime n = (length <<< factors) n == 1

cartesianProduct :: forall a. Array a -> Array a -> Array (Array a)
cartesianProduct xs ys = do
  x <- xs
  y <- ys
  pure [x, y]

pythagoreanTriple :: Int -> Array (Array Int)
pythagoreanTriple n = do
  a <- 3 .. n
  b <- a .. n
  c <- b .. n
  guard $ a * a + b * b == c * c
  pure [a, b, c]

factorizations :: Int -> Array Int
factorizations 1 = []
factorizations n = concat [[p], factorizations $ n / p]
  where
    p :: Int
    p = firstFactor n

firstFactor :: Int -> Int
firstFactor = unsafePartial head <<< factors'
  where
    factors' :: Int -> Array Int
    factors' n = do
      m <- (1 .. n)
      guard $ isPrime m
      guard $ mod n m == 0
      pure m

reverse :: forall a. Array a -> Array a
reverse = foldr (\x xs -> xs <> [x]) []

all :: Array Boolean -> Boolean
all = foldl (&&) true

-- f [false] is true
f :: Array Boolean -> Boolean
f = foldl (==) false

reverse' :: forall a. Array a -> Array a
reverse' = foldl (\xs x -> [x] <> xs) []

count :: forall a. (a -> Boolean) -> Array a -> Int -> Int
count _ [] acc = acc
count p xs acc = if p (unsafePartial head xs)
                  then count p (unsafePartial tail xs) (acc + 1)
                  else count p (unsafePartial tail xs) acc

main :: Eff (console :: CONSOLE) Unit
main = logShow "Hello"
