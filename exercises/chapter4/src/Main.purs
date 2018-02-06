module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Control.MonadZero (guard)
import Data.Array (null, filter, (..), range)
import Data.Array.Partial (tail, head)
import Data.Foldable (product)
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

main :: Eff (console :: CONSOLE) Unit
main = logShow "Hello"
