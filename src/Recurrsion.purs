-- Recursion, Maps And Folds
-- Chapter 4

module Recurrsion where 
import Data.Foldable
import Prelude

import Control.MonadZero (guard)
import Data.Array (null, filter, concat, (..), snoc, (:), concatMap)
import Data.Array.Partial (tail, head)
import Data.Function (on)
-- import Data.List.Lazy.NonEmpty (concatMap)
import Data.Traversable (traverseDefault)
import Partial.Unsafe (unsafeCrashWith, unsafePartial)

-- Introduction : Recurssion means happening again and again and Recurssion in purescript will be a function calling itself repeatadle with divided input.
-- Recurssion is used reduce the mutable state in our program.

-- factorial function using recurrsion.
factorial1 :: Int -> Int 
factorial1 0 = 1
factorial1 n = n * factorial1 (n-1)

--function for Fibonnacci value of a number
fibonnacci :: Int -> Int 
fibonnacci 0 = 1
fibonnacci 1 = 1 
fibonnacci n = fibonnacci (n-1) + fibonnacci (n-2)

-- 4) Recursion on Arrays
-- 4.1) Calculate the length of the array using recurrsion
length1 :: forall a . Array a -> Int
length1 arr =  if (null arr) then 0 else 1 + length1 (unsafePartial tail arr) -- head is considered so directly adding the length1 of tail with 1. 

-- Exercises
-- 1. (Easy) Write a recursive function which returns true if and only if its input is an even integer.
isEven ::  Int -> Boolean 
isEven n = if (n < 0) then false else if (n == 0) then true else isEven (n-2)

--2. (Medium) Write a recursive function which counts the number of even integers in an array. Hint: the function unsafePartial head (where head is also imported from Data.Array.Partial) can be used to find the first element in a non-empty array.
isEvenArray :: Array Int -> Int
isEvenArray arr =  if (null arr) 
                     then 0 
                     else if (isEven (unsafePartial head arr)) 
                             then 1 + isEvenArray (unsafePartial tail arr) 
                             else 0 + isEvenArray (unsafePartial tail arr)

-- 4.5 Maps (<$>)
-- The map function is an example of a recursive function on arrays. It is used to transform the elements of an array by applying a function to each element in turn
-- map :: forall a b f. Functor f=> (a -> b) -> f a -> f b
-- ( .. ) is present in Data.Array module that desfines the range of  the function. 
-- :t ( .. ) :: nt -> Int -> Array Int
-- 4.7 Filtering Arrays ()

-- Exercises
-- 1. (Easy) Use the map or <$> function to write a function which calculates the squares of an array of numbers.
squareArray :: Array Int -> Array Int
squareArray arr = map (\a -> a * a) arr
-- 2. (Easy) Use the filter function to write a function which removes the negative numbers from an array of number
noNegative :: Array Int -> Array Int
noNegative arr = filter (\a -> a > 0) arr

-- 4.8 Flattening Arrays
-- Another standard function on arrays is the concat function, defined in Data.Array. concat flattens an array of arrays into a single array:
-- :type concat :: forall a. Array (Array a) -> Array a
-- Another standard functio is concatMap that performs 
-- concatMap take elemnts from array perform some transformation on that element and stores it as array of element. At the time of returning evaluated output it performs concatination on each element forming a single array.
-- :t concatMap :: forall a b. (a -> Array b) -> Array a -> Array b

-- 4.9 Array Comprehensions
factorOf1 n = concatMap (\i -> i .. n ) (1 .. n)
factorOf2 n = concatMap (\i -> map (\j -> [i,j]) (1 .. n)) (1 .. n) -- all possible pairs b/n 1 to n
factorOf3 n = concatMap (\i -> map (\j -> [i,j]) (i .. n)) (1 .. n) -- all possible pairs b/n 1 to n with no repeats
factorOf4 n = filter (\pair -> product pair == n) (factorOf3 n)

-- 4.10 Do Notation
-- Just like map and concatMap allowed us to write array comprehensions, the more general operators map and bind allow us to write so-called monad comprehensions. We’ll see plenty more examples of monads later in the book, but in this chapter, we will only consider arrays.
-- We can rewrite our factors function using do notation as follows:

factorOf5 n = filter (\x -> product x == n) $ do
  i <- 1 .. n
  j <- i .. n
  pure [i, j]

-- 4.11 Guards
factorOf6 :: Int -> Array (Array Int)
factorOf6 n = do
  i <- 1 .. n
  j <- i .. n
  guard $ i * j == n 
  pure [i, j]


-- 4.12 Folds
-- foldl (\acc n -> acc <> show n) "" [1,2,3,4,5]
-- ((((("" <> show 1) <> show 2) <> show 3) <> show 4) <> show 5)

-- foldr (\n acc -> acc <> show n) "" [1,2,3,4,5]
-- ((((("" <> show 5) <> show 4) <> show 3) <> show 2) <> show 1)
