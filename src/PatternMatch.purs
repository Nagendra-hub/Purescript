-- Chapter 5 Pattern Matching

module PatternMatch where

import Prelude
import Data.Foldable (foldl)
import Global as Global -- bower install purescript-globals@3.0.0
import Math -- bower install purescript-math@2.0.0
import Data.Array.Partial (tail) -- bower install purescript-arrays@4.0.1
import Partial.Unsafe (unsafePartial) -- bower install purescript-partial@1.2.0



-- 5.3 Simple Pattern Matching
-- Euclidean Algorithm 
gcd :: Int -> Int -> Int
gcd n 0 = n
gcd 0 m = m
gcd n m = if n > m
            then gcd (n - m) m
            else gcd n (m - n)

--5.4 Simple Patterns
-- two types of patterns:
-- Integer literals patterns, which match something of type Int, only if the value matches exactly.
-- Variable patterns, which bind their argument to a name

fromString :: String -> Boolean
fromString "true" = true
fromString _      = false

toString :: Boolean -> String
toString true  = "true"
toString false = "false"

-- 5.5 Guards
-- A guard is a boolean-valued expression which must be satisfied in addition to the constraints imposed by the patterns. Here is the Euclidean algorithm rewritten to use a guard:

gcd2 :: Int -> Int -> Int
gcd2 n 0 = n
gcd2 0 n = n
gcd2 n m | n > m     = gcd2 (n - m) m
         | otherwise = gcd2 n (m - n)

-- Exercises
-- (Easy) Write the factorial function using pattern matching. Hint. Consider the two cases zero and non-zero inputs.

factorial 0 = 1 
factorial 1 = 1
factorial n = factorial ( n -1) * factorial (n -2)

-- 5.6 Array Patterns
isEmpty :: forall a. Array a -> Boolean
isEmpty [] = true
isEmpty _ = false

takeFive :: Array Int -> Int
takeFive [0, 1, a, b, _] = a * b
takeFive _ = 0

-- 5.7 Record Patterns and Row Polymorphism

-- showPerson :: { first :: String, last :: String } -> String
showPerson1 { first: x, last: y } = y <> ", " <> x
showPerson2 p =   p.first <> p.last

-- 5.8 Nested Patterns

type Address = { street :: String, city :: String }

type Person = { name :: String, address :: Address }

livesInLA :: Person -> Boolean
livesInLA { address: { city: "Los Angeles" } } = true
livesInLA _ = false

-- 5.9 Named Patterns
-- Patterns can be named to bring additional names into scope when using nested patterns. Any pattern can be named by using the @ symbol.
sortPair :: Array Int -> Array Int
sortPair arr@[x,y] 
   | x <= y = arr
   | otherwise = [y,x]
sortPair arr = arr

-- Exercises
-- (Easy) Write a function sameCity which uses record patterns to test whether two Person records belong to the same city.
-- (Medium) What is the most general type of the sameCity function, taking into account row polymorphism? What about the livesInLA function defined above?
-- (Medium) Write a function fromSingleton which uses an array literal pattern to extract the sole member of a singleton array. If the array is not a singleton, your function should return a provided default value. Your function should have type forall a. a -> Array a -> a

-- 5.10 Case Expressions
lzs xs = case xs of
           0 -> true
           _ -> false

-- 5.11 Pattern Match Failures and Partial Functions

partialFunction :: Boolean -> Boolean
partialFunction = unsafePartial \true -> true

-- 5.12 Algebraic Data Types
data Shape
  = Circle Point Number
  | Rectangle Point Number Number
  | Line Point Point
  | Text Point String

data Point = Point
  { x :: Number
  , y :: Number
  }

data Maybe' a = Nothing | Just a
-- data List a = Nil | Cons a (List a)
exampleLine :: Shape
exampleLine = Line p1 p2 
  where 
    p1 = Point {x:1.0,y:1.0}
    p2 = Point {x:1.0,y:1.0}

showPoint :: Point -> String
showPoint (Point { x: x, y: y }) =
   "{ x: " <> show x<> ", y: " <> show y<> "}"

showPoint' :: Point -> String
showPoint' (Point { x, y }) = 
   "{ x: " <> show x<> ", y: " <> show y<> "}"

--Exercises
-- (Easy) Construct a value of type Shape which represents a circle centered at the origin with radius 10.0.
-- (Medium) Write a function from Shapes to Shapes, which scales its argument by a factor of 2.0, center the origin.
-- (Medium) Write a function which extracts the text from a Shape. It should return Maybe String, and use the Nothing constructor if the input is not constructed using Text.

-- 5.15 Newtypes
-- Newtypes are introduced using the newtype keyword instead of the data keyword.
-- Newtypes must define exactly one constructor, and that constructor must take exactly one argument. That is, a newtype gives a new name to an existing type.

newtype Pixels = Pixels Number
newtype Inches = Inches Number

-- 5.16 A Library for Vector Graphics
-- type provides sunonum for the existing type. We can call name as String
type Name = String
type Picture = Array Shape
