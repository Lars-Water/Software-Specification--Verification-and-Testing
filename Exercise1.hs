-- Redo exercises 2 and 3 of Workshop 1 by writing QuickCheck tests for these statements.
-- Deliverables: Haskell program, indication of time spent.
-- Time spent on this exercise was: 30 minutes (due to the fact we had to figure out how quickcheck works)

module Exercise1
    (
    ) where
import Test.QuickCheck
import Data.List

sumSquared :: Int -> Int --helper for exer2 to get the sum of the digits squared
sumSquared n = sum [x * x | x <- [1..n]]

sumCubed :: Int -> Int --helper for exer3 to get the sum of the digits to the power of 3
sumCubed n = sum [x ^ 3 | x <- [1..n]]

formula1 :: Int -> Int --helper for exer2
formula1 x = (x * (x + 1)*(2*x + 1)) `div` 6

formula2 :: Int -> Int-- helper for exer3
formula2 x = ((x * (x + 1)) `div` 2)^2

-- Exercise 2

exer2 :: Int -> Property
exer2 x =  x > 0 ==> sumSquared x == formula1 x

-- Exercise 3

exer3 :: Int -> Property
exer3 x = x > 0 ==>  sumCubed x == formula2 x