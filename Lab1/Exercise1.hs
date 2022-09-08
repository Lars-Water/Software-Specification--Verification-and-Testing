import Data.List
import Test.QuickCheck

{-
    Return the sum of squared natural numbers from 1 till the input
    number.

    n : Max value for sum of all squares from 1 till n
-}
sum_squares :: Int -> Int
sum_squares n = sum [m^2 | m <- [1..n]]

{-
    Return the value of an equation that is supposed to be the sum
    of all squares given.

    n : Max value for sum of all squares from 1 till n
-}
sum_squares' :: Int -> Int
sum_squares' n = n * (n+1) * (2*n+1) `div` 6

{-
    Test whether the sum of squares function equals the
    return value of the equation.

    n : Max value for sum of all squares from 1 till n
-}
test_sum_squares :: Int -> Property
test_sum_squares n = n > 0 ==> sum_squares' n == sum_squares n


{-
    Return the sum of cubed natural numbers from 1 till the input
    number.

    n : Max value for sum of all cubes from 1 till n
-}
sum_cubes :: Int -> Int
sum_cubes n = sum [m^3 | m <- [1..n]]

{-
    Return the value of an equation that is supposed to be the sum
    of all cubes given.

    n : Max value for sum of all cubes from 1 till n
-}
sum_cubes' :: Int -> Int
sum_cubes' n = (n * (n+1) `div` 2)^2

{-
    Test whether the sum of cubes function equals the
    return value of the equation.

    n : Max value for sum of all cubes from 1 till n
-}
test_sum_cubes :: Int -> Property
test_sum_cubes n = n > 0 ==> sum_cubes' n == sum_cubes n