-- Do you have to test that your answer is correct? How could this be checked?
-- Part of this could be tested by checking if it is a prime number.
-- And the another way would be to create a list of consecutive primes
-- and then check if there are 101 prime numbers smaller than the answer of consecutive101Prime
-- and if they are consecutive

-- Time spent: 20 minutes

module Exercise5
    (
    ) where
import Test.QuickCheck
import Data.List

isPrime :: Integer -> Bool -- helper to check wether given number is prime
isPrime k   | k > 1 = null [ x | x <- [2..k - 1], k `mod` x == 0]
            | otherwise = False

-- this helper function is used to check if the sum of a given list is prime
checkConsecutivePrimes :: [Integer] -> Integer
checkConsecutivePrimes (x:xs) | isPrime (sum (take 101 (x:xs))) = sum (take 101 (x:xs))
                              | otherwise = checkConsecutivePrimes xs
consecutive101Prime :: Integer
consecutive101Prime = checkConsecutivePrimes (filter (\x -> isPrime x) [1..10000])