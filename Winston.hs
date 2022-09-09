module Lib
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

isPrime :: Integer -> Bool -- helper to check wether given number is prime
isPrime k   | k > 1 = null [ x | x <- [2..k - 1], k `mod` x == 0]
            | otherwise = False

reversal :: Integer -> Integer -- helper to reverse a number
reversal = read . reverse . show

-- this helper function is used to check if the sum of a given list is prime
checkConsecutivePrimes :: [Integer] -> Integer
checkConsecutivePrimes (x:xs) | isPrime (sum (take 101 (x:xs))) = sum (take 101 (x:xs))
                              | otherwise = checkConsecutivePrimes xs

-- this helper function is used to get all the permutations of a given list (given in the question)
perms :: [a] ->[[a]]
perms [] = [[]]
perms (x:xs) = concat (map (insrt x) (perms xs))
    where   insrt x [] = [[x]]
            insrt x (y:ys) = (x:y:ys) : map (y:) (insrt x ys)

factorial :: Int -> Int -- helper to calculate the factorial of a given number
factorial n | n < 2 = 1
            | otherwise =  n * factorial (n-1)

exer2 :: Int -> Property
exer2 x =  x > 0 ==> sumSquared x == formula1 x

exer3 :: Int -> Property
exer3 x = x > 0 ==>  sumCubed x == formula2 x

exer4 :: Int -> Property
exer4 x = x >= 0 ==> length (subsequences [1..x]) == 2^(length [1..x])

exer5 :: Int -> Property
exer5 x = x > 1 ==> length (perms [1..x]) == factorial x

reversibleStream :: [Integer]
reversibleStream = filter (\x -> isPrime x && isPrime (reversal x)) [1..10000]

consecutive101Prime :: Integer
consecutive101Prime = checkConsecutivePrimes (filter (\x -> isPrime x) [1..10000])

primes = filter (\x -> isPrime x) [2..13] -- helper for counterexamples

counterexamples ::  [([Integer], Integer)] -- this one works given the primes example but not with quickcheck
counterexamples | isPrime ((product (primes)) + 1) = []
                | otherwise =  [(primes, ((product (primes)) + 1))]

luhn :: Integer -> Bool
luhn x = (sum (map (\(x,y) -> if y `mod` 2 == 0 then x else if x * 2 > 9 then x * 2 - 9 else x * 2) (zip (map (\x -> read [x] :: Integer) (reverse (show x))) [1..]))) `mod` 10 == 0

isAmericanExpress, isMaster, isVisa::  Integer -> Bool
isAmericanExpress x = (length (show x) == 15) && (take 2 (show x) `elem` ["37", "34"]) && (luhn x)
isMaster x = (length (show x) == 16) && (take 2 (show x) `elem` ["51","52","53","54","55"]) && (luhn x)
isVisa x = (length (show x) == 13 || length (show x) == 16) && (head(show x) == '4') && (luhn x)

data Boy = Matthew | Peter | Jack | Arnold | Carl
        deriving (Eq,Show)

boys = [Matthew, Peter, Jack, Arnold, Carl]

accuses ::  Boy -> Boy -> Bool
accuses boy1 boy2   | boy1 == Peter && (boy2 == Matthew || boy2 == Jack) = True
                    | otherwise = False

accusers ::  Boy -> [Boy]
accusers boy    | boy == Matthew || boy == Jack = [Peter]
                | otherwise = []

guilty, honest ::  [Boy]
guilty = [Jack]
honest = [Matthew, Peter, Arnold]

main :: IO ()
main = do
    quickCheck exer2
    quickCheck exer3
    quickCheck exer5
