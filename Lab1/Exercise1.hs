import Data.List
import Test.QuickCheck

prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
    where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..]

infix 1 -->

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all


-- Return the sum of squared natural numbers from 1 till the input number.
sum_squares :: Int -> Int
sum_squares n = sum [m^2 | m <- [1..n]]

check_sum_squares :: (Int -> Int) -> Int -> Bool
check_sum_squares = 
