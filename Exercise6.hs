-- Time spent: 23 minutes

module Exercise6
    (
    ) where
import Test.QuickCheck
import Data.List

prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
    where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..] 

counterexamples ::  [([Integer], Integer)]
counterexamples = generateCounterPrimess [] 1

generateCounterPrimess xs n
    | not $ prime $ product (take n primes) + 1 = xs ++ [(take n primes, product (take n primes) + 1)] ++ generateCounterPrimess xs (n+1)
    | otherwise = xs ++ generateCounterPrimess xs (n+1)