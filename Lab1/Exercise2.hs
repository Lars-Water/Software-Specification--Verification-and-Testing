import Data.List
import Test.QuickCheck

cardinality_power :: Int -> Int
cardinality_power n = length (subsequences [1..n])

cardinality_power' :: Int -> Int
cardinality_power' n = 2^n

test_cardinality :: Int -> Property
test_cardinality n = n >= 0 ==> cardinality_power n == cardinality_power' n