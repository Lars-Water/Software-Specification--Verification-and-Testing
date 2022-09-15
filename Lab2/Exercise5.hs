import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] []         = True
isPermutation [] _          = False
isPermutation _ []          = False
isPermutation (x:xs) ys     = x `elem` ys && isPermutation xs (x `delete` ys)

isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement xs ys = isPermutation xs ys && all (==True) (zipWith (/=) xs ys)

deran :: Int -> [[Int]]
deran n = filter (\x -> isDerangement x [1..n]) (permutations [1..n])