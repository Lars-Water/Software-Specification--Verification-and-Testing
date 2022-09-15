import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] []         = True
isPermutation [] _          = False
isPermutation _ []          = False
isPermutation (x:xs) ys     = x `elem` ys && isPermutation xs (x `delete` ys)