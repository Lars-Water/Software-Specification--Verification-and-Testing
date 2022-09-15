import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement [] [] = True
isDerangement _  [] = False
isDerangement [] _  = False
isDerangement (x:xs) (y:ys) = x /= y && x `elem` (y:ys)