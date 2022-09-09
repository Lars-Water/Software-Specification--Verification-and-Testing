-- Time spent: 45 mintues
module Exercise7
    (
    ) where
import Test.QuickCheck
import Data.List


luhn :: Integer -> Bool
luhn x =  (sum (map (\(x,y) -> if y `mod` 2 == 0 then x else if x * 2 > 9 then x * 2 - 9 else x * 2) (zip (map (\x -> read [x] :: Integer) (reverse (show x))) [0..]))) `mod` 10 == 0

isAmericanExpress, isMaster, isVisa::  Integer -> Bool
isAmericanExpress x = (length (show x) == 15) && (take 2 (show x) `elem` ["37", "34"]) && (luhn x)
isMaster x = (length (show x) == 16) && (take 2 (show x) `elem` ["51","52","53","54","55"]) && (luhn x)
isVisa x = (length (show x) == 13 || length (show x) == 16) && (head(show x) == '4') && (luhn x)

-- exercise7 :: IO()
-- exercise7 = do
