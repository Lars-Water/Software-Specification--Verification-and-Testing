-- Time spent: 45 mintues
module Exercise7
    (
    ) where
import Test.QuickCheck
import Data.List


luhnMult :: (Integer, Integer) -> Integer
luhnMult (x,y) | y `mod` 2 == 0 = x
               | x * 2 > 9 = x * 2 - 9
               | otherwise = x * 2

reverseToList :: Integer -> [Integer]
reverseToList n = map (\x -> read [x] :: Integer) (reverse (show n))

luhn :: Integer -> Bool
luhn x =  sum (map luhnMult (zip (reverseToList x) [0..])) `mod` 10 == 0

isAmericanExpress, isMaster, isVisa::  Integer -> Bool
isAmericanExpress x = (length (show x) == 15) && (take 2 (show x) `elem` ["37", "34"]) && (luhn x)
isMaster x = (length (show x) == 16) && (take 2 (show x) `elem` ["51","52","53","54","55"]) && (luhn x)
isVisa x = (length (show x) == 13 || length (show x) == 16) && (head(show x) == '4') && (luhn x)

exercise7 :: IO()
exercise7 = do
putStrLn  "This function tests if the mastercard function returns the correct value for a valid mastercard."
putStrLn  "To test a valid credit card number is given where a True return value is expected."
putStrLn  "5557698037687941 is valid: "
print (isMaster 5557698037687941)
putStrLn  "This function tests if the mastercard function returns the correct value for an invalid mastercard."
putStrLn  "To test a valid credit card number is given where a True return value is expected."
putStrLn  "5557698037687942 is valid: "
print (isMaster 5557698037687942)
putStrLn  "This function tests if the VISA card function returns the correct value for a valid VISA card."
putStrLn  "To test a valid credit card number is given where a True return value is expected."
putStrLn  "4031263680314639 is valid: "
print (isVisa 4031263680314639)
putStrLn  "This function tests if the VISA card function returns the correct value for an invalid VISA card."
putStrLn  "To test a valid credit card number is given where a True return value is expected."
putStrLn  "4031263680314640 is valid: "
print (isVisa 4031263680314640)
putStrLn  "This function tests if the A.Express card function returns the correct value for a valid A.Express card."
putStrLn  "To test a valid credit card number is given where a True return value is expected."
putStrLn  "344240041968782 is valid: "
print (isAmericanExpress 344240041968782)
putStrLn  "This function tests if the A.Express card function returns the correct value for an invalid A.Express card."
putStrLn  "To test a valid credit card number is given where a True return value is expected."
putStrLn  "344240041968783 is valid: "
print (isAmericanExpress 344240041968783)
