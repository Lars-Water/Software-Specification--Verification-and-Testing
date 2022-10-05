import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

iban :: String -> Bool
iban string | (read (concat $ map char_to_string $ to_back $ filter (/=' ') string) :: Integer) `mod` 97 == 1 = True
            | otherwise = False

to_back :: String -> String
to_back string = remainder ++ init
    where remainder = drop 4 string
          init      = take 4 string

char_to_string :: Char -> [Char]
char_to_string x | x `elem` ['0'..'9'] = [x]
                 | otherwise           = show $ (fromEnum $ toUpper x) - 55