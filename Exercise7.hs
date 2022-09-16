-- This file is property of the group Notorious Fortunate Panda © 2022
-- Time spent: 30 mintues

{-
    In the main function we have given a list of working iban numbers and non valid iban numbers.
    source: https://www.iban.com/structure
    on which we have all tested the iban function (which could be transformed to a property).
    The results show that all the iban numbers are valid and all the non valid iban numbers are invalid.

    We could automate this process by transforming the iban function into a property and then use quickCheck to test it.
    Howwever since the iban constraint on the string is rather specific we feel like quickCheck would not be able to generate
    enough random strings to test the property before giving up.
-}

module Exercise7
    (
    ) where
import Test.QuickCheck
import Data.List
import Data.Char


iban :: String -> Bool
iban string | (read (concat $ map (char_to_string) $ to_back $ filter (/=' ') string) :: Integer) `mod` 97 == 1 = True
            | otherwise = False

to_back :: String -> String
to_back string = remainder ++ init
    where remainder = drop 4 string
          init      = take 4 string

char_to_string :: Char -> [Char]
char_to_string x | x `elem` ['0'..'9'] = [x]
                 | otherwise           = show $ (fromEnum $ toUpper x) - 55

working_iban :: [String]
working_iban = ["HR1723600001101234565", "LU120010001234567891","LT601010012345678901", "DE89370400440532013000", "CH9300762011623852957"]

non_working_iban :: [String]
non_working_iban = ["HR1723600001101234563", "LU120010001234567893","LT601010012345678903", "DE89370400440532013003", "CH9300762011623852959"]

main :: IO ()
main = do
    putStrLn "Working IBANs:"
    mapM_ print working_iban
    putStrLn "Does our function validate them?"
    print $ all iban working_iban
    putStrLn "Non-working IBANs:"
    mapM_ print non_working_iban
    putStrLn "Does our function see them as invalid?"
    print $ all (not . iban) non_working_iban