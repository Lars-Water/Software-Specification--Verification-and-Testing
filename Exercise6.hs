-- This file is property of the group Notorious Fortunate Panda © 2022
-- Time spent: 30 mintues

{-
    ROT13 is a simple algorithm that changes every character in a string to the character 13 places further from it
    in the alphabet. If the 13 range jump is further than the character z, the algorithm continues at the start of
    the alphabet again. This is for both capital and non-capital alphabet characters.

    For example, the string "Hello World" would be encoded as "Uryyb Jbeyq" using ROT13.
    In our our implementation we check for both upper and lower case letters and only encode letters.
    All other characters are left as they are. For example, the string "Hello World!" would be encoded as "Uryyb Jbeyq!".
    If a letter goes beyond the end of the alphabet, it wraps around to the start of the alphabet.

    We check our function with two properties:
    1. We make sure that the length of the encoded string is the same as the length of the original string.
    2. We make sure that if the original string is a non-alphabet character string, the encoded string is the same as the original string.

        * Our second property is quite strong since we are very specific we only allow non-alphabet characters string, this
        results in QuickCheck not being able to find enough counter examples before giving up.
-}


module Exercise6
    (
    ) where

import Test.QuickCheck
import Data.List
import Data.Char

rot13 :: [Char] -> [Char]
rot13 [] = []
rot13 xs = map (\x -> if isAlpha x then rot13Single x else x) xs

-- Rotates a single character 13 places in the alphabet, if it goes beyond the alphabet, it starts again at the beginning
rot13Single :: Char -> Char
rot13Single x   | isLower x = if chr(ord x + 13) > 'z' then chr(ord x - 13 )  else chr(ord x + 13)
                | isUpper x = if chr(ord x + 13) > 'Z' then chr(ord x - 13)  else chr(ord x + 13)
                | otherwise = x

-- This property checks if the the rot13 function returns the same length as the input
sameLength :: [Char] -> Property
sameLength x = length x > 0 ==>  length x == length (rot13 x)

-- helper function to check the string with non-alphabet characters are not changed
nonAlpha :: [Char] -> Property
nonAlpha x  = length x > 0 && (all (== True) (map (\y -> not (isAlpha y)) x)) ==> x == rot13 x

main :: IO ()
main = do
    putStrLn "Testing if the length of the rot13 function works"
    putStrLn "Original string: Hello World"
    putStrLn "Rot13 string: "
    print (rot13 "Hello World")
    putStrLn "Now we check if the length of the original string is the same as the length of the rot13 string: "
    quickCheck sameLength
    putStrLn "Now we check if the rot13 function stays the same given a string of non-alphabet characters: "
    quickCheck nonAlpha
