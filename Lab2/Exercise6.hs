import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

-- ROT13 is a simple algorithm that changes every character in a string to the character 13 places further from it
-- in the alphabet. If the 13 range jump is further than the character z, the algorithm continues at the start of
-- the alphabet again. This is for both capital and non-capital alphabet characters.

rot13 :: [Char] -> [Char]
rot13 string = map (rot13') string

rot13' :: Char -> Char
rot13' x | (fromEnum x >= 65 && fromEnum x < 78) ||
           (fromEnum x >= 97 && fromEnum x < 110)   = toEnum $ fromEnum x + 13
         | fromEnum x >= 78 && fromEnum x < 91      = toEnum (65 + 13 - (91 - fromEnum x))
         | fromEnum x >= 110 && fromEnum x < 123    = toEnum (97 + 13 - (123 - fromEnum x))
         | otherwise                                = x