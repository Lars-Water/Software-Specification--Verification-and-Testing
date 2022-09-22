import Data.List
import System.Random
import Test.QuickCheck
import Lecture3

main :: IO()
main = do

    -- Incorrect string input.
    print $ parse ""
    print $ parse "*"
    print $ parse "2==>2"

    -- Correct string input.
    print $ parse "((1==>2)<=>(-2==>-1))"
    print $ parse "((1==>2)<=>(-1==>-2))"
    print $ parse "(*((1==>2) (2==>3))==>(1==>3))"