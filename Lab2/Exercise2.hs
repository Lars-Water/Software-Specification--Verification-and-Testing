import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

data Shape = NoTriangle | Equilateral | Isosceles | Rectangular | Other deriving (Eq,Show)

triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c = triangle' sides
    where sides = sort [a,b,c]

triangle' :: [Integer] -> Shape
triangle' [a,b,c] | a + b <= c                  = NoTriangle
                  | a == b && b == c            = Equilateral
                  | a == b || b == c || a == c  = Isosceles
                  | a*a + b*b == c*c            = Rectangular
                  | otherwise                   = Other