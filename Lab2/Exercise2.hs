import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

data Shape = NoTriangle | Equilateral | Isosceles | Rectangular | Other deriving (Eq,Show)

{-
    Sort the side lengths to easily determine NoTriangle and Rectangular.
-}
triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c = triangle' sides
    where sides = sort [a,b,c]

triangle' :: [Integer] -> Shape
triangle' [a,b,c] | a + b <= c                  = NoTriangle
                  | a == b && b == c            = Equilateral
                  | a == b || b == c || a == c  = Isosceles
                  | a*a + b*b == c*c            = Rectangular
                  | otherwise                   = Other

main = do
    let triangles = [[a,b,c] | a <- [2..5], b <- [2..5], c <- [2..5]]
    putStrLn ("NoTriangles: " ++ show (filter (\[a,b,c] -> triangle a b c == NoTriangle ) triangles))
    putStrLn ("Equilateral: " ++ show (filter (\[a,b,c] -> triangle a b c == Equilateral) triangles))
    putStrLn ("Isosceles: "   ++ show (filter (\[a,b,c] -> triangle a b c == Isosceles  ) triangles))
    putStrLn ("Rectangular: " ++ show (filter (\[a,b,c] -> triangle a b c == Rectangular) triangles))
    putStrLn ("Other: "       ++ show (filter (\[a,b,c] -> triangle a b c == Other      ) triangles))

{-
    In the main function a nested list comprehension is created. The elements of every list element
    can be taken out of integer values in the range of 2-5. This allows for a sample containing possible
    variations in the range [2-5, 2-5, 2-5] which means there should be a representation of every type of
    triangle at least once.

    Looking at the filtered nested lists based on their Shape type, the results appear to follow the
    correct requirements for every triangle shape. These results imply the function to determine whether
    a shape is a triangle, and if so what triangle, is correcly implemented as there appear to be no
    abnormalities in this sample size.
-}
