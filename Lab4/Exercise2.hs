-- This file is property of the group Notorious Fortunate Panda © 2022
-- Time spent: 35 minutes

{-
    In the main function a nested list comprehension is created. The elements of every list element
    can be taken out of integer values in the range of 2-5. This allows for a sample containing possible
    variations in the range [2-5, 2-5, 2-5] which means there should be a representation of every type of
    triangle at least once.

    Looking at the filtered nested lists based on their Shape type, the results appear to follow the
    correct requirements for every triangle shape. These results imply the function to determine whether
    a shape is a triangle, and if so what triangle, is correcly implemented as there appear to be no
    abnormalities in this sample size.

    At the implementation of the various tests, we considered ordering the three values instead of
    making guard cases for each possibility. However looking at time constraint we thought the code would
    be more time efficient and readable if we used guard cases.

    In the triangle function we have based the tests on the strongest post-condition first.
    Before testing for the post-condition we first test to see if the values indeed form a triangle.
    This means that our order of sets would be as follows: Other >= Isosceles >= Equilateral >= Rectangular.
    However these sets are not strict subsets (except for the fact that Equilateral is a subset of Isoceles)
    of eachother but it is possible that they overlap.
    Since a Rectangular triangle is also an Isosceles triangle, but this doens't have to be.
-}

module Exercise2
    (
    ) where
import Data.List

data Shape = NoTriangle | Equilateral | Isosceles  | Rectangular | Other
                    deriving (Eq,Show)

-- This check is to make sure that each side is not equal or greater than the sum of the other sides
notNull :: Integer -> Integer -> Integer -> Bool
notNull x y z   | x >= (y + z) = False
                | y >= (x + z) = False
                | z >= (x + y) = False
                | otherwise = True

-- This check is to see if is isosceles
equalLegs :: Integer -> Integer -> Integer -> Bool
equalLegs x y z | x == y = True
                | y == z = True
                | x == z = True
                | otherwise = False

-- This check is to see if is equilateral
equalSides :: Integer -> Integer -> Integer -> Bool
equalSides x y z  | x == y && y == z = True
                  | otherwise = False

-- This check is to see if is rectangular
pythagoras :: Integer -> Integer -> Integer -> Bool
pythagoras x y z | x^2 + y^2 == z^2 = True
                 | y^2 + z^2 == x^2 = True
                 | x^2 + z^2 == y^2 = True
                 | otherwise = False

-- This function determines the type of triangle
triangle :: Integer -> Integer -> Integer -> Shape
triangle x y z  | notNull x y z == False = NoTriangle
                | pythagoras x y z == True = Rectangular
                | equalSides x y z == True = Equilateral
                | equalLegs x y z == True = Isosceles
                | otherwise = Other

main = do
    let triangles = [[a,b,c] | a <- [2..5], b <- [2..5], c <- [2..5]]
    putStrLn ("NoTriangles: " ++ show (filter (\[a,b,c] -> triangle a b c == NoTriangle ) triangles))
    putStrLn ("Equilateral: " ++ show (filter (\[a,b,c] -> triangle a b c == Equilateral) triangles))
    putStrLn ("Isosceles: "   ++ show (filter (\[a,b,c] -> triangle a b c == Isosceles  ) triangles))
    putStrLn ("Rectangular: " ++ show (filter (\[a,b,c] -> triangle a b c == Rectangular) triangles))
    putStrLn ("Other: "       ++ show (filter (\[a,b,c] -> triangle a b c == Other      ) triangles))