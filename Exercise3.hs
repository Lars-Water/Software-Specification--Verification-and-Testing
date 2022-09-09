-- Redo exercise 5 of Workshop 1 and test the property for integer lists of the form [1..n].

-- Is the property hard to test? If you find that it is, can you given a reason why?

-- Answer:  This property also seems hard to test due to the fact that this property grows factorial
--          with the size of any given x for the original list.

-- Again, give your thoughts on the following issue: when you perform the test for exercise 5,
-- what are you testing actually? Are you checking a mathematical fact?
-- Or are you testing whether perms satisfies a part of its specification?
-- Or are you testing something else still?

-- Answer:  Here we are testing wether the length of all possible permutations of a list is the same
--          as the the factorial of the length of the original list. We cannot say we are testing a
--          mathematical fact, because we are not testing all possible numbers. We are testing whether perms returns a list of the correct length
--          However this does not mean that the elements of the list are correct

-- Time spent: 35 minutes

module Exercise3
    (
    ) where
import Test.QuickCheck
import Data.List


factorial :: Int -> Int -- helper to calculate the factorial of a given number
factorial n | n < 2 = 1
            | otherwise =  n * factorial (n-1)

-- this helper function is used to get all the permutations of a given list (given in the question)
perms :: [a] ->[[a]]
perms [] = [[]]
perms (x:xs) = concat (map (insrt x) (perms xs))
    where   insrt x [] = [[x]]
            insrt x (y:ys) = (x:y:ys) : map (y:) (insrt x ys)

exer5 :: Int -> Property
exer5 x = x > 1 ==> length (perms [1..x]) == factorial x