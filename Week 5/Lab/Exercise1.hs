-- This file is property of the group Notorious Fortunate Panda © 2022
-- Time spent on this exercise was: 45 minutes

module Exercise1 where
import Mutation
import MultiplicationTable
import Test.QuickCheck
import Data.List
import Data.Maybe

{-  Looking at the Mutation.hs file we are given some examples
    of functions that given a list of integers, return a list of integers
    that is mutated in some way. All outputs should be covered by the anyList
    function from the examples. However, this is a very weak condition
    (any list of integers) is allowed, which is not necessarily desirable,
    because repeatin your mutation tests might lead to totally different outcomes.
    Here is an overview of outputs that are not covered by the other
    example functions:
    
    - outputs containing duplicates
    - outputs that are a (random) subset of the input
    - outputs with a reversed order
    - outputs with negated elements
    - outputs were the elements are multiplied
    - outputs with no elements
    
    We have used the same type definition as the examples into
    and came up with the following possible mutations for a list of integers.
    Each will have an example and (if applicable)
    a statement of whether it is weaker or stronger than other post conditions.
    (Stronger post conditions are more restrictive.)
    The first 9 of them we have implemented below.

    1. Duplicating each element of the list
    example: [1,2,3] -> [1,1,2,2,3,3]
    This postcondition is stronger than 13
    2. Removing all elements from the list
    example: [1,2,3] -> []
    This postcondition is stronger than 5
    3. Shuffling the list
    example: [1,2,3] -> [2,3,1]
    This postcondition is weaker than 4
    4. Reversing the list
    example: [1,2,3] -> [3,2,1]
    This postcondition is stronger than 3
    5. Taking a sublist
    example: [1,2,3,4] -> [1,3]
    This postcondition is weaker than 2 and 7
    6. Negating all elements
    example: [1,-2,3] -> [-1,2,-3]
    This postcondition is stronger than 10
    7. Removing 1 to (length - 1) elements from the front of the list
    example: [1,2,3,4] -> [3,4]
    This postcondition is stronger than 5
    8. Squaring all elements of the list
    example: [1,2,3] -> [1,4,9]
    9. Duplicating entire list
    example: [1,2,3] -> [1,2,3,1,2,3]

    10. Negating elements (randomly)
    example: [1,-2,-3,4] -> [-1,-2,3,-4]
    This poscondition is weaker than 6
    11. Multiplying all elements with the same (random) number
    example (with number 4): [1,2,3] -> [4,8,12]
    12. Multiplying all elements with a different (random) number
    example (with numbers 8,6,2): [1,2,3] -> [8,12,1]
    13. Duplicating elements (randomly)
    example: [1,2,3] -> [1,1,1,2,3,3]
    This postcondition is weaker than 1
    14. Repeating the list a (random) number of times
    example: [1,2,3] -> [1,2,3,1,2,3,1,2,3]

-}

genNum :: Integer -> Gen Integer
genNum x = abs `fmap` (arbitrary :: Gen Integer) `suchThat` (== x)

-- 1. Duplicating each element of the list
duplicateElements :: [Integer] -> Gen [Integer]
duplicateElements [] = return []
duplicateElements (x:xs) = do
    n <- genNum x
    s <- genNum x
    rest <- duplicateElements xs
    return $ (n:s:rest)

-- 2. Removing all elements from the list
emptied :: [Integer] -> Gen [Integer]
emptied xs = return []

-- 3. Shuffling the list
shuffled :: [Integer] -> Gen [Integer]
shuffled xs = shuffle xs

-- 4. Reversing the list
reversed :: [Integer] -> Gen [Integer]
reversed xs = return $ reverse xs

-- 5. Taking a sublist
sublist :: [Integer] -> Gen [Integer]
sublist xs = sublistOf xs

-- 6. Negating all elements
negating :: [Integer] -> Gen [Integer]
negating xs = return $ map negate xs

-- 7. Removing 1 to (length - 1) elements from the front of the list
removeElementsFront :: [Integer] -> Gen [Integer]
removeElementsFront xs = choose (1, length xs - 1) >>= \x -> return $ drop x xs

-- 8. Squaring all elements
square :: [Integer] -> Gen [Integer]
square xs = return $ map (^2) xs

-- 9. Duplicating entire list
duplicateList :: [Integer] -> Gen [Integer]
duplicateList xs = return $ xs ++ xs


