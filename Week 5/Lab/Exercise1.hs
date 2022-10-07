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
    that is mutated in some way. We have taken the same function type into account
    and come up with the following possible mutations for a list of integers.
    Each will have an example and some of these we have implemented below as well.

    1. Adding duplicates to the list                <- NOT REALLY COVERED YET
    example: [1,2,3] -> [1,1,2,2,3,3]
    2. Removing all elements from the list          <- NOT YET COVERED
    example: [1,2,3] -> []
    3. Shuffle  the list                            <- NOT YET COVERED
    example: [1,2,3] -> [2,3,1]
    5. Splitting the list into sublists             <- NOT YET COVERED
    example: [1,2,3] -> [[1],[2],[3]]
    6. Wrap the original list into a new list       <- NOT YET COVERED
    example: [1,2,3] -> [[1,2,3]]

-}

genNum :: Integer -> Gen Integer
genNum x = abs `fmap` (arbitrary :: Gen Integer) `suchThat` (== x)

subs :: [Integer] -> [[Integer]]
subs [] = []
subs (x:xs) = [x] : subs xs

-- 1. Adding duplicates to the list
duplicated :: [Integer] -> Gen [Integer]
duplicated [] = return []
duplicated (x:xs) = do
    n <- genNum x
    s <- genNum x
    rest <- duplicated xs
    return $ (n:s:rest)

-- 2. Removing all elements from the list
emptied :: [Integer] -> Gen [Integer]
emptied xs = do
    sublist <- sublistOf xs
    let n = length sublist
    return $ take n sublist

-- 3. Reversing the list
shuffled :: [Integer] -> Gen [Integer]
shuffled xs = shuffle xs

-- -- 4. Splitting the list into sublists
-- subbed :: [Integer] -> Gen [[Integer]]
-- subbed xs = Gen $ subs xs




