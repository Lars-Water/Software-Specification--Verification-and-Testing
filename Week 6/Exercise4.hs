-- This file is property of the group Notorious Fortunate Panda © 2022
-- Time spent on this exercise was: 45 minutes

module Exercise4 where
import Exercise3
import SetOrd
import Test.QuickCheck
import Data.List

-- Domain generator
domainGenerator :: Gen [Pos Int]
domainGenerator = do
    xs <- listOf arbitrary
    return xs

isSerial :: Eq a => [a] -> Rel a -> Bool
isSerial [] _ = True
isSerial (x:xs) ys = x `elem` map fst ys && isSerial xs ys

rightLength :: Eq a => [a] -> Rel a -> Bool
rightLength [] _ = True
rightLength x xs =  length xs >= length x

sameIntial :: Ord a => [a] -> Rel a -> Bool
sameIntial [] _ = True
sameIntial xs ys = minimum xs == minimum ( map (fst) ys)

-- Property to make sure that the relational set is at least the length of the domain
-- Example:  length [1,2,3] <=  length [(1,2),(2,3),(3,4)]  is true
isRightLength :: Eq a => [a] -> Rel a -> Bool
isRightLength xs ys = isSerial xs ys == rightLength xs ys

-- Property to make sure that the relational set contains the same initial element as the domain
-- Example:  minimum [1,2,3] ==  minimum [(1,2),(2,3),(3,4)]  is true
isSameInitial :: Ord a => [a] -> Rel a -> Bool
isSameInitial xs ys = isSerial xs ys == sameIntial xs ys




-- main4 :: IO ()
-- main4 = do
--     quickCheck $ isSerial isFromList