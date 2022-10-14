-- This file is property of the group Notorious Fortunate Panda © 2022
-- Time spent: 30 minutes --

module Exercise3 where
import Exercise2
import SetOrd
import Test.QuickCheck
import Data.List

type Rel a = [(a,a)]

-- Helper function to get the inverse of a relation
inverseRel :: Ord a => Rel a -> Rel a
inverseRel [] = []
inverseRel ((x,y):xs) = (y,x) : inverseRel xs

-- Helper function to get the union of two relations
unionRel :: Ord a => Rel a -> Rel a -> Rel a
unionRel [] ys = ys
unionRel (x:xs) ys = nub $ x : unionRel xs ys

symClos :: Ord a => Rel a -> Rel a
symClos [] = []
symClos xs = unionRel xs (inverseRel xs)

main3 :: IO ()
main3 = do
    print "Given a relation set of: [(1,2),(2,3),(3,4)]"
    print "The symmetric closure of this relation set is:"
    print (symClos [(1,2),(2,3),(3,4)])
