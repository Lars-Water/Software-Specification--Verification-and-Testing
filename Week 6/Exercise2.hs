-- This file is property of the group Notorious Fortunate Panda © 2022
-- Time spent on this exercise was: 3.45 hours

module Exercise2 where
import Exercise1
import SetOrd
import Test.QuickCheck
import Data.List

-- Helper function to get the length of a set
getLength :: Ord a => Set a -> Int
getLength (Set xs) = length xs

setIntersection ::   Ord a => Set a -> Set a -> Set a
setIntersection (Set xs) (Set ys) = Set (intersect xs ys)

setUnion ::  Ord a => Set a -> Set a -> Set a
setUnion (Set []) set2  =  set2
setUnion (Set (x:xs)) set2  =
   insertSet x (setUnion (Set xs) set2)

setDifference ::  Ord a => Set a -> Set a -> Set a
setDifference (Set xs) (Set ys) = Set (xs \\ ys)

-- IK HEB HIER 3 PROPERTIES DIE ALLEEN DE LENGTE TESTEN
-- MEER PROPERTIES ZIJN NODIG EN WE MOETEN BESCHRIJVEN WAT ZE PRECIES TESTEN
-- EEN VOORBEELD KAN ZIJN BIJ INTERSECT: TESTEN OF INTERSECT ALLEEN UNIEKE ELEMENTEN BEVAT,
-- TESTEN OF INTERSECT ALLEEN ELEMENTEN BEVAT DIE IN BEIDE SETS ZITTEN

-- Property to test set intersection -> the intersect of two lists can be at most the length of the smallest list
maxIntersectLength :: Ord a => Set a -> Set a -> Bool
maxIntersectLength set1 set2 =  intersectLength <= getLength set1 && intersectLength <= getLength set2
    where intersectLength = getLength (setIntersection set1 set2)

-- Property to test set union -> the union of two lists can be at most the length of the sum of the lengths of the two lists
maxUnionLength :: Ord a => Set a -> Set a -> Bool
maxUnionLength set1 set2 = unionLength <= getLength set1 + getLength set2
    where unionLength = getLength (setUnion set1 set2)

-- Property to test set difference -> the difference of two lists can be at most the length of the first list
maxDifferenceLength :: Ord a => Set a -> Set a -> Bool
maxDifferenceLength set1 set2 = differenceLength <= getLength set1
    where differenceLength = getLength (setDifference set1 set2)

main2 :: IO ()
main2 = do
    -- quickCheck $ forAll quickCheckGenerator $ \set1 -> forAll quickCheckGenerator $ \set2 -> HIEPROPERTY set1 set2
    print "Testing the properties of the set intersection, union and difference functions"
    print "Testing the property that the intersection of two sets can be at most the length of the smallest set"
    print "QuickCheck generator:"
    set1 <- generate quickCheckGenerator
    set2 <- generate quickCheckGenerator
    print set1
    print set2
    print "Intersection:"
    print (setIntersection set1 set2)
    print "Does the property hold?"
    print (maxIntersectLength set1 set2)
    print "Applying quickCheck to the property:"
    quickCheck $ forAll quickCheckGenerator $ \set1 -> forAll quickCheckGenerator $ \set2 -> maxIntersectLength set1 set2
    print "Testing the property that the union of two sets can be at most the length of the sum of the lengths of the two sets"
    print "QuickCheck generator:"
    set1 <- generate quickCheckGenerator
    set2 <- generate quickCheckGenerator
    print set1
    print set2
    print "Union:"
    print (setUnion set1 set2)
    print "Does the property hold?"
    print (maxUnionLength set1 set2)
    print "Applying quickCheck to the property:"
    quickCheck $ forAll quickCheckGenerator $ \set1 -> forAll quickCheckGenerator $ \set2 -> maxUnionLength set1 set2
    print "Testing the property that the difference of two sets can be at most the length of the first set"
    print "QuickCheck generator:"
    set1 <- generate quickCheckGenerator
    set2 <- generate quickCheckGenerator
    print set1
    print set2
    print "Difference:"
    print (setDifference set1 set2)
    print "Does the property hold?"
    print (maxDifferenceLength set1 set2)
    print "Applying quickCheck to the property:"
    quickCheck $ forAll quickCheckGenerator $ \set1 -> forAll quickCheckGenerator $ \set2 -> maxDifferenceLength set1 set2