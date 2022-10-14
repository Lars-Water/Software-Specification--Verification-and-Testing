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

-- Helper function to test if a set only contains unique elements
uniqueElems :: Ord a => Set a -> Bool
uniqueElems (Set []) = True
uniqueElems (Set (x:xs)) = not (elem x xs) && uniqueElems (Set xs)

setIntersection ::   Ord a => Set a -> Set a -> Set a
setIntersection (Set xs) (Set ys) = Set (intersect xs ys)

setUnion ::  Ord a => Set a -> Set a -> Set a
setUnion (Set []) set2  =  set2
setUnion (Set (x:xs)) set2  =
   insertSet x (setUnion (Set xs) set2)

setDifference ::  Ord a => Set a -> Set a -> Set a
setDifference (Set xs) (Set ys) = Set (xs \\ ys)

-- Properties for intersection

-- Property to test set intersection -> the intersection of two sets should only contain unique elements
uniqueElemsIntersect :: Ord a => Set a -> Set a -> Bool
uniqueElemsIntersect set1 set2 = uniqueElems $ setIntersection set1 set2

-- Property to test set intersection -> the intersect of two sets can be at most the length of the smallest set
maxIntersectLength :: Ord a => Set a -> Set a -> Bool
maxIntersectLength set1 set2 =  intersectLength <= getLength set1 && intersectLength <= getLength set2
    where intersectLength = getLength (setIntersection set1 set2)

-- Property to test if Intersection of an arbitrary set with the empty set will return the empty set.
emptySetIntersection :: Ord a => Set a -> Bool
emptySetIntersection set1 = isEmpty $ setIntersection set1 emptySet

-- Test idempotent property of Set Intersection. The intersection of an arbitrary set with itself returns the arbitrary set.
idempotenceIntersection :: Ord a => Set a -> Bool
idempotenceIntersection set1 = (setUnion set1 set1) == set1

-- Test commutative property of Set Intersection. The order of the intersection on the sets does not change the returned set.
commutativityIntersection :: Ord a => Set a -> Set a -> Bool
commutativityIntersection set1 set2 = (setIntersection set1 set2) == (setIntersection set2 set1)

-- Test associative property of Set Intersection. Sets under Intersection grouped through parentheses return the same set when the parentheses are changed.
associativityIntersection :: Ord a => Set a -> Set a -> Set a -> Bool
associativityIntersection set1 set2 set3 = (setIntersection set1 (setIntersection set2 set3)) == (setIntersection set3 (setIntersection set1 set2))

-- Test distributive property of Set Intersection. Intersection of a set with grouped sets under Union return the same set as
-- Union of all individual Intersection of the single set with the grouped sets.
distributivityIntersection :: Ord a => Set a -> Set a -> Set a -> Bool
distributivityIntersection set1 set2 set3 = (setIntersection set1 (setUnion set2 set3)) == setUnion (setIntersection set1 set2) (setIntersection set1 set3)

-- Properties for union

-- Property to test set union -> the union of two sets should only contain unique elements
uniqueElemsUnion :: Ord a => Set a -> Set a -> Bool
uniqueElemsUnion set1 set2 = uniqueElems $ setUnion set1 set2

-- Property to test set union -> the union of two sets can be at most the length of the sum of the lengths of the two sets
maxUnionLength :: Ord a => Set a -> Set a -> Bool
maxUnionLength set1 set2 = unionLength <= getLength set1 + getLength set2
    where unionLength = getLength (setUnion set1 set2)

-- Property to test if Union of an arbitrary set with the empty set will return the arbitrary set.
emptySetUnion :: Ord a => Set a -> Bool
emptySetUnion set1 = (setUnion set1 emptySet) == set1

-- Test idempotent property of Set Union. The unification of an arbitrary set with itself returns the arbitrary set.
idempotenceUnion :: Ord a => Set a -> Bool
idempotenceUnion set1 = (setUnion set1 set1) == set1

-- Test commutative property of Set Union. The order of the unification on the sets does not change the returned set.
commutativityUnion :: Ord a => Set a -> Set a -> Bool
commutativityUnion set1 set2 = (setUnion set1 set2) == (setUnion set2 set1)

-- Test associative property of Set Union. Sets under Union grouped through parentheses return the same set when the parentheses are changed.
associativityUnion :: Ord a => Set a -> Set a -> Set a -> Bool
associativityUnion set1 set2 set3 = (setUnion set1 (setUnion set2 set3)) == (setUnion set3 (setUnion set1 set2))

-- Test distributive property of Set Union. Union of a set with grouped sets under Intersection return the same set as
-- Intersection of all individual Unions of the single set with the grouped sets.
distributivityUnion :: Ord a => Set a -> Set a -> Set a -> Bool
distributivityUnion set1 set2 set3 = (setUnion set1 (setIntersection set2 set3)) == setIntersection (setUnion set1 set2) (setUnion set1 set3)

-- Properties for Difference

-- Property to test set difference -> the difference of two sets should only contain unique elements
uniqueElemsDiff :: Ord a => Set a -> Set a -> Bool
uniqueElemsDiff set1 set2 = uniqueElems $ setDifference set1 set2

-- Property to test set difference -> the difference of two sets can be at most the length of the first set
maxDifferenceLength :: Ord a => Set a -> Set a -> Bool
maxDifferenceLength set1 set2 = differenceLength <= getLength set1
    where differenceLength = getLength (setDifference set1 set2)

-- Property to test set intersection + difference -> the intersection of (the intersection of two sets and the difference of these sets) 
-- should be empty
intersectOfDifferenceAndIntersect :: Ord a => Set a -> Set a -> Bool
intersectOfDifferenceAndIntersect set1 set2 = isEmpty $ setIntersection (setIntersection set1 set2) (setDifference set1 set2)

-- Property to test set difference -> the intersection of (the difference of two sets and the difference of these same sets 
-- placed the other way around) should be empty
intersectOfDifferenceAndDifference :: Ord a => Set a -> Set a -> Bool
intersectOfDifferenceAndDifference set1 set2 = isEmpty $ setIntersection (setDifference set1 set2) (setDifference set2 set1)

main2 :: IO ()
main2 = do
    print "Testing the properties of the set intersection"
    print "Testing the property that the intersection of two sets only contains unique elements"
    print "QuickCheck generator:"
    set1 <- generate quickCheckGenerator
    set2 <- generate quickCheckGenerator
    print set1
    print set2
    print "Applying quickCheck to the property:"
    quickCheck $ forAll quickCheckGenerator $ \set1 -> forAll quickCheckGenerator $ \set2 -> uniqueElemsIntersect set1 set2
    
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
    
    print "Testing the property that the intersection of a set with the empty set returns the empty set."
    print "QuickCheck generator:"
    set1 <- generate quickCheckGenerator
    print set1
    print "Intersection with empty set:"
    print (setIntersection set1 emptySet)
    print "Does the property hold?"
    print (emptySetIntersection set1)
    print "Applying quickCheck to the property:"
    quickCheck $ forAll quickCheckGenerator $ \set1 -> emptySetIntersection set1
    
    print "Testing the idempotence property under Intersection."
    print "QuickCheck generator:"
    set1 <- generate quickCheckGenerator
    print set1
    print "Idempotence under Intersection:"
    print (setIntersection set1 set1)
    print "Does the property hold?"
    print (idempotenceIntersection set1)
    print "Applying quickCheck to the property:"
    quickCheck $ forAll quickCheckGenerator $ \set1 -> idempotenceIntersection set1
    
    print "Testing the intersection property under Intersection"
    print "QuickCheck generator:"
    set1 <- generate quickCheckGenerator
    set2 <- generate quickCheckGenerator
    print set1
    print set2
    print "Set 1 Intersection Set 2:"
    print (setIntersection set1 set2)
    print "Set 2 Intersection Set 1:"
    print (setIntersection set2 set1)
    print "Does the property hold?"
    print (commutativityIntersection set1 set2)
    print "Applying quickCheck to the property:"
    quickCheck $ forAll quickCheckGenerator $ \set1 -> forAll quickCheckGenerator $ \set2 -> commutativityIntersection set1 set2

    print "Testing the associativity property under Intersection"
    print "QuickCheck generator:"
    set1 <- generate quickCheckGenerator
    set2 <- generate quickCheckGenerator
    set3 <- generate quickCheckGenerator
    print set1
    print set2
    print set3
    print "Set 1 Intersection (Set 2 Intersection Set 3):"
    print (( setIntersection set1 (setIntersection set2 set3)))
    print "Set 3 Intersection (Set 1 Intersection Set 2):"
    print (( setIntersection set3 (setIntersection set1 set2)))
    print "Does the property hold?"
    print (associativityIntersection set1 set2 set3)
    print "Applying quickCheck to the property:"
    quickCheck $ forAll quickCheckGenerator $ \set1 -> forAll quickCheckGenerator $ \set2 -> forAll quickCheckGenerator $ \set3 -> associativityIntersection set1 set2 set3

    print "Testing the distributivity property under Intersection"
    print "QuickCheck generator:"
    set1 <- generate quickCheckGenerator
    set2 <- generate quickCheckGenerator
    set3 <- generate quickCheckGenerator
    print set1
    print set2
    print set3
    print "Set 1 INTERSECTION (Set 2 UNION Set 3):"
    print ( setIntersection set1 (setUnion set2 set3))
    print "(Set 1 INTERSECTION Set 2) UNION (Set 1 INTERSECTION Set 3):"
    print (setUnion (setIntersection set1 set2) (setIntersection set1 set3))
    print "Does the property hold?"
    print (distributivityIntersection set1 set2 set3)
    print "Applying quickCheck to the property:"
    quickCheck $ forAll quickCheckGenerator $ \set1 -> forAll quickCheckGenerator $ \set2 -> forAll quickCheckGenerator $ \set3 -> associativityIntersection set1 set2 set3


    print "Testing the properties of the set union"
    print "Testing the property that the union of two sets only contains unique elements"
    print "QuickCheck generator:"
    set1 <- generate quickCheckGenerator
    set2 <- generate quickCheckGenerator
    print set1
    print set2
    print "Applying quickCheck to the property:"
    quickCheck $ forAll quickCheckGenerator $ \set1 -> forAll quickCheckGenerator $ \set2 -> uniqueElemsUnion set1 set2

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

    print "Testing the property that the union of a set with the empty set returns the set."
    print "QuickCheck generator:"
    set1 <- generate quickCheckGenerator
    print set1
    print "Union with empty set:"
    print (setUnion set1 emptySet)
    print "Does the property hold?"
    print (emptySetUnion set1)
    print "Applying quickCheck to the property:"
    quickCheck $ forAll quickCheckGenerator $ \set1 -> emptySetUnion set1

    print "Testing the idempotence property under Union."
    print "QuickCheck generator:"
    set1 <- generate quickCheckGenerator
    print set1
    print "Idempotence under Union:"
    print (setUnion set1 set1)
    print "Does the property hold?"
    print (idempotenceUnion set1)
    print "Applying quickCheck to the property:"
    quickCheck $ forAll quickCheckGenerator $ \set1 -> idempotenceUnion set1

    print "Testing the commutativity property under Union"
    print "QuickCheck generator:"
    set1 <- generate quickCheckGenerator
    set2 <- generate quickCheckGenerator
    print set1
    print set2
    print "Set 1 Union Set 2:"
    print (setUnion set1 set2)
    print "Set 2 Union Set 1:"
    print (setUnion set2 set1)
    print "Does the property hold?"
    print (commutativityUnion set1 set2)
    print "Applying quickCheck to the property:"
    quickCheck $ forAll quickCheckGenerator $ \set1 -> forAll quickCheckGenerator $ \set2 -> commutativityUnion set1 set2

    print "Testing the associativity property under Union"
    print "QuickCheck generator:"
    set1 <- generate quickCheckGenerator
    set2 <- generate quickCheckGenerator
    set3 <- generate quickCheckGenerator
    print set1
    print set2
    print set3
    print "Set 1 Union (Set 2 Union Set 3):"
    print (( setUnion set1 (setUnion set2 set3)))
    print "Set 3 Union (Set 1 Union Set 2):"
    print (( setUnion set3 (setUnion set1 set2)))
    print "Does the property hold?"
    print (associativityUnion set1 set2 set3)
    print "Applying quickCheck to the property:"
    quickCheck $ forAll quickCheckGenerator $ \set1 -> forAll quickCheckGenerator $ \set2 -> forAll quickCheckGenerator $ \set3 -> associativityUnion set1 set2 set3

    print "Testing the distributivity property under Union"
    print "QuickCheck generator:"
    set1 <- generate quickCheckGenerator
    set2 <- generate quickCheckGenerator
    set3 <- generate quickCheckGenerator
    print set1
    print set2
    print set3
    print "Set 1 UNION (Set 2 INTERSECTION Set 3):"
    print (setUnion set1 (setIntersection set2 set3))
    print "(Set 1 UNION Set 2) INTERSECTION (Set 1 UNION Set 3):"
    print (setIntersection (setUnion set1 set2) (setUnion set1 set3))
    print "Does the property hold?"
    print (distributivityUnion set1 set2 set3)
    print "Applying quickCheck to the property:"
    quickCheck $ forAll quickCheckGenerator $ \set1 -> forAll quickCheckGenerator $ \set2 -> forAll quickCheckGenerator $ \set3 -> associativityUnion set1 set2 set3


    print "Testing the properties of the set union"
    print "Testing the property that the difference of two sets only contains unique elements"
    print "QuickCheck generator:"
    set1 <- generate quickCheckGenerator
    set2 <- generate quickCheckGenerator
    print set1
    print set2
    print "Applying quickCheck to the property:"
    quickCheck $ forAll quickCheckGenerator $ \set1 -> forAll quickCheckGenerator $ \set2 -> uniqueElemsDiff set1 set2

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

    print "Testing the property that the intersection of the difference of two sets and the difference of the same sets but flipped is empty"
    print "QuickCheck generator:"
    set1 <- generate quickCheckGenerator
    set2 <- generate quickCheckGenerator
    print set1
    print set2
    print "Applying quickCheck to the property:"
    quickCheck $ forAll quickCheckGenerator $ \set1 -> forAll quickCheckGenerator $ \set2 -> intersectOfDifferenceAndIntersect set1 set2
    
    print "Testing the property that the intersection of the difference of two sets and the intersection of the same two sets is empty"
    print "QuickCheck generator:"
    set1 <- generate quickCheckGenerator
    set2 <- generate quickCheckGenerator
    print set1
    print set2
    print "Applying quickCheck to the property:"
    quickCheck $ forAll quickCheckGenerator $ \set1 -> forAll quickCheckGenerator $ \set2 -> intersectOfDifferenceAndDifference set1 set2
