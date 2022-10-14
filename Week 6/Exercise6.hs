-- This file is property of the group Notorious Fortunate Panda © 2022
-- Time spent: 165 Minutes --

module Exercise6 where
import Exercise3
import Exercise4
import Exercise5
import SetOrd
import Test.QuickCheck
import Data.List


-- A symmetric closure of a relation R should be at least the same size as R.
-- Thus a symmetric closure is always a superset of R.
supersetSymclos :: Ord a => Rel a -> Bool
supersetSymclos r = length (symClos r) >= length r

-- A symmetric closure of a relation R should always contain both the original relations of R.
-- And the inverse of the original relations of R.
checkInverse :: Ord a => Rel a -> Bool
checkInverse r = and $ map (\(x,y) -> (y,x) `elem` symClos r && (x,y) `elem` symClos r) r

-- A transitive closure of a relation R should be at least the same size as R.
-- Thus a transitive closure is always a superset of R.
supersetTrclos :: Ord a => Rel a -> Bool
supersetTrclos r = length (trClos r) >= length r

-- For each element in the transitive closure of R that is not in R the following should hold:
-- given (x,y) in (trClos R) - R there should be a (x, b -> b /= y) and (b, y) in R.
-- example:
-- R = [(1,2),(2,3),(3,4)]
-- trClos R = [(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)]
-- (trClos R) - R = [(1,3),(1,4),(2,4)]
-- (1,3) should be in R because there is a (1, b -> b /= 3) and (b, 3) in R.
prevsetTrclos :: Ord a => Rel a -> Bool
prevsetTrclos r = and $ map (\(x,y) -> (fstRelationInTrclos x y r) && (sndRelationInTrclos x y r)) diff
    where diff = trClos r \\ r

-- Helper function to check given a relation checks if there is a (x, b -> b /= y) in R.
fstRelationInTrclos :: Ord a => a -> a -> Rel a -> Bool
fstRelationInTrclos x y r =  not $ null $ filter (\(a,b) -> a == x && b /= y) (trClos r)

-- Helper function to check given a relation checks if there is a (b -> b /= y, y) in R.
sndRelationInTrclos :: Ord a => a -> a -> Rel a -> Bool
sndRelationInTrclos x y r =  not $ null $ filter (\(a,b) -> a /= y && b == y) (trClos r)

main6 :: IO ()
main6 = do
    print "Symmetric closure is a superset of the original relation set:"
    quickCheck $ forAll domainGenerator $ \set1 -> forAll (relationalGenerator set1) $ \set2 -> supersetSymclos set2
    print "Symmetric closure contains the original relation set and the inverse of the original relation set:"
    quickCheck $ forAll domainGenerator $ \set1 -> forAll (relationalGenerator set1) $ \set2 -> checkInverse set2
    print "Transitive closure is a superset of the original relation set:"
    quickCheck $ forAll domainGenerator $ \set1 -> forAll (relationalGenerator set1) $ \set2 -> supersetTrclos set2
    print "For each element in the transitive closure of R that is not in R the following should hold:"
    print "given (x,y) in (trClos R) - R there should be a (x, b -> b /= y) and (b, y) in R."
    print "example:"
    print "R = [(1,2),(2,3),(3,4)]"
    print "trClos R = [(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)]"
    print "(trClos R) - R = [(1,3),(1,4),(2,4)]"
    print "(1,3) should be in R because there is a (1, b -> b /= 3) and (b, 3) in R."
    quickCheck $ forAll domainGenerator $ \set1 -> forAll (relationalGenerator set1) $ \set2 -> prevsetTrclos set2
