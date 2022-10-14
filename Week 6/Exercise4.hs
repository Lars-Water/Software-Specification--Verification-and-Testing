-- This file is property of the group Notorious Fortunate Panda © 2022
-- Time spent on this exercise was: 5.5 hours

module Exercise4 where
import Exercise3
import SetOrd
import Test.QuickCheck
import Data.List

genPos :: Gen Int
genPos = abs `fmap` (arbitrary :: Gen Int) `suchThat` (> 0)

-- Domain generator
domainGenerator :: Gen [Int]
domainGenerator = do
    n <- choose (5, 10) -- choose size of at most 10 and at least 5
    m <- (listOf genPos)
    return $ take n (nub m)

-- Relational generator
-- This generator uses as input a given domain, then it generates
-- A random new domain to make random relations. This menas that given a domain
-- it should always generate a serial relational set.
relationalGenerator :: [Int] -> Gen (Rel Int)
relationalGenerator x = do
    ys <- domainGenerator
    return $ zip x ys

isSerial :: Eq a => [a] -> Rel a -> Bool
isSerial [] _ = True
isSerial (x:xs) ys = x `elem` map fst ys && isSerial xs ys

-- Poperty to test if the relational set of a domain is at most the length of the domain
-- This is the case since a serial relational set is a subset of the domain.
rightLength :: Eq a => [a] -> Rel a -> Bool
rightLength [] _ = True
rightLength x xs =  length xs >= length x

-- Property to test if the relational set is serial
-- This is the case since a relational set minimum value should always be the same as the
-- domain minimum value.
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

{-  4.C Consider the relation R = {(x, y) | x = y(mod n)}, where (mod n) is the modulo function in modular arithmetic and n > 0.
    Discuss whether (and when) R is serial. How can you test whether R is serial? How can you prove that R is serial?
    Answer: If we look at mod we know that if n is > y then y mod n will result in y. Thus if we want to make sure given a domain:
    [1,2,3] that the relation is serial we need to make sure that the relation is [(1,1),(2,2),(3,3)]. We can ensure this by making sure
    that n is greater or equal than the maximum element in the domain. We can then surely say that each x has a relation in the relational set
    with y. However when n becomes smaller than the maximum element (let's say we choose n = 2) we will end up with a relational set such as [(1,1), (2,2) (1,3)]
    As we can see this time the relational set is not serial anymore.
    So we can test this by defining a property that makes sure n is greater or equal to the maximum element of the domain
-}

main4 :: IO ()
main4 = do
    print "Testing if the relational set is serial"
    print "For this we use two distinct generatos, one for the domain and one for the relational set"
    print "Notice that our relational set is always a subset of the domain, since it uses the domain as input"
    domain <- generate domainGenerator
    print domain
    relations <- generate (relationalGenerator domain)
    print relations
    print "notice that the is right length property will always pass since the relational set is always smaller than the domain"
    print "this is because that is how our generator works, it makes a zipped list of the domain and a random generated other domain"
    print "thus it will never become bigger than the domain"
    quickCheck $ forAll domainGenerator $ \set1 -> forAll (relationalGenerator set1) $ \set2 -> isRightLength set1 set2
    print "howwever this of course is not full-proof as we can see in the following test, let's say we have a domain of"
    print "[1,2,3] and a relational set that generates anothe domain of [1,2] we will end up with the relational set of [(1,1),(2,2)]"
    print "which since we're missing the 3 is not serial"
    quickCheck $ forAll domainGenerator $ \set1 -> forAll (relationalGenerator set1) $ \set2 -> isSameInitial set1 set2