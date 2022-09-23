-- This file is property of the group Notorious Fortunate Panda © 2022
-- Time spent on this exercise was: 25 minutes

module Exercise1 where
import Data.List
import System.Random
import Test.QuickCheck
import Lecture3

{-  Description of our method of checking the definitions:
    in the main function we have given a few examples formulas
    of which we are certain these are either contradictions, tautologies,
    equivalances or entailments.
    For each we tested to see if these were indeed the case.
-}

-- logical contradiction
contradiction :: Form -> Bool
contradiction f = not (satisfiable f)

-- logical tautology
tautology :: Form -> Bool
tautology f = all (\ v -> evl v f) (allVals f)

-- logical entailment
entails :: Form -> Form -> Bool
entails f g = tautology $ Impl f g

-- logical equivalence
equiv :: Form -> Form -> Bool
equiv f g = tautology $ Equiv f g

main1 :: IO ()
main1 = do
    print ("Here we have given some examples of formulas behind which we have stated the\
        \rightfull boolean accoring to our functions. We have checked these by hand aswell")
    print ("The formula (p AND ~p) is a contradiction: " ++ show ( contradiction (Cnj [p, Neg p])))
    print ("The formula (p) is a contradiction: " ++ show (contradiction p))
    print ("The formula (p OR ~p) is a tautology: " ++ show (tautology (Dsj [p, Neg p])))
    print ("The formula (p) is a tautology: " ++ show (tautology p))
    print ("The formula (p -> q) entails (q -> p): " ++ show (entails (Impl p q) (Impl q p)))
    print ("The formula (p -> p) entails (p -> p): " ++ show (entails p p))
    print ("The formula (p -> q) is equivalent to (q -> p): " ++ show (equiv (Impl p q) (Impl q p)))
    print ("The formula (p -> p) is equivalent to (p -> p): " ++ show (equiv p p))

