

-- Time spent on this exercise was: 45 minutes 
-- Do i need quickcheck here?

module Exercise1 
where
import Data.List
import System.Random
import Test.QuickCheck
import Lecture3

contradiction :: Form -> Bool
contradiction f = not (satisfiable f)

tautology :: Form -> Bool
tautology f = all (\ v -> evl v f) (allVals f)

-- | logical entailment
entails :: Form -> Form -> Bool
entails f g | satisfiable f = satisfiable g
            | not (satisfiable f) = True
            | otherwise = False

 -- | logical equivalence
equiv :: Form -> Form -> Bool
equiv f g = allVals f == allVals g