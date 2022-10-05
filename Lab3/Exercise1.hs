import Data.List
import System.Random
import Test.QuickCheck
import Lecture3

contradiction :: Form -> Bool
contradiction f = not $ satisfiable f

tautology :: Form -> Bool
tautology f = all (\ v -> evl v f) (allVals f)

-- | logical entailment
entails :: Form -> Form -> Bool
entails f g = tautology $ Impl f g

-- | logical equivalence
equiv :: Form -> Form -> Bool
equiv f g = (map (\ v -> evl v f) (allVals f)) == (map (\ v -> evl v g) (allVals g))