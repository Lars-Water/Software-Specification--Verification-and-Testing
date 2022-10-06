module Exercise2 where

import Mutation
import MultiplicationTable
import Test.QuickCheck
import Data.List

-- countSurvivors :: Integer -> [([Integer] -> Integer -> Bool)] -> (Integer -> [Integer]) -> Gen Integer
-- countSurvivors n_mutants props fut

main = generate $ mutate removeElements prop_tenElements multiplicationTable 1
