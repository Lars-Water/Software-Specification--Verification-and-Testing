-- This file is property of the group Notorious Fortunate Panda © 2022
-- Time spent on this exercise was: ?? minutes

module Exercise3 where
import Mutation
import Exercise1
import Exercise2
import MultiplicationTable
import Test.QuickCheck
import Data.List
import Data.Maybe

-- minPropSubsets ::
minPropSubsets fut props = do
    let powerset = propsPowerset props
    show $ length powerset

-- Generate powerset of propery list: https://medium.com/@angerman/powersets-in-haskell-1df9684db52a
propsPowerset :: [[Integer] -> Integer -> Bool] -> [[[Integer] -> Integer -> Bool]]
propsPowerset [] = [[]]
propsPowerset (x:xs) = map (x:) (propsPowerset xs) ++ propsPowerset xs
