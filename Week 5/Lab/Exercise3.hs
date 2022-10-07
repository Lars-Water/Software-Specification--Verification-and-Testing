-- This file is property of the group Notorious Fortunate Panda © 2022
-- Time spent on this exercise was: ?? minutes

module Exercise3 where
import Mutation
import Exercise1
import Exercise2
import MultiplicationTable
import Test.QuickCheck
import Data.List
import Data.Function
import Data.Maybe

{-
    To calculate the minimal property susbset
    we will first create the powerset of (list of) properties
    to get all the possible combinations of properties.
    Then we will count the number of survivors for each combination
    and determine the which combinations lead to the minimum amount of survivors.
    I if there is only, this is the minimal property subset.
    If there are multiple combinations with the minimum
    we select the subset with the smallest number of properties.
    For example, if [prop1, prop2] has the same amount of survivors as [prop1, prop2, prop3]
    then the subset containing prop1 and prop2 is the minimal property set.
    If there are multiple subsets with the same amount of properties
    (and with the minimum amount of survivors), we return them both.
-}


-- Returns the minimal property subset
-- Type definition of the function: mutator -> number of mutants -> [properties] -> functions -> [properties]
minPropTest :: Eq a => (a -> Gen a) -> Integer -> [(a -> Integer -> Bool, String)] -> (Integer -> a) -> Gen [[String]]
minPropTest mutator n_mutants props fun = do
    let powerProps = powerSet props
    surviversPerProps <- sequence $ map (\propSubset -> countSurvivors mutator n_mutants (map (\(prop,name) -> prop) propSubset ) fun) powerProps
    let minSurvivor = minimum surviversPerProps
    let propIndices = elemIndices minSurvivor surviversPerProps
    let propNames = map (\propSubset -> map (\propTuple -> snd propTuple) propSubset ) powerProps
    let minProp = map (\x -> propNames !! x) propIndices
    return $ minProp


-- Generate powerset of propery list: https://medium.com/@angerman/powersets-in-haskell-1df9684db52a
powerSet :: [(a -> Integer -> Bool, String)] -> [[(a -> Integer -> Bool, String)]]
powerSet [] = [[]]
powerSet (x:xs) = map (x:) (powerSet xs) ++ powerSet xs



main3 :: IO ()
main3 = do
    let properties = [(prop_tenElements, "prop_tenElements"), (prop_linear, "prop_linear"), (prop_firstElementIsInput, "prop_firstElementIsInput")]
    testOne <- generate $ minPropTest addElements 3000 properties multiplicationTable
    print $ testOne
    let min = length (minimumBy (compare `on` length) testOne)
    print $ filter (\x -> length x == min) testOne
