-- This file is property of the group Notorious Fortunate Panda © 2022
-- Time spent on this exercise was: 4.5 hours

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
-- Type definition of the function: mutator -> number of mutants -> [(properties, name of property)] -> function -> [minimal property subset]
minPropTest :: Eq a => (a -> Gen a) -> Integer -> [(a -> Integer -> Bool, String)] -> (Integer -> a) -> Gen [String]
minPropTest mutator n_mutants props fun = do
    let powerProps = powerSet props
    surviversPerProps <- sequence $ map (\propSubset -> countSurvivors mutator n_mutants (map (\(prop,name) -> prop) propSubset ) fun) powerProps
    let minSurvivor = minimum surviversPerProps
    let propIndices = elemIndices minSurvivor surviversPerProps
    let propNames = map (\propSubset -> map (\propTuple -> snd propTuple) propSubset ) powerProps
    let minPropList = map (\x -> propNames !! x) propIndices
    let min = length (minimumBy (compare `on` length) minPropList)
    return $ concat $ filter (\x -> length x == min) minPropList


-- Generate powerset of propery list: https://medium.com/@angerman/powersets-in-haskell-1df9684db52a
powerSet :: [(a -> Integer -> Bool, String)] -> [[(a -> Integer -> Bool, String)]]
powerSet [] = [[]]
powerSet (x:xs) = map (x:) (powerSet xs) ++ powerSet xs


main3 :: IO ()
main3 = do
    putStrLn " "
    putStrLn "Minimal property subset for addElements mutator"
    putStrLn "Given the following properties: prop_tenElements, prop_linear, prop_firstElementIsInput"
    putStrLn "The minimal property subset is: "
    let properties = [(prop_tenElements, "prop_tenElements"), (prop_linear, "prop_linear"), (prop_firstElementIsInput, "prop_firstElementIsInput")]
    testOne <- generate $ minPropTest addElements 3000 properties multiplicationTable
    print $ testOne
    putStrLn " "
    putStrLn "Note that since addElements is a random mutator, each time minPropTest is ran, a different minimal property subset is returned."
    putStrLn "We can see as well that if we were to increase the mutants to 10000, the minimal property subset would be: "
    testTwo <- generate $ minPropTest addElements 10000 properties multiplicationTable
    print $ testTwo
    putStrLn " "
    putStrLn "However since addElements is a random mutator, it's hard to say what the difference in mutants means for the minimal property subset."
    putStrLn "To check for this we will run minPropTest for the other mutators as well, in this specific case we chose the shuffled mutator. Since this mutator is contentwise the same a increase in mutants should not change the minimal property subset."
    testThree <- generate $ minPropTest shuffled 3000 properties multiplicationTable
    print $ testThree
    testFour <- generate $ minPropTest shuffled 10000 properties multiplicationTable
    print $ testFour
    putStrLn "We can indeed see that the minimal property subset is the same for both 3000 and 10000 mutants. This means to us that the increase in mutants our test does not find a stronger or weaker minimal property subset."
    putStrLn " "
    putStrLn "Finally we would like to note that we have based our minimal property subset on which smallest possible combinations of properties lead to the least amount of survivors."
    putStrLn " "