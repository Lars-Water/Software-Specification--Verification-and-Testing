-- This file is property of the group Notorious Fortunate Panda © 2022
-- Time spent on this exercise was: 4.5 hours

module Exercise5 where
import Mutation
import Exercise1
import Exercise2
import Exercise3
import MultiplicationTable
import Test.QuickCheck
import Data.List
import Data.Function
import Data.Maybe

{-
    To calculate the conjectures
    we will first create the powerset of (list of) properties
    to get all the possible combinations of properties.
    Then we will count the number of survivors for each combination
    and determine based on the survivors which conjectures can be made.

    Example:
    Properties: [1,2,3]
    Combinations: [[1], [2], [3], [1,2], [1,3], [2,3], [1,2,3]]
    Survivors: [1, 2, 0, 2, 0, 0, 0]
    Some example conjectures: 1 ==> 2, 3 ==> 1, [1,3] == [2,3]
    Do note that this function will not be able to check for disjunctions.
    So both property 1 and 2 might have 1 survivor and we will state them as equivalent,
    but the survivor might be different making them disjoint
-}

-- Returns the number of survivors for each property subset
-- Type definition of the function: mutator -> number of mutants -> [(properties, name of property)] -> function -> [Number of survivors]
surviversPerProps :: Eq a => (a -> Gen a) -> Integer -> [(a -> Integer -> Bool, String)] -> (Integer -> a) -> Gen [Integer]
surviversPerProps mutator n_mutants props fun = do
    let powerProps = powerSet props
    survivors <- sequence $ map (\propSubset -> countSurvivors mutator n_mutants (map (\(prop,name) -> prop) propSubset ) fun) powerProps
    return $ survivors

main5 :: IO ()
main5 = do
    let properties = [(prop_tenElements, "prop_tenElements"), (prop_linear, "prop_linear"), (prop_firstElementIsInput, "prop_firstElementIsInput")]
    let props = map (\x -> map (\(prop,name) -> name ) x) (powerSet properties)
    print props
    testOne <- generate $ surviversPerProps addElements 3000 properties multiplicationTable
    print $ testOne