-- This file is property of the group Notorious Fortunate Panda © 2022
-- Time spent on this exercise was: ?? minutes

module Exercise5 where
import Mutation
import Exercise1
import Exercise2
import Exercise3
import MultiplicationTable
import Test.QuickCheck
import Data.List
import Data.Maybe

{-

    Conjectures equivalence:
        The approach to calculating equivalence conjectures starts with defining the powerset of the list of properties. Subsequently,
        mutation tests are performed on every subset in this powerset. Specification of the mutants that were killed should be returned
        from these mutation tests. Consequently, equivalence conjectures can be determined through comparison of the killed mutants where
        an equal return value implies equivalence.
    Conjecture implication:
        ...

-}

-- Generate powerset of propery list: https://medium.com/@angerman/powersets-in-haskell-1df9684db52a
testPower :: [a -> Integer -> Bool] -> [[a -> Integer -> Bool]]
testPower [] = [[]]
testPower (x:xs) = map (x:) (testPower xs) ++ testPower xs

survivors mutator n_mutants props func = do
    let inputs = [0..n_mutants]
    results <- generate $ mapM (\x -> survived mutator props func x) inputs
    return results

-- Input:
--  Survivors per subset
equivalence :: [([Bool],[Char])] -> [[([Char],Bool)]]
equivalence survivorsSubsets = do
    let equivalence_list = map (\(subset,name) -> map (\(compareSubset,compareName) -> (name ++ " == " ++ compareName, compareSubset == subset)) (delete (subset,name) survivorsSubsets)) survivorsSubsets
    return $ filter (\(_,check) -> check) (concat equivalence_list)

-- Returns the number of survivors for each property subset
-- Type definition of the function: mutator -> number of mutants -> [(properties, name of property)] -> function -> [Number of survivors]
surviversPerProps :: Eq a => (a -> Gen a) -> Integer -> [(a -> Integer -> Bool, String)] -> (Integer -> a) -> Gen [Integer]
surviversPerProps mutator n_mutants props fun = do
    let powerProps = powerSet props
    survivors <- sequence $ map (\propSubset -> countSurvivors mutator n_mutants (map (\(prop,name) -> prop) propSubset ) fun) powerProps
    return $ survivors


main5 :: IO ()
main5 = do
    let properties = [(prop_tenElements, "prop_tenElements"), (prop_firstElementIsInput, "prop_firstElementIsInput")]
    let props = map (\x -> map (\(prop,name) -> name ) x) (powerSet properties)
    print props
    testOne <- generate $ surviversPerProps addElements 3000 properties multiplicationTable
    print $ testOne

    let powersetProps = testPower [prop_tenElements, prop_firstElementIsInput]   --  Determine powerset of props
    mutantSurvivors <- sequence $ map (\subsetProps ->                           --  Determine mutant survivors per subset
        survivors addElements 100 subsetProps multiplicationTable) powersetProps
    let equivalences = equivalence (zip mutantSurvivors ["prop_tenElements & prop_firstElementIsInput", "prop_firstElementIsInput", "prop_tenElements", "NULL"])
    return equivalences

-- -- main5 :: IO [[Bool]]
-- main5 = do
--     let powersetProps = testPower [prop_tenElements, prop_firstElementIsInput]   --  Determine powerset of props
--     mutantSurvivors <- sequence $ map (\subsetProps ->                           --  Determine mutant survivors per subset
--         survivors addElements 100 subsetProps multiplicationTable) powersetProps
--     -- Determine equivalences between the subsets.
--     let equivalences = equivalence (zip mutantSurvivors ["prop_tenElements & prop_firstElementIsInput", "prop_firstElementIsInput", "prop_tenElements", "NULL"])
--     return equivalences
--     -- return mutantSurvivors
