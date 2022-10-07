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

{-

    Conjectures equivalence:
        The approach to calculating equivalence conjectures starts with defining the powerset of the list of properties. Subsequently,
        mutation tests are performed on every subset in this powerset. Specification of the mutants that were killed should be returned
        from these mutation tests. Consequently, equivalence conjectures can be determined through comparison of the killed mutants where
        an equal return value implies equivalence.
    Conjecture implication:
        ...

-}

-- https://stackoverflow.com/questions/16108714/removing-duplicates-from-a-list-in-haskell-without-elem
rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs)   | x `elem` xs   = rmdups xs
                | otherwise     = x : rmdups xs

-- Generate powerset of propery list: https://medium.com/@angerman/powersets-in-haskell-1df9684db52a
powerProps :: [a] -> [[a]]
powerProps [] = [[]]
powerProps (x:xs) = map (x:) (powerProps xs) ++ powerProps xs

-- Return a nesetd list where every list nesting represents a subset of properties where every element represents
-- whether the mutant survived (True) or is killed (False)
survivors :: Eq a => (a -> Gen a) -> Integer -> [a -> Integer -> Bool] -> (Integer -> a) -> IO [Bool]
survivors mutator n_mutants props func = do
    let inputs = [0..n_mutants]
    results <- generate $ mapM (\x -> survived mutator props func x) inputs
    return results

-- For every property subset determine if the other property subsets killed the same mutants.
equivalence :: [([Bool],[Integer])] -> [[([[Integer]],Bool)]]
equivalence survivorsSubsets = do
    let equivalence_list = map (\(subset,props) -> map (\(compareSubset,compareProps) -> (sort([props] ++ [compareProps]), compareSubset == subset)) (delete (subset,props) survivorsSubsets)) survivorsSubsets
    return $ rmdups (filter (\(_,check) -> check) (concat equivalence_list))

-- For every property subset determine if the other property subsets killed the same mutants.
overrules :: [([Bool],[Integer])] -> [[([[Integer]],Bool)]]
overrules survivorsSubsets = do
    let equivalence_list = map (\(subset,props) -> map (\(compareSubset,compareProps) -> ([props] ++ [compareProps], impl compareSubset subset)) (delete (subset,props) survivorsSubsets)) survivorsSubsets
    return $ filter (\(_,check) -> check) (concat equivalence_list)

impl :: [Bool] -> [Bool] -> Bool
impl [] [] = True
impl [] xs = False
impl xs [] = False
impl (x:xs) (y:ys) | x == True && y == False = False
                   | otherwise = impl xs ys

{-
    Properties index:
        Property 1: Output list has exactly 10 elements
        Property 2: First number is the input
        Property 3: The sum of the output is the input times the 10th triangle number
        Property 4: The difference between consecutive elements is the input
        Property 5: Any element modulo the input is zero
-}
main5 :: IO ()
main5 = do
    --  Determine powerset of props
    let powersetProps = powerProps [prop_tenElements, prop_firstElementIsInput]
    --  Determine mutant survivors per property subset
    mutantSurvivors <- sequence $ map (\subsetProps ->
        survivors addElements 30 subsetProps multiplicationTable) powersetProps

    print ""
    print "Ten Elements && First Element is Input: "
    print $ mutantSurvivors!!0
    print ""
    print "Ten Elements: "
    print $ mutantSurvivors!!1
    print ""
    print "First Element is Input: "
    print $ mutantSurvivors!!2
    print ""
    print "No Props: "
    print $ mutantSurvivors!!3
    print ""
    print "Equivalence: "
    -- Determine equivalences between the subsets.
    let equivalences = map (\(props,_) -> props) (head $ equivalence (zip mutantSurvivors (powerProps [1..2])))
    print equivalences
    print ""
    print "Implies: "
    -- Determine equivalences between the subsets.
    let implies = map (\(props,_) -> props) (head $ overrules (zip mutantSurvivors (powerProps [1..2])))
    let equiv_filtered = (filter (\[x,y] -> not([x,y] `elem` equivalences)&&not([y,x] `elem` equivalences)) implies)
    print (filter (\[x,y] -> x /= []) equiv_filtered)
