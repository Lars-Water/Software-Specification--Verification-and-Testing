-- This file is property of the group Notorious Fortunate Panda © 2022
-- Time spent: 360 minutes --

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

    To calculate the conjectures we will first create the powerset of (list of) properties to get all the possible combinations of properties.
    For every subset of properties, a list is returned where the elements represent the state of the mutant (Survived=True & Killed=False).
    Mutation tests are performed on every subset in this powerset. Through utilization of these mutation tests and the helper functions
    'equivalence' and 'implication', the conjectures are determined for every subset of properties.

    Conjectures equivalence:
        Specification of the mutants that were killed should be returned from the mutation tests. Consequently, equivalence conjectures
        can be determined through comparison of the killed mutants where the same mutants killed implies equivalence.
        The 'equivalence' helper function maps the list of mutation tests for every property subset to the remaining mutation tests of property
        subsets. If the compared mutations tests portrayed equal results, the compared property subsets are added as an equivalence pair to the returned list.
        The returned list contains all property subset pairs with equivalence conjecture.
    Conjecture implication:
        Specification of the mutant states should be returned from the mutation tests. Consequently, implication conjectures
        can be determined through comparison of the killed mutants from a property subset where AT LEAST the same mutants killed in the
        other property subset implies implication.
        The 'implication' helper function maps the list of mutation tests for every property subset to the remaining mutation tests of property subsets.
        If AT LEAST all mutants killed in the first test of the compared mutations tests were also killed in the second test, the compared property subsets
        are added as an implication pair to the returned list.
        The returned list contains all property subset pairs with implication conjecture.

    *The example below was ran to check the funcitonality of the functions:
    Properties: [1,2]
    Combinations: [[1,2], [1], [2], []]]
    Survivors:
        [1,2] - [False,False,False,False,False,False,False,False,False,False]
        [1]   - [False,False,False,False,False,False,False,False,False,False]
        [2]   - [False,True,False,False,False,False,False,False,False,False]
        []    - [True,True,True,True,True,True,True,True,True,True]
    Resulting conjectures were: [1] == [1,2], [2] ==> [1,2], [2] ==> [1]

    The results from the above example run implies that the implementation for conjectures was done correctly.
    The equivalence conjectures holds for property subset 1 and 2 and property subset 1. Both these subsets killed the same mutants.
    The implication conjecture holds for property subset 2 to property subset 1 and 2 and for property subset 2 to property subset 1.
    The set of killed mutants from property subset 2 is a subset of killed mutants for both property subset 1 and property subset 1 and 2.

    Properties index:
        Property 1: Output list has exactly 10 elements
        Property 2: First number is the input
        Property 3: The sum of the output is the input times the 10th triangle number
        Property 4: The difference between consecutive elements is the input
        Property 5: Any element modulo the input is zero

-}

-- Remove duplicates from list: https://stackoverflow.com/questions/16108714/removing-duplicates-from-a-list-in-haskell-without-elem
rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs)   | x `elem` xs   = rmdups xs
                | otherwise     = x : rmdups xs

-- Generate powerset of propery list: https://medium.com/@angerman/powersets-in-haskell-1df9684db52a
powerProps :: [a] -> [[a]]
powerProps [] = [[]]
powerProps (x:xs) = map (x:) (powerProps xs) ++ powerProps xs

-- Return a nested list where every list nesting represents a subset of properties where every element represents
-- whether the mutant survived (True) or is killed (False)
-- Type definition of the function: mutator -> number of mutants -> [properties] -> function under test -> [Mutant state result]
survivors :: Eq a => (a -> Gen a) -> Integer -> [a -> Integer -> Bool] -> (Integer -> a) -> IO [Bool]
survivors mutator n_mutants props func = do
    let inputs = [0..n_mutants]
    results <- generate $ mapM (\x -> survived mutator props func x) inputs
    return results

-- For every property subset determine if the other property subsets killed the same mutants.
-- Type definition of the function: [([Mutant state],[Property subset Indices])] -> [[([[Property subset equivalence pair]],Equivalence boolean value)]]
equivalence :: [([Bool],[Integer])] -> [[([[Integer]],Bool)]]
equivalence survivorsSubsets = do
    let equivalence_list = map (\(subset,props) -> map (\(compareSubset,compareProps) -> (sort([props] ++ [compareProps]), compareSubset == subset)) (delete (subset,props) survivorsSubsets)) survivorsSubsets
    return $ rmdups (filter (\(_,check) -> check) (concat equivalence_list))

-- For every property subset determine if the other property subsets killed the same mutants.
-- Type definition of the function: [([Mutant state],[Property subset Indices])] -> [[([[Property subset implication pair]],Implication boolean value)]]
implication :: [([Bool],[Integer])] -> [[([[Integer]],Bool)]]
implication survivorsSubsets = do
    let implication_list = map (\(subset,props) -> map (\(compareSubset,compareProps) -> ([props] ++ [compareProps], impl compareSubset subset)) (delete (subset,props) survivorsSubsets)) survivorsSubsets
    return $ filter (\(_,check) -> check) (concat implication_list)

-- Helper function that determines if the first list is a subset of the second subset regarding
-- Type definition of the function: [Property subset 1] -> [Property subset 2] -> Property subset 1 is subset of property subset 2
impl :: [Bool] -> [Bool] -> Bool
impl [] [] = True
impl [] xs = False
impl xs [] = False
impl (x:xs) (y:ys) | x == True && y == False = False
                   | otherwise = impl xs ys

-- Exmample run
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
    -- Determine equivalences between the subsets. Only the equivalence conjuctures are selected to be printed to make it more readable.
    let equivalences = map (\(props,_) -> props) (head $ equivalence (zip mutantSurvivors (powerProps [1..2]))) -- `powerProps [1..2]` because two properties are tested
    print equivalences
    print ""
    print "Implies: "
    -- Determine implications between the subsets. Only the implication conjuctures are selected to be printed to make it more readable.
    let implies = map (\(props,_) -> props) (head $ implication (zip mutantSurvivors (powerProps [1..2]))) -- `powerProps [1..2]` because two properties are tested
    -- Filter out empty set and equivalence conjectures to result with the implication conjectures.
    let equiv_filtered = (filter (\[x,y] -> not([x,y] `elem` equivalences)&&not([y,x] `elem` equivalences)) implies)
    print (filter (\[x,y] -> x /= []) equiv_filtered)
