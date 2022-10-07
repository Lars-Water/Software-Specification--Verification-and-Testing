-- This file is property of the group Notorious Fortunate Panda © 2022
-- Time spent on this exercise was: 3.5 hours

module Exercise2 where
import Mutation
import Exercise1
import MultiplicationTable
import Test.QuickCheck
import Data.List
import Data.Maybe

-- This is how the function works:
-- Type definition of the function: mutator -> number of mutants -> list of properties -> function to test (multiplicationtable) -> suriving mutants
-- 1. Generate list of inputs for the function up till the number of mutants and mutate these
-- 2. For each of these mutants we use the mutate' function to see if it satisfies the properties
-- 3. If it does, we add it to the list of surviving mutants
countSurvivors ::  Eq a => (a -> Gen a) -> Integer -> [a -> Integer -> Bool] -> (Integer -> a) -> Gen Integer
countSurvivors mutator n_mutants props func = do
    let inputs = [1..n_mutants]
    results <- mapM (\x -> survived mutator props func x) inputs
    return $ toInteger $ length $ filter (==True) results

-- This helper function will return True if the mutant satisfies all the properties.
-- Example: the mutator addElements would work as follows
-- 1. We mutate the input: [1,2,3] -> [1,2,3,4]
-- 2. We check if the mutated input satisfies the properties: [1,2,3,4] -> [prop_tenElements, prop_linear] -> [True, False]
-- 3. If all the properties are satisfied, we return True, else False: [True, False] -> False
survived :: Eq a => (a -> Gen a) -> [a -> Integer -> Bool] -> (Integer -> a) -> Integer -> Gen Bool
survived mutator props func x = do
    mutants <- mutate' mutator props func x
    return $ all (==True) mutants


main2 :: IO ()
main2 = do
    putStrLn " "
    testOne <- generate $ countSurvivors addElements 4000 [prop_tenElements] multiplicationTable
    testTwo <- generate $ countSurvivors addElements 4000 [prop_tenElements, prop_linear] multiplicationTable
    testThree <- generate $ countSurvivors shuffled 4000 [prop_tenElements, prop_linear] multiplicationTable
    putStrLn "After implementation of the countSurvivors function we can see the following results per test"
    putStrLn "Test 1: 4000 mutants, properties: prop_tenElements, mutator: addElements"
    putStrLn "Number of survived mutants: "
    print testOne

    putStrLn "Test 2: 4000 mutants, properties: prop_tenElements, prop_linear, mutator: addElements"
    putStrLn "Number of survived mutants: "
    print testTwo

    putStrLn "Test 3: 4000 mutants, properties: prop_tenElements, prop_linear, mutator: shuffled"
    putStrLn "Number of survived mutants: "
    print testThree

    putStrLn ""
    putStrLn "Based on the tests that we ran above we can see the following results."
    putStrLn "If we have a random mutator such as addElements, we cans see that more properties will not necessarily lead to less surviving mutants."
    putStrLn "This is due to the randomness of the mutator which makes it hard to test for the same input"
    putStrLn ""
    putStrLn "However we doe believe that given a valid non-random mutator, a higher number of properties will lead to less surviving mutants."
    putStrLn "We also see that if a mutator does not change the content of the list, like our shuffled mutator. The test succesfully kills all mutants."

