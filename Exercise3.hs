-- This file is property of the group Notorious Fortunate Panda © 2022
-- Time spent: 55 minutes

{-
    We implemented all four properties in the order in which they appeared in exercise 3
    (namely: prop1, prop2, prop3 and prop4 down below).
    Unfortunately we were not able to find a way to print them.

    However we have implemented a way to test (not proof!) if the ordering corresponds with the one we
    found during the workshop: [prop1, prop2, prop4, prop3], with prop2 and prop 4 of equal strength.
    We have used a random int generator to generate a random number between 1 and 100,
    which is then used as input to test the properties.

    The test is run through the propertyShow function.
    The value of the generated number is printed first.
    Then all the results of applying the properties in the ordered list
    (using the order function) to this number
    And lastly, the results of applying the properties in the list with the known correct order.
    It should be noted that with this test it is not possible to check whether two properties
    of equal strength appear in the same order in the two lists,
    seeing as they give the same output for the same input number.

    We were not able to compare the properties in the two list directly,
    otherwise we could have used QuickCheck on them.
-}

module Exercise3
    (
    ) where
import Test.QuickCheck
import System.Random
import Data.List

infix 1 -->

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker xs p q = stronger xs q p

prop1, prop2, prop3, prop4 :: Int -> Bool
prop1 = (\ x -> even x && x > 3)
prop2 = even
prop3 = (\ x -> even x || x > 3)
prop4 = (\ x -> (even x && x > 3) || even x)

-- This function orders properties from strongest to weakest
-- If two properties are of equal strength, they remain in their original order
-- The domain used, [-10..10], was suggested in the assignment
order :: [Int->Bool] -> [Int->Bool]
order [z] = [z]
order (x:y:xs) | weaker [-10..10] x y = y : order (x:xs)
               | otherwise = x : order (y:xs)

getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (1,n))               

-- This order corresponds with the answer we found during the workshop
-- in which prop2 and 4 are of equal strength
descendingList = [prop1, prop2, prop4, prop3]

propertyShow ::  IO ()
propertyShow  = do
    x <- getRandomInt 100 -- get random number between 1 and 100
    putStrLn ("Value of variable is:" ++ show x ++ "\n")
    let list = order [prop1, prop2, prop3, prop4]
    putStrLn ("Value of the first ordered property is: " ++ ( show ((descendingList !! 0) x )))
    putStrLn ("Value of the second ordered property is: " ++ ( show ((descendingList !! 1) x )))
    putStrLn ("Value of the third ordered property is: " ++ ( show ((descendingList !! 2) x )))
    putStrLn ("Value of the fourth ordered property is: " ++ ( show ((descendingList !! 3) x ))  ++ "\n")
    putStrLn ("Value of the known strongest property is: " ++ ( show ((list !! 0) x )))
    putStrLn ("Value of the known second strongest property is: " ++ ( show ((list !! 1) x )))
    putStrLn ("Value of the known third strongest property is: " ++ ( show ((list !! 2) x )))
    putStrLn ("Value of the known weakest property is: " ++ ( show ((list !! 3) x )))
