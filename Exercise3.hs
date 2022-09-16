-- This file is property of the group Notorious Fortunate Panda © 2022
-- Time spent: 35 minutes

{-
    We have used a random int generator to generate a random number between 1 and 100, to check the properties
    
    





    
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

-- forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (1,n))

stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker xs p q = stronger xs q p

prop1, prop2, prop3, prop4 :: Int -> Bool
prop1 = (\ x -> even x && x > 3)
prop2 x = even x
prop3 = (\ x -> even x || x > 3)
prop4 = (\ x -> (even x && x > 3) || even x)

-- This corresponds with the answers we found in the exercises
-- prop2 and 4 of equal strength
descendingList = [prop1, prop2, prop4, prop3]

order :: [Int->Bool] -> [Int->Bool]
order [z] = [z]
order (x:y:xs) | weaker [-10..10] x y = y : order (x:xs)
               | otherwise = x : order (y:xs)

propertyShow ::  IO ()
propertyShow  = do
    x <- getRandomInt 100 -- get random number between 1 and 100
    let list = order [prop1, prop2, prop3, prop4]
    putStrLn ("Value of the first property is: " ++ ( show ((list !! 1) x )))
    putStrLn ("Value of the second property is: " ++ ( show ((list !! 2) x )))
    putStrLn ("Value of the third property is: " ++ ( show ((list !! 3) x )))
    putStrLn ("Value of the fourth property is: " ++ ( show ((list !! 4) x )))