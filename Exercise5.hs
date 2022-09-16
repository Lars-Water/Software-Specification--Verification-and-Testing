-- This file is property of the group Notorious Fortunate Panda © 2022
-- Time spent: 60 mintues

{-
    In the main function we have used a random integer generator to generate a random integer between 1 and 10.
    This determines to which values the list from 1 to random int will contain.

    Afterwhich we made a genarator that generates a random list given an integer. Unfortunately we could not get it to work
    due to the fact that the type of the Generator did not work in our main function. Thus we went with an alternative solution
    which meant we used a shuffle function which we found online.

    Given two randomized list with the same values our permutation function should thus always return true as it is a permutation
    Seeing as how both lists are of the same size our main function this indeed checks out.

    Next we test for derangement by checking by checking if each value in a given position in a list is different from the
    value of the same position in the other list. If this is the case we return true, else we return false. Furthermore we check
    that each derangement is of the same size as the original list and also a permutation which makes derangements a subset of
    permutations.

    In our test we have chosen to go with system random as a replacement for quickcheck as this was overall easier to implement.
-}

module Exercise5
    (
    ) where

import Data.List
import System.Random
import Test.QuickCheck
import Test.QuickCheck.Gen

getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (1,n))

genRandomListN :: Int -> Gen [Int]
genRandomListN n = (arbitrary :: Gen [Int]) `suchThat` (all (\x-> 0 < x && x <= n ) )

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] [] = True
isPermutation (x:xs) (y:ys) | length (x:xs) /= length (y:ys) = False
                            | x `notElem` (y:ys) = False
                            | otherwise = isPermutation xs (delete x (y:ys))

-- source: https://programming-idioms.org/idiom/10/shuffle-a-list/826/haskell
shuffle' :: [a] -> IO [a]
shuffle' x  | length x < 2 = return x
            | otherwise = do
	                    i <- randomRIO (0, length(x)-1)
	                    r <- shuffle' (take i x ++ drop (i+1) x)
	                    return (x!!i : r)


isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement xs ys = isPermutation xs ys && all (==True) (zipWith (/=) xs ys)

allDerangements :: Int -> [[Int]]
allDerangements n = filter (\x -> isDerangement x [1..n]) (permutations [1..n])

main :: IO ()
main = do
    n <- getRandomInt 5 -- we chose a max of 5 to make it easier more readable in the terminal
    let xs = [1..n]
    listOne <- shuffle' xs -- listOne <- genRandomListN n (could not get this to work, due to the type of genRandomListN)
    listTwo <- shuffle' xs -- listTwo <- genRandomListN n
    putStrLn("Given the following lists")
    putStrLn("List 1: " ++ show listOne ++ "\nList 2: " ++ show listTwo)
    putStrLn("Are they permutations of each other? " ++ show (isPermutation listOne listTwo))
    putStrLn("Do these list have the same length? " ++ show (length listOne == length listTwo))
    putStrLn("Are they derangements of each other? " ++ show (isDerangement listOne listTwo))
    putStrLn("All derangements of " ++ show n ++ " elements: " ++ show (allDerangements n))
