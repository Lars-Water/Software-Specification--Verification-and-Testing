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

    Our tests also does test for duplicates which means that if one list contains duplicates and the other does not, the permutation
    would return false, otherwise if they'd have the same amount of duplicates it would work.
    Our tes also checks for the size of the list, if the size is different it will return false. Thus we would need to change our
    test even if the input would contain duplicates. In this particular case our test does not work with duplicates.

    In our test we have chosen to go with system random as a replacement for quickcheck as this was overall easier to implement.
-}



module Exercise4
    (
    ) where
import Data.List
import System.Random
import Data.Array.IO
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
shuffle' x  | length x < 2 = return x
            | otherwise = do
	                    i <- randomRIO (0, length(x)-1)
	                    r <- shuffle' (take i x ++ drop (i+1) x)
	                    return (x!!i : r)

main :: IO ()
main = do
    n <- getRandomInt 10
    let xs = [1..n]
    listOne <- shuffle' xs -- listOne <- genRandomListN n (could not get this to work, due to the type of genRandomListN)
    listTwo <- shuffle' xs -- listTwo <- genRandomListN n
    putStrLn("Given the following lists")
    putStrLn("List 1: " ++ show listOne ++ "\nList 2: " ++ show listTwo)
    putStrLn("Are they permutations of each other? " ++ show (isPermutation listOne listTwo))
    putStrLn("Do these list have the same length? " ++ show (length listOne == length listTwo))
