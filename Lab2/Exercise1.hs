import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
    p <- getStdRandom random
    ps <- probs (n-1)
    return (p:ps)

{-
    Return the number of occurrences in a specific quartile.

    Inspired from StackExchange code:
-}
count_quartile :: Int -> [Int] -> Int
count_quartile n | n == 1    = length . filter (==1)
                 | n == 2    = length . filter (==2)
                 | n == 3    = length . filter (==3)
                 | otherwise = length . filter (==4)

{-
    Determine the quartile the given float value is located in.
-}
determine_quartile :: Float -> Int
determine_quartile q | q < 0.25   = 1
                     | q < 0.5    = 2
                     | q < 0.75   = 3
                     | otherwise  = 4

{-
    Return a list where every randomly generated number is set to the quartile it
    is located in.
-}
check_quartiles :: [Float] -> [Int]
check_quartiles xs = map determine_quartile xs

main = do
    list <- probs 10000
    let quartile_indices = check_quartiles list
    let quartile1 = count_quartile 1 quartile_indices
    let quartile2 = count_quartile 2 quartile_indices
    let quartile3 = count_quartile 3 quartile_indices
    let quartile4 = count_quartile 4 quartile_indices
    putStrLn "Occurrences per quartile:"
    putStrLn $ "1st Quartile - " ++ show quartile1
    putStrLn $ "2nd Quartile - " ++ show quartile2
    putStrLn $ "3rd Quartile - " ++ show quartile3
    putStrLn $ "4th Quartile - " ++ show quartile4
    putStrLn $ "Total numbers generated: " ++ show (quartile1 + quartile2 + quartile3 + quartile4)

{-
    Every run with a sample size of 10,000 randomly generated numbers returns an occurrence of roughly
    2500 numbers in every quartile. These results imply that the probs function simulates a uniformly
    distributed random number generator. We therefore can assume that Curry was right about his
    statement.

    Furthermore, the occurrence count shows that the total number of occurrences sum up to 10,000, the
    total sample size.
-}
