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

count_quartile :: Int -> [Int] -> Int
count_quartile n | n == 1    = length . filter (==1)
                 | n == 2    = length . filter (==2)
                 | n == 3    = length . filter (==3)
                 | otherwise = length . filter (==4)

determine_quartile :: Float -> Int
determine_quartile q | q < 0.25   = 1
                     | q < 0.5    = 2
                     | q < 0.75   = 3
                     | otherwise  = 4

check_quartiles :: [Float] -> [Int]
check_quartiles xs = map determine_quartile xs

main = do
    list <- probs 1000
    let quartile_indices = check_quartiles list
    putStrLn "Occurrences per quartile:"
    putStrLn ("1st Quartile - " ++ show (count_quartile 1 quartile_indices))
    putStrLn ("2nd Quartile - " ++ show (count_quartile 2 quartile_indices))
    putStrLn ("3rd Quartile - " ++ show (count_quartile 3 quartile_indices))
    putStrLn ("4th Quartile - " ++ show (count_quartile 4 quartile_indices))