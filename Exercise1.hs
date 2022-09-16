-- This file is property of the group Notorious Fortunate Panda © 2022
-- Time spent on this exercise was: 50 minutes

{-
    Every run with a sample size of 10000 randomly generated numbers returns an occurrence of roughly
    2500 numbers in every quartile. These results imply that the probs function simulates a uniformly
    distributed random number generator. We therefore can assume that Curry was right about his
    statement.

    Furthermore, the occurrence count shows that the total number of occurrences sum up to 10,000, the
    total sample size. This can also be seen in our test were we show the original length of the list
    and the sum of each of the quartiles. For this exercise we have chosen to stick with the sample size
    of 10000, but we have also tested the function with a sample size of 1000 and 5000 to which we saw
    similar results.

    These results can be derrived from the main function.
-}

module Exercise1
    (
    ) where
import Data.List
import Data.Char
import System.Random

probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
    p <- getStdRandom random
    ps <- probs (n-1)
    return (p:ps)

amountInList :: [Float]-> Float -> Float -> Float
amountInList [] _ _= 0
amountInList (x:xs) min max
        | x <= max && x >= min = 1 + amountInList xs min max
        | otherwise = amountInList xs min max

evenQuartiles :: [Float] -> IO ()
evenQuartiles xs = do
    let firstQuartile = amountInList xs 0 0.25
    let secondQuartile = amountInList xs 0.25 0.5
    let thirdQuartile = amountInList xs 0.5 0.75
    let fourthQuartile = amountInList xs 0.75 1
    let lengthList = fromIntegral (length xs) :: Float

    putStrLn("\nThis test was carried out with a list length of: " ++ show (length xs))
    putStrLn("Quartiles:")
    putStrLn("First quartile percentage: " ++ show (firstQuartile / lengthList))
    putStrLn("Second quartile percentage: " ++ show (secondQuartile / lengthList))
    putStrLn("Third quartile percentage: " ++ show (thirdQuartile / lengthList))
    putStrLn("Fourth quartile percentage: " ++ show (fourthQuartile / lengthList))
    putStrLn("\nSum of all the quartiles: " ++ show(firstQuartile + secondQuartile + thirdQuartile + fourthQuartile))
    putStrLn("\nExample deviation looking at the first quartile we get a deviation of: " ++ show ((abs ((firstQuartile / lengthList) - 0.25) / 0.25) * 100) ++ " %")

main :: IO ()
main = do
    li <- probs 10000
    evenQuartiles li

