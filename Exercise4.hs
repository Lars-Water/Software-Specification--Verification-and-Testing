-- Time spent: 15 mintues
module Exercise4
    (
    ) where
import Test.QuickCheck
import Data.List

isPrime :: Integer -> Bool -- helper to check wether given number is prime
isPrime k   | k > 1 = null [ x | x <- [2..k - 1], k `mod` x == 0]
            | otherwise = False

reversal :: Integer -> Integer -- helper to reverse a number
reversal = read . reverse . show

reversibleStream :: [Integer]
reversibleStream = filter (\x -> isPrime x && isPrime (reversal x)) [1..10000]

allIsPrime :: [Integer] -> Bool -- helper to check wether all numbers in a list are prime
allIsPrime [] = True
allIsPrime (x:xs) = isPrime x && allIsPrime xs


exercise4 :: IO()
exercise4 = do
  putStrLn  "This function gets all the primes and also its reversal counterpart prime under the 10000"
  putStrLn  "To test this we check if all the values in the result list are prime"
  putStrLn  "which is:"
  print (allIsPrime reversibleStream)
  putStrLn "Chosen solution: first get the total amount of subsequences of a list, then get the length of that list"
  putStrLn "Then get the 2 to the power of the length of the original list (given x). And finally compare if these values are the same"
  putStrLn  "This code will run for quite some time due to the exponantial behaviour of subsequences Example output: "
  print (reversibleStream)
