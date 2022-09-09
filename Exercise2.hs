-- Redo exercise 4 of Workshop 1 and test the property for integer lists of the form [1..n].
-- You can use subsequences :: [a] -> [[a]] for the list of all subsequences of a given list.

-- Is the property hard to test? If you find that it is, can you given a reason why?

-- Answer: Yes, because the list of subsequences extends exponentially based on the initial list. Thus
-- testing will take exponentially longer.

-- Give your thoughts on the following issue: when you perform the test for exercise 4, what are you testing actually?
-- Are you checking a mathematical fact? Or are you testing whether subsequences satisfies a part of its specification?
-- Or are you testing something else still?

-- Answer: We are not proving a mathematical fact, however what we are actually testing is if the total amount of
-- subsequences of a list is equal to 2^n, where n is the chosen length of the list.


-- Time spent: 20 minutes
module Exercise2
    (
    ) where
import Test.QuickCheck
import Data.List

exer4 :: Int -> Property
exer4 x = x >= 0 ==> length (subsequences [1..x]) == 2^(length [1..x])

exercise2 :: IO()
exercise2 = do
  putStrLn  "Check if the amount of subsequences of a list is equal to 2^n"
  putStrLn "Chosen solution: first get the total amount of subsequences of a list, then get the length of that list"
  putStrLn "Then get the 2 to the power of the length of the original list (given x). And finally compare if these values are the same"
  putStrLn  "This code will run for quite some time due to the exponantial behaviour of subsequences Example output: "
  quickCheck exer4


