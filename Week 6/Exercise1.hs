-- This file is property of the group Notorious Fortunate Panda © 2022
-- Time spent on this exercise was: 45 minutes

module Exercise1 where
import SetOrd
import Test.QuickCheck
import Data.List
import System.Random (randomRIO)


-- Manual generator
manualGenerator :: IO (Set Int)
manualGenerator = do
    n <- randomRIO (1, 10)
    return (list2set [1..n])

-- QuickCheck generator
quickCheckGenerator :: Gen (Set Int)
quickCheckGenerator = do
    xs <- listOf arbitrary
    return (list2set xs)

-- Helper function to check if list only contains unique elements
diff :: (Eq a) => [a] -> Bool
diff []     = True
diff (x:xs) = x `notElem` xs && diff xs

-- Check if a set only contains unique elements
isUnique :: (Ord a) => Set a -> Bool
isUnique (Set xs) = diff xs

main1 :: IO ()
main1 = do
    print "Since Set's only contain unique elements we can test this data type by checking if the elements in a set are unique"
    print "Testing if a set only contains unique elements using the isUnique property"
    print "Manual generator:"
    set <- manualGenerator
    print set
    print (isUnique set)
    print "QuickCheck generator:"
    set <- generate quickCheckGenerator
    print set
    print (isUnique set)
    print "Applying quickCheck to the property:"
    quickCheck $ forAll quickCheckGenerator isUnique

