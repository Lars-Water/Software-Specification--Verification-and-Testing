module Exercise4 where

import Data.List
import System.Random
import Test.QuickCheck
import SetOrd

type Rel a = [(a,a)]

isSerial :: Eq a => [a] -> Rel a -> Bool
isSerial domain relations = all (==True) serialElements
    where serialElements = map (\domainElement -> (any (==True) (testSerial domainElement relations domain))) domain

testSerial domainElement relations domain = map (\(_,y) -> y `elem` domain) filtered
    where filtered = (filter (\(x,_) -> domainElement==x) relations)

main = do
    print $ isSerial [1,2,3] [(1,2),(2,3)]
    print $ isSerial [1,2,3] [(1,2),(2,3),(3,1)]
