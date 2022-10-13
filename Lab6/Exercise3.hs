module Exercise3 where

import Data.List
import System.Random
import Test.QuickCheck
import SetOrd

type Rel a = [(a,a)]

-- Invert a relation: Taken from Haskell Road to Logic
invR :: Ord a => Rel a -> Rel a
invR [] = []
invR ((x,y):r) = (y,x) : (invR r)

-- symClos :: Ord a => Rel a -> Rel a
