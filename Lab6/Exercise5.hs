module Exercise5 where

import Data.List
import System.Random
import Test.QuickCheck
import SetOrd
import Exercise4

infixr 5 @@

-- Relational composition
(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s =
  nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

-- trClos ::  Ord a => Rel a -> Rel a
-- trClos relations = (list2set relations) @@ (list2set relations)

main5 = do
    -- [(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)]
    -- print $ trClos [(1,2),(2,3),(3,4)]
    print $ [(1,2),(2,3),(3,4)] @@ [(1,3),(1,4),(2,4),(4,1)]
