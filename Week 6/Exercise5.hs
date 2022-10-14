-- This file is property of the group Notorious Fortunate Panda © 2022
-- Time spent on this exercise was: 2 hours

module Exercise5 where
import Data.List
import SetOrd
import Exercise3

infixr 5 @@

-- Relational composition
(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s =
  nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

-- Union of relationships
relUnion ::  Ord a => Rel a -> Rel a -> Rel a
relUnion [] rel2  =  rel2
relUnion (x:xs) rel2  =
   x : (relUnion xs rel2)

-- Taken from The Haskell Road: No idea what trans and pair do
transR :: Ord a => Rel a -> Bool
transR [] = True
transR s = and [ trans pair s | pair <- s ] where
    trans (x,y) r =
        and [ (x,v) `elem` r | (u,v) <- r, u == y ]

trClos ::  Ord a => Rel a -> Rel a
trClos relations |  transR relations = nub relations
                 |  otherwise = trClos (relUnion relations (relations @@ relations))

main5 = do
    print "Given a relation set of: [(1,2),(2,3),(3,4)]"
    print "The transitive closure of this relation set is:"
    print $ trClos [(1,2),(2,3),(3,4)]