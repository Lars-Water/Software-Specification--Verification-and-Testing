-- This file is property of the group Notorious Fortunate Panda © 2022
-- Time spent on this exercise was: 2 hours

module Exercise5 where

import Data.List
import SetOrd

type Rel a = [(a,a)]

infixr 5 @@

-- Function that returns the relational compostion of the given sets.
(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s =
  nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

-- Function that returns the union or two relations.
relUnion ::  Ord a => Rel a -> Rel a -> Rel a
relUnion [] rel2  =  rel2
relUnion (x:xs) rel2  =
   x : (relUnion xs rel2)

-- Function that determines if the given set has the transitive property. Found in the Haskell Road to Logic book on page 179.
    {-
        The function takes in a relational set. The empty set is transitive and, therefore, returns True.

        Otherwise, transitivity is checked as follow:
            1.) Perform a list comprehension where every element represents a transitivity check for a relation (x,y) in the set of relations.
            2.) This transitivity check itself returns a function with input the given relation and the corresponding set of relations. In this function,
                a list comprehension is performed where all relations (y, z) are taken from the set of relations.
                For every element in the list comprehension it is checked if the relation (x, z) is in the given set of relations. On the created list
                comprehension a conjunction of Boolean values returns if the relation (x,y) is transitive.
            3.) A conjunction of Boolean values for every relation (x,y) in the set of relations determine if the set of relations has the transitivity property.
    -}
transitivityRelations :: Ord a => Rel a -> Bool
transitivityRelations []        = True
transitivityRelations relations = and [ checkTrans relation relations | relation <- relations ]
    where checkTrans (x,y) r = and [ (x,z) `elem` r | (u,z) <- r, u == y ]

-- Function that returns the transitive closure of the given set of relations.
    {-
        If the given set of relations does not have the transitivity property, the function is called upon recursively with the Union of the given set and
        the relational composition of this given set with itself.

        This implementation is taken from the notion that the transitive closure can be computed as:
            R^+ = R ∪ R^2 ∪ R^3 ∪ · · · R^k     for some k

        This notion is presented in the Logic in Action book (J. van Benthem, et al. 2016)
    -}
trClos ::  Ord a => Rel a -> Rel a
trClos relations |  transitivityRelations relations = nub relations
                 |  otherwise = trClos (relUnion relations (relations @@ relations))

main5 = do
    print "Given a relation set of: [(1,2),(2,3),(3,4)]"
    print "The transitive closure of this relation set is:"
    print $ trClos [(1,2),(2,3),(3,4)]
