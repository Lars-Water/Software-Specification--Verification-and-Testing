-- This file is property of the group Notorious Fortunate Panda © 2022
-- Time spent on this exercise was: 2.5 hours

module Exercise6 where
import Data.List
import System.Random
import Test.QuickCheck
import Test.QuickCheck.Gen
import Control.Monad
import Exercise4
import Lecture3

type Clause  = [Int]
type Clauses = [Clause]

{-  In this file we once again use our formGenerator from exercise 4 to test the property of Clause
    transformation. We state that if a fomrula is a CNF it can be converted into a Clause list, in which
    all unique props in the clause list should be the same as the unconverted formula. We check this
    by generating a random formula filtered on CNF's. The property should then hold for all CNF's.
    Which as we can see is indeed the case. This automatic testing is done in the main function.
-}

-- This helper function gets all props from a Form, even
-- if there occurences are more than once it will still
-- return that prop multiple times
propNames' :: Form -> Clause
propNames' = sort.pnames
            where
                pnames (Prop name) = [name]
                pnames (Neg (Prop name))  = [-name]

-- This helper function gets called when the cnf2cls
-- function recursively finds a disjunction.
-- It takes the list of Forms (either Props of Negatives)
-- of the disjunction as input and returns a Clause based on the originals list.
dsjform2cls :: [Form] -> Clause
dsjform2cls = concatMap propNames'

-- This function, given a CNF formula input, will return a list of clauses.
-- Since we can assume that the input is a CNF,
-- we only need to check for these four patterns below.
cnf2cls :: Form -> Clauses
cnf2cls (Prop x) = [[x]]
cnf2cls (Neg (Prop x)) = [[-x]]
cnf2cls (Dsj fs) = [dsjform2cls fs]
cnf2cls (Cnj fs) = concatMap cnf2cls fs

-- This property checks given a formula is a CNF. Transforming it to a list of Clauses the unique
-- properties in the formula are the same as the unique properties in the list of Clauses. It filters
-- on formula's that are CNF's and then checks if the unique properties in the formula are the same.
equalAmountOfProps :: Form -> Property
equalAmountOfProps f = isCNF' f ==> length (propNames f) == length (nub (concat (cnf2cls f)))

-- Given a CNF Form, if the highest scope of the formula is an Cnj, the number of Props included in the list of
-- Cnj should be the same as the number of clauses in the list of cnf2cls with the same Form input. We see that
-- this property is very specific and thus quickCheck has a hard time finding relevant testcases.
equalAmountOfCnj :: Form -> Property
equalAmountOfCnj f = isCNF' f && isCnj f ==> lengthOfCnj f == length (cnf2cls f)

-- Given a Form check if the highest scope is a Cnj
isCnj :: Form -> Bool
isCnj (Cnj f) = True
isCnj f = False

lengthOfCnj :: Form -> Int
lengthOfCnj (Cnj f) = length f
lengthOfCnj f = 0

main6 :: IO ()
main6 = do
    quickCheck $ forAll formGenerator equalAmountOfProps
    quickCheck $ forAll formGenerator equalAmountOfCnj
