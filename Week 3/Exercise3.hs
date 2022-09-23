-- This file is property of the group Notorious Fortunate Panda © 2022
-- Time spent on this exercise was: 6 hours

module Exercise3 where
import Data.List
import System.Random
import Test.QuickCheck
import Test.QuickCheck.Gen
import Control.Monad
import Lecture3

{-
    The cnf function transforms a propositional logical formula to its conjunction normal form.
    First of all all implications and equivalences are transformed with use of the provided 'arrowfree'
    function. Secondly, negation elimination was performed with the provided 'nnf' function.
    Following these functions, the distributions of disjunction over conjunction were transformed by
    traversal into the formula with the created 'distribute' formula.
    Basic properties and Negation properties are returned when these encompass the provided form. There
    are no further nested forms after these.
    Conjunction forms can have either Basic, Negated, Conjunction or Disjunction forms. Therefore,
    'distribute' is called upon both child forms in order to traverse further into the formula. No
    transformations are initiated as CNF conversion does not require this.
    Similarly, Disjunctive forms are traversed in the same manner. However, another function is called upon
    when done traversing called 'disjunction'. This function performs a disjunction transformation
    if the patterns of the child forms match a distribution of disjunction over conjunction. If this
    is not the pattern the original disjunction is returned instead.
-}

cnf :: Form -> Form
cnf = distribute . nnf . arrowfree

-- this helper function is used to create multiple disjunctions via map
createDsj :: Form -> Form -> Form
createDsj f g = Dsj [f,g]

distribute :: Form -> Form
-- Return the basic Property and Negation properties.
distribute (Prop x) = Prop x
distribute (Neg f) = Neg f
-- Traverse into the conjunction conditions.
distribute (Cnj fs) = Cnj $ map distribute fs
-- Traverse into the children and afterwards transform current disjunction.
distribute (Dsj [f]) = distribute f
distribute (Dsj (f:fs)) = disjunction (distribute f) (distribute (Dsj fs))


disjunction :: Form -> Form -> Form
disjunction (Cnj [f1, f2]) (Cnj [g1, g2]) =
    Cnj [Cnj [distribute (Dsj [f1, g1]),distribute (Dsj [f1, g2])], Cnj [distribute (Dsj [f2, g1]),distribute (Dsj [f2, g2])]]  -- Children both are Conjunction (Double Dsitribution).
disjunction f (Cnj (g:gs)) =
    Cnj (map (distribute . createDsj f) (g:gs))   -- Right Child is Conjunction.
disjunction (Cnj (f:fs)) g =
    Cnj (map (distribute . createDsj g) (f:fs))   -- Left Child is Conjunction.    
disjunction f g =
    Dsj [f, g]   -- No conjunctions in children.

-- 3 ∨ (1 ∧ 2)  ==>  (3 ∨ 1) ∧ (3 ∨ 2)
test = Dsj [Prop 3, Cnj [Prop 1, Prop 2]]
-- 1 ∧ (2 ∨ (3 ∧ 4))  ==>  1 ∧ (2 ∨ 3) ∧ (2 ∨ 4)
test2 = Cnj [Prop 1, Dsj [Prop 2, Cnj [ Prop 3, Prop 4]]]
-- (3 ∧ 4) ∨ (1 ∧ 2)  ==>  (3 ∨ 1) ∧ (3 ∨ 2) ∧ (4 ∨ 1) ∧ (4 ∨ 2)
test3 = Dsj [Cnj [Prop 3, Prop 4], Cnj [Prop 1, Prop 2]]
-- 1 ∨ (2 ∧ (3 ∨ (4 ∧ 5)))  ==>  (1 V 2) ∧ (1 V (3 ∨ 4)) ∧ (1 V (3 ∨ 5))
test4 = Dsj [Prop 1, Cnj [Prop 2, Dsj [Prop 3, Cnj [Prop 4, Prop 5]]]]

main3 :: IO ()
main3 = do
    print $ cnf test
    print $ cnf test2
    print $ cnf test3
    print $ cnf test4
