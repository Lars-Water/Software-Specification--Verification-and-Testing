import Data.List
import System.Random
import Test.QuickCheck
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

distribute :: Form -> Form
-- Return the basic Property and Negation properties.
distribute (Prop x) = Prop x
distribute (Neg f) = (Neg f)
-- Traverse into the conjunction conditions.
distribute (Cnj fs) = Cnj $ map distribute fs
-- Traverse into the children and afterwards transform current disjunction.
distribute (Dsj [f1, f2]) = disjunction (distribute f1) (distribute f2)

disjunction :: Form -> Form -> Form
disjunction (Cnj [f1, f2]) (Cnj [g1, g2]) =
    Cnj [Cnj [ Dsj [f1, g1], Dsj [f1, g2]], Cnj [ Dsj [f2, g1], Dsj [f2, g2]]]  -- Children both are Conjunction (Double Dsitribution).
disjunction f (Cnj [g1, g2]) =
    Cnj [Dsj [f, g1], Dsj [f, g2]]   -- Right Child is Conjunction.
disjunction (Cnj [f1, f2]) g =
    Cnj [Dsj [f1, g], Dsj [f2, g]]   -- Left Child is Conjunction.
disjunction f g =
    Dsj [f, g]   -- No conjunctions in children.


-- 3 ∨ (1 ∧ 2)  ==>  (3 ∨ 1) ∧ (3 ∨ 2)
test  = Dsj [Prop 3, Cnj [Prop 1, Prop 2]]
-- 1 ∧ (2 ∨ (3 ∧ 4))  ==>  1 ∧ (2 ∨ 3) ∧ (2 ∨ 4)
test2 = Cnj [Prop 1, Dsj [Prop 2, Cnj [ Prop 3, Prop 4]]]
-- (3 ∧ 4) ∨ (1 ∧ 2)  ==>  (3 ∨ 1) ∧ (3 ∨ 2) ∧ (4 ∨ 1) ∧ (4 ∨ 2)
test3 = Dsj [Cnj [Prop 3, Prop 4], Cnj [Prop 1, Prop 2]]

-- 1 ∨ (2 ∧ (3 ∨ (4 ∧ 5)))  ==> No idea what the result should be for this.
test4 = Dsj [Prop 1, Cnj [Prop 2, Dsj [Prop 3, Cnj [Prop 4, Prop 5]]]]
