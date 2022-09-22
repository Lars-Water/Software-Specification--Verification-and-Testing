import Data.List
import System.Random
import Test.QuickCheck
import Lecture3

cnf :: Form -> Form
cnf = nnf . arrowfree

distribute :: Form -> Form
-- Return the basic Property and Negation properties (transformed w/ nnf).
distribute (Prop x) = Prop x
distribute (Neg f) = (Neg f)
-- Traverse into the conjunction conditions.
distribute (Cnj fs) = Cnj $ map distribute fs
-- Transform disjunction conditions.
distribute (Dsj [Cnj [fs1, fs2], Prop x]) = Cnj [Dsj [fs1, Prop x], Dsj [fs2, Prop x]]
distribute (Dsj [Prop x, Cnj [fs1, fs2]]) = Cnj [Dsj [Prop x, fs1], Dsj [Prop x, fs2]]


test = Dsj [Prop 3, Cnj [Prop 1, Prop 2]]
test2 = Cnj [Prop 1, Dsj [Prop 2, Cnj [ Prop 3, Prop 4]]]
