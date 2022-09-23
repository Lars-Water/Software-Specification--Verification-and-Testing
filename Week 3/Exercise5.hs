-- This file is property of the group Notorious Fortunate Panda © 2022
-- Time spent on this exercise was: ??? hours

module Exercise5
where
import Data.List
import System.Random
import Test.QuickCheck
import Test.QuickCheck.Gen
import Lecture3
import SetOrd

sub :: Form -> Set Form
sub (Prop x) = Set [Prop x]
sub (Neg f) = unionSet (Set [Neg f]) (sub f)
sub f@(Cnj [f1,f2]) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)
sub f@(Dsj [f1,f2]) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)
sub f@(Impl f1 f2) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)
sub f@(Equiv f1 f2) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)


-- This propery checks if the sub function returns a set of all the subforms of a given form
-- where no new props occur in the subforms
-- noNewProps :: Form -> Property
-- noNewProps f = propNames f == mapM (\x -> propNames x) (sub f)