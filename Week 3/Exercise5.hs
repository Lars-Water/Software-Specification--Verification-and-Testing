-- This file is property of the group Notorious Fortunate Panda © 2022
-- Time spent on this exercise was: 5 hours

{-
    5.1)
    One property that was attempted to be tested was to check whether the given formula is an element in the
    created set of subformulae. Nevertheless, the implemented quickCheck test did not pass returning the error:
    'Non-exhaustive patterns in function sub'.

    5.2)
    nsub takes any formula f of type Form and returns the exact number of possible sub-formalae.

    nsub calls upon the helper function 'size' that takes in the set of sub-formulae from a formula
    f of type Form. In this case, nsub uses 'sub' to create this set. The function 'size' increments
    all elements in the input set recursively, returning the number of elements in the set. The returned
    value to nsub would then be the size of the set, in this case the set of subformulae of f.

    According to a consulted source:
        https://math.stackexchange.com/questions/3602488/a-proposition-with-n-connectives-has-at-most-2n-1-subformulas
    The maximum number of subformulae for a proposition with n connectives is 2n + 1. This can be proven by induction
    as was shown in the consulted source.

    Assuming this proof by induction, a property to test for the function of nsub would therefore be that the
    returned number should not exceed 2n + 1 w/ n being the number of connectives for any randomly generated formula.
    Sadly, we were not able to implement this test on time and are therefore limited to the consulted proof. An obstacle
    we could not overcome was to implement a function that determined the connectives in a given formula. Nevertheless,
    this still would not provide us with the confidence that the exact number is generated.
-}

module Exercise5
where
import Data.List
import System.Random
import Test.QuickCheck
import Test.QuickCheck.Gen
import Lecture3
import Exercise4
import SetOrd

sub :: Form -> Set Form
sub (Prop x) = Set [Prop x]
sub (Neg f) = unionSet (Set [Neg f]) (sub f)
sub f@(Cnj [f1,f2]) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)
sub f@(Dsj [f1,f2]) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)
sub f@(Impl f1 f2) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)
sub f@(Equiv f1 f2) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)

nsub :: Form -> Int
nsub f = size $ sub f

size :: Set Form -> Int
size (Set []) = 0
size (Set (_:xs)) = 1 + size (Set xs)

formInSub :: Form -> Bool
formInSub f = inSet f $ sub f

main5 :: IO ()
main5 = do
    quickCheck $ forAll formGenerator formInSub
