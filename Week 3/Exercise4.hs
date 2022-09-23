-- This file is property of the group Notorious Fortunate Panda © 2022
-- Time spent on this exercise was: 3.5 hours

module Exercise4 where
import Data.List
import System.Random
import Test.QuickCheck
import Test.QuickCheck.Gen
import Control.Monad
import Lecture3
import Exercise1
import Exercise3

instance Arbitrary Form where
    arbitrary = formGenerator
{-  Generator for Form
    Why did we choose this generator and our understanding of how it works:
    We went with this generator because it provided a clear overview of how the generator
    distinguishes between props and operands (neg, conj, disj, impl, equiv). It also provides
    great compatibility with the quickCheck since quickCheck can simply pick any Integer upon which
    the generator will create a form based on that. This way we can efficiently check for a lot of forms
    given a certain property at.
    Source: https://stackoverflow.com/questions/63972160/quickcheck-propositional-logic-generator
-}

{-  This helper is being called in our generator
    in order to optimize generation speed when quickCheck
    is asked to choose an n value.
-}
div2 :: Int -> Int
div2 n = n `div` 2

formGenerator = sized formGenerator'         -- we size the formGenerator' function with the sized function which quickcheck can use to choose a size for the form
formGenerator' 0 = liftM Prop (choose (1,3)) -- case 0 we pick a random number between 1 and 3 to use as prop and lift this to monad (we choose 1 to 3 to keep overview)
formGenerator' n | n > 15 = formGenerator' (div2 n) -- to prevent the chosen n to be too big we call the div2 function to halve it for efficiency
formGenerator' n | n > 0 =                   -- in the other cases we will pick a random propositinal element, here we again allow for atoms
    oneof [ liftM Prop (choose (1,5)),       -- here we pick a random propositional element and lift it to the monad form to construct our form.
            liftM Neg subform,
            liftM Cnj subforms,
            liftM Dsj subforms,
            liftM2 Impl subform subform,     -- since the implies and equivalent takes two subforms we use liftM2 to lift both arguments to the monad form
            liftM2 Equiv subform subform]
        where subform = formGenerator' (div2 n)
              subforms = resize (div2 n)  (vector (1 + (n `div` 2))) -- subforms are a list of subform, which we resize and the second subform we vectorize for efficiency

-- Properties to test
-- 1. Given a formula f, the cnf of that formula should be logoically equivalent to the original formula.
equalCNF :: Form -> Property
equalCNF f = equiv f (cnf f) === True

-- 2. Given a formula f, the outcome of cnf f should be an actual cnf.
isCNF :: Form -> Property
isCNF f = isCNF' (cnf f) === True

-- This helper function checks if a given form is a cnf based on exhaustive pattern matching.
-- There could be edge cases that are missing in this function, thus unfortunately we cannot
-- guarantee the full-proofness of this function.
isCNF' :: Form -> Bool
isCNF' (Prop x) = True
isCNF' (Neg (Prop x)) = True
isCNF' (Neg (Neg x)) = False
isCNF' (Dsj []) = True
isCNF' (Neg (Dsj x)) = False
isCNF' (Neg (Cnj x)) = False
isCNF' (Neg (Equiv x y)) = False
isCNF' (Neg (Impl x y)) = False
isCNF' (Dsj ((Cnj x):xs)) = False
isCNF' (Cnj xs) = all isCNF' xs
isCNF' (Dsj xs) = all isCNF' xs
isCNF' (Impl x y) = False
isCNF' (Equiv x y) = False

{-  In the main function we run quickCheck on our properties.
    Above we have defined our properties that a cnf function should satisfy
    should it work. Our formGenerator provides a great way of testing these
    properties for a lot of different forms. Unfortunately our CNF function
    is not yet full-proof. Given a full-proof CNF function we would expect that
    the following tests would pass generated forms, since each CNF should be
    logical equivalent to it's non CNF formula and it should follow the rules
    to be a CNF.
-}
main4 :: IO ()
main4 = do
    quickCheck $ forAll formGenerator equalCNF
    quickCheck $ forAll formGenerator isCNF


