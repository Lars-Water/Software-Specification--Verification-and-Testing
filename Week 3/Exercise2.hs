

-- Time spent on this exercise was:


module Exercise2 ()where
import Data.List
import System.Random
import Test.QuickCheck
import Test.QuickCheck.Gen
import Control.Monad
import Lecture3
import Exercise1

instance Arbitrary Form where
    arbitrary = formGenerator
-- Source: https://stackoverflow.com/questions/63972160/quickcheck-propositional-logic-generator
-- Why did we choose this generator and our understanding of how it works:
-- 
-- Generator for Form
formGenerator = sized formGenerator' -- we size the formGenerator' function with the sized function which quickcheck can use to choose a size for the form
formGenerator' 0 = liftM Prop (choose (1,5)) -- case 0 we pick a random number between 1 and 15 to use as prop and lift this to monad
formGenerator' n | n > 15 = formGenerator' (n `div` 2)
formGenerator' n | n > 0 =                  -- in the other cases we will pick a random propositinal element, here we again allow for atoms
           oneof [liftM Prop (choose (1,15)), -- here we pick a random propositional element and lift it to the monad form to construct our form.
                  liftM Neg subform,
                  liftM Cnj subforms,
                  liftM Dsj subforms,
                  liftM2 Impl subform subform,
                  liftM2 Equiv subform subform]
           where subform = formGenerator' (n `div` 2)
                 subforms = resize (n `div` 2)  (vector (1 + (n `div` 2))) -- subforms are a list of subform, which we resize and the second subform we vectorize for efficiency

-- Properties to test
satisfiableP :: Form -> Property
satisfiableP f  =  satisfiable f  ==> satisfiable (head (parse (show f)))

tautologyP :: Form -> Property
tautologyP f = tautology f ==> tautology (head (parse (show f)))

contradictionP :: Form -> Property
contradictionP f = contradiction f ==> contradiction (head (parse (show f)))



main :: IO()
main = do
    quickCheck $ forAll formGenerator satisfiableP
    quickCheck $ forAll formGenerator tautologyP
    quickCheck $ forAll formGenerator contradictionP

