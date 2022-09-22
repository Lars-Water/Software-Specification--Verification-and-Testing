import Data.List
import System.Random
import Test.QuickCheck
import Lecture3
import Control.Monad

instance Arbitrary Form where
    arbitrary = gform

gform = sized gform'

gform' 0 = liftM Prop (choose (1,15))
gform' n | n > 15 = gform' (n `div` 2)
gform' n | n > 0 =
           oneof [liftM Prop (choose (1,15)),
                  liftM Neg subform,
                  liftM Cnj subforms,
                  liftM Dsj subforms,
                  liftM2 Impl subform subform,
                  liftM2 Equiv subform subform]
           where subform = gform' (n `div` 2)
                 subforms = resize (n `div` 2) (vector (n `div` 2))

main :: IO()
main = do
    print "Hello"