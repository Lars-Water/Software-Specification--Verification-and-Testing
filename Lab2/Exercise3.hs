import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

infix 1 -->

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker   xs p q = stronger xs q p

prop1, prop2, prop3 :: Int -> Bool
prop1 = (\ x -> even x && x > 3)
prop2 = (\ x -> even x || x > 3)
prop3 = (\ x -> (even x && x > 3) || even x)