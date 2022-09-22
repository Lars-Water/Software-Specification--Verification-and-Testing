

-- Time spent on this exercise was: 45 minutes 
-- Do i need quickcheck here?

module Exercise3 ()where
import Data.List
import System.Random
import Test.QuickCheck
import Lecture3


cnf :: Form -> Form
cnf f = nnf . arrowfree f

dl :: Form -> Form
dl Cnj (f:fs) = Cnj map f fs
dl (Cnj [Dsj[]:fs]) =  Cnj map (\f -> Cnj f ) fs

main :: IO()
main = print("test")