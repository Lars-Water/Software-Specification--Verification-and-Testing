-- This file is property of the group Notorious Fortunate Panda © 2022
-- Time spent on this exercise was: 0.5 hours

module Exercise2 where
import Data.List
import System.Random
import Test.QuickCheck
import Test.QuickCheck.Gen
import Control.Monad
import Control.Exception
import Lecture3
import Exercise4

{-  In this exercise we have used the our logical form generator from Exercise 4 to
    help generate random logical forms, which we could then use to test properties.
    If the pareser works accordingly it should statisfy our defined properties. For
    each property we have defined why it should satisfy.
-}

-- Properties to test
-- 1. Given a formula f, if the formula f was satisfiable before parsing, it should be satisfiable after parsing aswell.
satisfiableP :: Form -> Property
satisfiableP f  =  satisfiable f  ==> satisfiable (head (parse (show f)))

-- 2. Given a formula f, if the formula f is valid, the formula should stay the same after parsing aswell.
equalP :: Form -> Property
equalP f  =  satisfiable f  ==> f == (head (parse (show f)))

-- 3. Given a formula f, if the formula f is invalid, the parser should return an empty list.
--      unfortunately we could'nt quite get this property to work, we have written an example
--      function that should catch an error using the allVals function. However this function is type
--      guarderd, we couldn't get a propper way to catch this error. Otherwise we could've used this to
--      to determine whether the formula is valid or not and then check if the parser returns an empty list.

-- emptyP :: Form -> Property
-- emptyP f  =   empty f == "" ==> null (parse (show f))

-- -- helper to check whether given formula is valid
-- empty :: Form -> IO()
-- empty f = catch (print (allVals f)) handler
--         where
--             handler :: SomeException -> IO()
--             handler ex = ""

main2 :: IO ()
main2 = do
    quickCheck $ forAll formGenerator satisfiableP
    quickCheck $ forAll formGenerator equalP
    -- quickCheck $ forAll formGenerator emptyP
