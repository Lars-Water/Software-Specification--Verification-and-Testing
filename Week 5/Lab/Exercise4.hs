-- This file is property of the group Notorious Fortunate Panda © 2022
-- Time spent on this exercise was: 30 minutes

module Exercise4 where
import Mutation
import Exercise1
import Exercise2
import Exercise3
import MultiplicationTable
import Test.QuickCheck
import Data.List
import Data.Maybe

{-

    The strength of a given set of properties i.e. the percentage of mutants they kill, can be defined as [1 - (n_survivors / n_mutants) * 100%].

    The percentage of killed mutants can be determined from the percentage of survived mutants. The percentage of survivors will be calculated by
    utilizing the 'countSurvivors' function as defined in Exercise2, after which the number of survivors is divided by the number of mutants.

    Consequently, the remaining percentage of mutants would be the number of mutants killed.

    Although simple, this approach appears to cover what is expected.

-}

strength mutator n_mutants props func = do
    n_survivors <- countSurvivors mutator n_mutants props func
    return $ 1 - (fromIntegral n_survivors) / (fromIntegral n_mutants)
