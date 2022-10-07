-- This file is property of the group Notorious Fortunate Panda © 2022
-- Time spent on this exercise was: ?? minutes

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

    The strength of a given set of properties i.e. the percentage of mutants they kill, could be defined as [1 - (n_survivors / n_mutants) * 100%].

    The percentage of mutants to be killed can be determined from the percentage of survivors. The percentage of survivors will be calculated by
    utilization of the 'countSurvivors' function as defined in Exercise2, after which the number is survivors is divided by the number of mutants.

    Consequently, the remaining percentage of mutants is then the number of mutants killed.

    Although simple, this approach appears to cover what is expected

-}

strength mutator n_mutants props func = do
    n_survivors <- countSurvivors mutator n_mutants props func
    return $ 1 - (fromIntegral n_survivors) / (fromIntegral n_mutants)
