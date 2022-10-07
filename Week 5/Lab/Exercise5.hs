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

    Conjectures equivalence:
        The approach to calculating equivalence conjectures starts with defining the powerset of the list of properties. Subsequently,
        mutation tests are performed on every subset in this powerset. Specification of the mutants that were killed should be returned
        from these mutation tests. Consequently, equivalence conjectures can be determined through comparison of the killed mutants where
        an equal return value implies equivalence.
    Conjecture implication:
        ...

-}

