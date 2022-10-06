module Oompaloompa where

import Mutation
import MultiplicationTable
import Test.QuickCheck

main :: Gen (Maybe Bool)
main = do
    mutant <- mutate removeElements prop_tenElements multiplicationTable 1
    return mutant