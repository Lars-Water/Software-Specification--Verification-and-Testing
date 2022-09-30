-- This file is property of the group Notorious Fortunate Panda © 2022
-- Time spent: 45 minutes --
module Exercise1 where
import Data.List
import LTS

-- helper functions to merge two lists
merge [] ys = ys
merge (x:xs) ys = x:merge ys xs

-- helper function to see if two lists are disjoint
compareList :: (Eq a) => [a] -> [a] -> Bool
compareList a = not . null . intersect a

-- helper function to get all labels out of a list of transitions
getLabels :: [LabeledTransition] -> [Label]
getLabels transitions = makeSet (map (\(_,labels,_) -> labels) transitions)

-- helper function to get all states out of a list of transitions
getStates :: [LabeledTransition] -> [State]
getStates transitions = makeSet (concatMap (\(s1,_,s2) -> [s1,s2]) transitions )

-- – Q is a countable, non-empty set of states;
-- – L is a countable set of labels;
-- – T ⊆ Q × (L ∪ { τ }) × Q, with τ / ∈ L, is the transition relation;
-- – q0 ∈ Q is the initial state.
-- The following properties are set up based on Tretmans definition
emptyStates :: IOLTS
emptyStates = ([],["coin"], ["tea"], [(2,"tea",3)], 2)

overlappingInAndOut :: IOLTS
overlappingInAndOut = ([2,3],["tea"], ["tea"],[(2,"tea",3)], 2)

initialStateNotInStates :: IOLTS
initialStateNotInStates = ([2,3],["coin"], ["tea"],[(2,"tea",3)], 1)

undefinedTranstitionLabel :: IOLTS
undefinedTranstitionLabel = ([2,3],["coin"], ["coin"],[(2,"tea",3)], 1)

undefinedStates :: IOLTS
undefinedStates= ([2,3],["coin"], ["coin"],[(2,"tea",4)], 1)

validLTS :: IOLTS
validLTS = ([1,2,3],["coin"], ["tea"],[(1,"coin",2),(2,"tea",3)], 1)

-- states, inputs, outputs, traces, initial state
-- Validity accoring to Tretmans definition
validateLTS :: IOLTS -> Bool
validateLTS (states,inputs,outputs,transitions,initialstate)    | null states = False    -- Q is a countable, non-empty set of states
                                                                | compareList inputs outputs = False-- inputs and outputs should be disjoint
                                                                | initialstate `notElem` states = False -- initial state should be in states
                                                                | intersect (getLabels transitions) (merge inputs outputs) /= getLabels transitions = False -- labels should be in inputs or outputs
                                                                | intersect (getStates transitions) states /= getStates transitions = False -- states should be in states
                                                                | otherwise = True
main1 :: IO()
main1 = do
    print("Here we will test the following properties given an LTS the following test should thus be False")
    print ("Given an LTS with transitions, the states should not be empty: " ++ show  (validateLTS emptyStates))
    print ("Input and Output labels of an IOLTS should be unique: " ++ show (validateLTS overlappingInAndOut))
    print ("Given an LTS the initial state should be defined in the set of states: " ++ show (validateLTS initialStateNotInStates))
    print ("A transition label should always be defined in either the inputs or outputs: " ++ show (validateLTS undefinedTranstitionLabel))
    print ("States to which there is a transition should be defined in the states set: " ++ show (validateLTS undefinedStates))
    print ("validLTS: " ++ show (validateLTS validLTS))
