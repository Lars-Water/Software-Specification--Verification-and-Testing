-- This file is property of the group Notorious Fortunate Panda © 2022
module Exercise4 where
import Data.List
import LTS
import Test.QuickCheck

-- Time spent: 300 minutes --

{-
    The after function in this module represents the after function corresponding with the definition in the Tretmans paper.

    The function is given an IOLTS model and a trace of labels. The function returns the set of states reached after the given
    trace. The implementation works by calling upon a helper function that takes in an IOLTS model and a trace of lables
    (similar to the arguments in 'after'), and a list of states resembling the current states positioned in.
    The helper function returns the list of states reached from this list of current states with the first label in the trace.
    The helper function is called upon recursively with the remainder of labels as the new trace parameters and the returned
    reached list of states as the new parameter for the current states.

    Additionally, the afterStates function includes that for every returned list of states a concatenation is performed with
    a list of states that can be reached from the states of this returned list with a tau transition. The defined helper
    function "tauTransitions' is used to create this list states reachable by tau transition.
    Moreover, empty lists will be returned by 'after' if any label is present in the trace that is not present in the IOLTS model,
    assuming the trace is an invalid trace.

    As a test the models K1-3, S4 and I3 were implemented as IOLTS models and traces were given that were either expected
    to return a list of states if the trace reached any states in the IOLTS model, or an empty list if the trace was invalid or
    did not reach any states in the IOLTS model. Furthermore, the implementation of I3 is expected to return states 1 and 3
    for trace ["a"] due to the presence of a tau transition.

    All traces expected to result in one ore more reached states returned these. Similarly, the traces expected to reach no
    states due to an invalid trace corresponding to the IOLTS model returned empty lists. The tau transition was handled
    correctly by the after function. These results imply a correct implementation of the after function as described in the
    Tretmans paper. Nevertheless, the testing performed was scarce due to the highly specific test cases. Therefore, the
    assumption of correct implementation is can not be seen as strong assumption.
-}

after :: IOLTS -> Trace -> [State]
iolts@(_,_,_,_,state) `after` []      = [state] ++ (tauTransitions iolts [state])   -- Return the current State + tau transitions
iolts@(_,_,_,_,state) `after` labels  = afterStates iolts [state] labels            -- Return the output States of the multi-label trace

-- Return the output states of the TransitionLabels with the corresponding input state and label.
afterStates :: IOLTS -> [State] -> Trace -> [State]
afterStates iolts@(_, _, _, labeledTransitions, _) states [curLabel] = outputStates ++ (tauTransitions iolts outputStates)
    where outputStates = map thd outputTransitions
            where outputTransitions = (filter (\ (state, label, _) -> label == curLabel && state `elem` states) labeledTransitions)
afterStates iolts@(_, _, _, labeledTransitions, _) states (curLabel: nextLabels) = afterStates iolts outputsAndTaus nextLabels
    where outputsAndTaus = outputStates ++ (tauTransitions iolts outputStates)
            where outputStates = map thd outputTransitions
                    where outputTransitions = (filter (\ (state, label, _) -> label == curLabel && state `elem` states) labeledTransitions)

-- Return states reachable with tau from requested states.
tauTransitions :: IOLTS -> [State] -> [State]
tauTransitions (_, _, _, labeledTransitions, _) afters = map (\(_, _, end) -> end) (filter correspondingTaus labeledTransitions)
    where correspondingTaus = (\(start, label, _) -> label == tau && start `elem` afters)

-- Return the third element of a triple tuple.
thd :: LabeledTransition -> State
thd (_, _, a) = a

main = do
    putStrLn $ show "TretmanK1: Expected to return states"
    putStrLn $ show $ tretmanK1 `after` ["but"]
    putStrLn $ show $ tretmanK1 `after` ["but", "but"]
    putStrLn $ show $ tretmanK1 `after` ["but", "but", "liq"]
    putStrLn $ show $ tretmanK1 `after` ["but", "but", "liq", "but"]
    putStrLn $ show $ tretmanK1 `after` []
    putStrLn $ show "TretmanK1: Expected to NOT return any states"
    putStrLn $ show $ tretmanK1 `after` ["but", "abc", "but", "liq"]
    putStrLn $ show $ tretmanK1 `after` ["liq", "liq"]
    putStrLn $ show $ tretmanK1 `after` ["but", "but", "liq", "liq"]
    putStrLn $ show "TretmanK2: Expected to return states"
    putStrLn $ show $ tretmanK2 `after` ["but"]
    putStrLn $ show $ tretmanK2 `after` ["but", "but"]
    putStrLn $ show $ tretmanK2 `after` ["but", "but", "liq"]
    putStrLn $ show $ tretmanK2 `after` ["but", "but", "liq", "but"]
    putStrLn $ show $ tretmanK2 `after` []
    putStrLn $ show "TretmanK2: Expected to NOT return any states"
    putStrLn $ show $ tretmanK2 `after` ["but", "abc", "but", "liq"]
    putStrLn $ show $ tretmanK2 `after` ["choc"]
    putStrLn $ show $ tretmanK2 `after` ["but", "but", "liq", "liq"]
    putStrLn $ show $ tretmanK2 `after` ["but", "but", "liq", "choc"]
    putStrLn $ show "TretmanK3: Expected to return states"
    putStrLn $ show $ tretmanK3 `after` ["but"]
    putStrLn $ show $ tretmanK3 `after` ["but", "but"]
    putStrLn $ show $ tretmanK3 `after` ["but", "but", "liq"]
    putStrLn $ show $ tretmanK3 `after` []
    putStrLn $ show "TretmanK3: Expected to NOT return any states"
    putStrLn $ show $ tretmanK3 `after` ["but", "abc", "but", "liq"]
    putStrLn $ show $ tretmanK2 `after` ["choc"]
    putStrLn $ show $ tretmanK3 `after` ["but", "but", "liq", "but"]
    putStrLn $ show $ tretmanK3 `after` ["but", "but", "liq", "liq"]
    putStrLn $ show "TretmanS4: Expected to return states INCLUDING tau transitions"
    putStrLn $ show $ tretmanS4 `after` ["a"]
    putStrLn $ show $ tretmanS4 `after` ["a","x"]
    putStrLn $ show $ tretmanS4 `after` []
    putStrLn $ show "TretmanI3: Expected to return states"
    putStrLn $ show $ tretmanI3 `after` ["a", "a", "x"]
    putStrLn $ show $ tretmanI3 `after` ["a", "a", "x", "b"]
    putStrLn $ show $ tretmanI3 `after` ["b", "b", "y", "b"]
