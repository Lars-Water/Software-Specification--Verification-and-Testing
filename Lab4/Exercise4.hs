module Exercise4 where
import Data.List
import LTS
import Test.QuickCheck

-- Time spent: 300 minutes --

{-
    The after function in this module represents the after function corresponding with the definition in the Tretmans paper.

    The function is given an IOLTS model and a trace of labels. The function returns the set of states reached after the given
    trace. The implementation works by calling upon a helper function that takes in an IOLTS model and a trace of lables
    (similar to the arguments in 'after'), and a list of states.

    As a test the models K1-3, S4 and I3
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
    putStrLn $ show $ tretmanK1 `after` ["liq", "liq"]
    putStrLn $ show $ tretmanK1 `after` ["but", "but", "liq", "liq"]
    putStrLn $ show "TretmanK2: Expected to return states"
    putStrLn $ show $ tretmanK2 `after` ["but"]
    putStrLn $ show $ tretmanK2 `after` ["but", "but"]
    putStrLn $ show $ tretmanK2 `after` ["but", "but", "liq"]
    putStrLn $ show $ tretmanK2 `after` ["but", "but", "liq", "but"]
    putStrLn $ show $ tretmanK2 `after` []
    putStrLn $ show "TretmanK2: Expected to NOT return any states"
    putStrLn $ show $ tretmanK2 `after` ["choc"]
    putStrLn $ show $ tretmanK2 `after` ["but", "but", "liq", "liq"]
    putStrLn $ show $ tretmanK2 `after` ["but", "but", "liq", "choc"]
    putStrLn $ show "TretmanK3: Expected to return states"
    putStrLn $ show $ tretmanK3 `after` ["but"]
    putStrLn $ show $ tretmanK3 `after` ["but", "but"]
    putStrLn $ show $ tretmanK3 `after` ["but", "but", "liq"]
    putStrLn $ show $ tretmanK3 `after` []
    putStrLn $ show "TretmanK3: Expected to NOT return any states"
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
