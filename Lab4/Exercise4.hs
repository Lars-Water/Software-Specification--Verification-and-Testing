module Exercise4 where
import Data.List
import LTS
import Test.QuickCheck

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
    putStrLn $ show "TretmanK1"
    putStrLn $ show $ tretmanK1 `after` ["but"]
    putStrLn $ show $ tretmanK1 `after` ["but", "but"]
    putStrLn $ show $ tretmanK1 `after` ["but", "but", "liq"]
    putStrLn $ show $ tretmanK1 `after` ["but", "but", "liq", "but"]
    putStrLn $ show $ tretmanK1 `after` ["but", "but", "liq", "liq"]
    putStrLn $ show "TretmanK2"
    putStrLn $ show $ tretmanK2 `after` ["but"]
    putStrLn $ show $ tretmanK2 `after` ["but", "but"]
    putStrLn $ show $ tretmanK2 `after` ["but", "but", "liq"]
    putStrLn $ show $ tretmanK2 `after` ["but", "but", "liq", "but"]
    putStrLn $ show $ tretmanK2 `after` ["but", "but", "liq", "liq"]
    putStrLn $ show "TretmanK3"
    putStrLn $ show $ tretmanK3 `after` ["but"]
    putStrLn $ show $ tretmanK3 `after` ["but", "but"]
    putStrLn $ show $ tretmanK3 `after` ["but", "but", "liq"]
    putStrLn $ show $ tretmanK3 `after` ["but", "but", "liq", "but"]
    putStrLn $ show $ tretmanK3 `after` ["but", "but", "liq", "liq"]
    putStrLn $ show "TretmanS4"
    putStrLn $ show $ tretmanS4 `after` []
    putStrLn $ show $ tretmanS4 `after` ["a"]
    putStrLn $ show $ tretmanS4 `after` ["a","x"]
