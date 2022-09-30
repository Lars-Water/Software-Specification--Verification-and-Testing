module Exercise4 where
import Data.List
import LTS
import Test.QuickCheck

after :: IOLTS -> State -> Trace -> [State]
after iolts state []             = [state]                                          -- Return the current State
after iolts state [label]        = afterStates iolts state label                    -- Return the output State of the single label and input state
after iolts state (label:labels) = foldr (++) [] (map (nextNextStates) nextStates)  -- Return the output States of the multi-label trace
    where nextStates     = afterStates iolts state label                    --  Return the next states from the first label in the trace
          nextNextStates = \afterState -> after iolts afterState labels     --  Recurively call upon 'after' with remaining labels in trace and given list of states

-- Return the output states of the TransitionLabels with the corresponding input state and label.
afterStates :: IOLTS -> State -> Label -> [State]
afterStates (_, _, _, xs, _) curState curLabel =
    map thd (filter (\ (state, label, _) -> label == curLabel && state == curState) xs)

-- Return the third element of a triple tuple.
thd :: LabeledTransition -> State
thd (_, _, a) = a

main = do
    putStrLn $ show "TretmanK1"
    putStrLn $ show $ after tretmanK1 0 ["but"]
    putStrLn $ show $ after tretmanK1 0 ["but", "but"]
    putStrLn $ show $ after tretmanK1 0 ["but", "but", "liq"]
    putStrLn $ show $ after tretmanK1 0 ["but", "but", "liq", "but"]
    putStrLn $ show $ after tretmanK1 0 ["but", "but", "liq", "liq"]
    putStrLn $ show "TretmanK2"
    putStrLn $ show $ after tretmanK2 0 ["but"]
    putStrLn $ show $ after tretmanK2 0 ["but", "but"]
    putStrLn $ show $ after tretmanK2 0 ["but", "but", "liq"]
    putStrLn $ show $ after tretmanK2 0 ["but", "but", "liq", "but"]
    putStrLn $ show $ after tretmanK2 0 ["but", "but", "liq", "liq"]
    putStrLn $ show "TretmanK3"
    putStrLn $ show $ after tretmanK3 0 ["but"]
    putStrLn $ show $ after tretmanK3 0 ["but", "but"]
    putStrLn $ show $ after tretmanK3 0 ["but", "but", "liq"]
    putStrLn $ show $ after tretmanK3 0 ["but", "but", "liq", "but"]
    putStrLn $ show $ after tretmanK3 0 ["but", "but", "liq", "liq"]