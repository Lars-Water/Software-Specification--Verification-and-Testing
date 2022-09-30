
module Exercise5 where
import Data.List
import LTS
import Test.QuickCheck
import Exercise4
import Exercise3
import SetOrd

-- Determine ioco between model i and model m.
ioco :: IOLTS -> IOLTS -> Bool
i@(_, _, _, _, initial_i) `ioco` m@(_, _, _, _, initial_m) = all (==True) subsets
    -- For every strace in straces determine if the outputlabels from modl i are a subset of the outputlabels from model m
    -- NOTE: straces is taken from Exercise3.hs, but is now a static list of traces because straces is not implemented yet!!!
    where subsets = map (\strace -> (list2set (out i initial_i strace)) `subSet` (list2set (out m initial_m strace))) straces

out :: IOLTS -> State -> Trace -> [Label]
out iolts state trace = foldr (++) [] (map output afterTrace)
    where afterTrace  = (after iolts state trace)                   --  Determine the states after the requested trace from the given state in the IOLTS model
          output = (\afterState -> outputLabels iolts afterState)   --  Determine the outputlabels of the requested state in the IOLTS model

outputLabels :: IOLTS -> State -> [Label]
outputLabels (_, _, modelOutputLabels, labeledTransitions, _) curState =
    map second (filter filterOutputLabels labeledTransitions)  --  Return map of output labels from requested state
        where second             = \(_, label, _) -> label     --  Return second element of a triple tuple
              filterOutputLabels = \(state, label, _) ->       --  Return TransitionLabels that contain outputlabels and requested state
                label `elem` modelOutputLabels && state == curState

testmain = do
    putStrLn $ show $ out tretmanK3 0 ["but", "but"]
    putStrLn $ show $ tretmanK1 `ioco` tretmanK3
    putStrLn $ show $ tretmanK3 `ioco` tretmanK1