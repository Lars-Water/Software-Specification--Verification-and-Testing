module Exercise5 where
import Data.List
import LTS
import Test.QuickCheck
import Exercise4
import Exercise3
import SetOrd

-- Time spent: 300 minutes --

-- Determine ioco between model i and model m.
ioco :: IOLTS -> IOLTS -> Bool
i@(_, _, _, _, initial_i) `ioco` m@(_, _, _, _, initial_m) = all (==True) subsets
    -- For every strace in straces determine if the outputlabels from model i are a subset of the outputlabels from modl m
    where subsets = map (\strace ->
            if length (out i initial_i strace) == 0
                then length (out m initial_m strace) == 0
            else (list2set (out i initial_i strace)) `subSet` (list2set (out m initial_m strace))) (straces m)

out :: IOLTS -> State -> Trace -> [Label]
out iolts state trace = foldr (++) [] (map output afterTrace)
    where afterTrace  = (iolts `after` trace)                            --  Determine the states after the requested trace from the given state in the IOLTS model
          output      = (\afterState -> outputLabels iolts afterState)   --  Determine the outputlabels of the requested state in the IOLTS model

outputLabels :: IOLTS -> State -> [Label]
outputLabels (_, _, modelOutputLabels, labeledTransitions, _) curState =
    map second (filter filterOutputLabels labeledTransitions)       --  Return map of output labels from requested state
        where second             = \(_, label, _) -> label          --  Return second element of a triple tuple
              filterOutputLabels = \(state, label, _) ->            --  Return TransitionLabels that contain outputlabels and requested state
                label `elem` modelOutputLabels && state == curState


-- To test our function, we used the given implementation and specification LTS'es in LTS.hs which were originally from the paper. The paper also included
-- a figure with test results of performing ioco on all combinations of the LTS'es (p.21). With these we can compare the results of our function with those
-- of the paper and see if they match. Unfortunately, our function fails in two scenarios, (I4,S1) and (I4,S2). Both returning true in our case, while they
-- should be false. We think the problem arises because we do not handle deltas which exist implicitly in non-output states correctly. Looking at the 2 failed
-- implementations and specifications directly and solving it by hand, the ioco function should indeed return true if we wouldn't account for the deltas existing.
-- Nevertheless, we were not able to implement this functionality before the deadline.

testmain = do
    putStrLn $ show "i1 ioco s(1-4)"
    putStrLn $ show $ (tretmanI1 `ioco` tretmanS1)
    putStrLn $ show $ (tretmanI1 `ioco` tretmanS2)
    putStrLn $ show $ (tretmanI1 `ioco` tretmanS3)
    putStrLn $ show $ (tretmanI1 `ioco` tretmanS4)
    putStrLn $ show "i2 ioco s(1-4)"
    putStrLn $ show $ (tretmanI2 `ioco` tretmanS1)
    putStrLn $ show $ (tretmanI2 `ioco` tretmanS2)
    putStrLn $ show $ (tretmanI2 `ioco` tretmanS3)
    putStrLn $ show $ (tretmanI2 `ioco` tretmanS4)
    putStrLn $ show "i3 ioco s(1-4)"
    putStrLn $ show $ (tretmanI3 `ioco` tretmanS1)
    putStrLn $ show $ (tretmanI3 `ioco` tretmanS2)
    putStrLn $ show $ (tretmanI3 `ioco` tretmanS3)
    putStrLn $ show $ (tretmanI3 `ioco` tretmanS4)
    putStrLn $ show "i4 ioco s(1-4)"
    putStrLn $ show $ (tretmanI4 `ioco` tretmanS1)
    putStrLn $ show $ (tretmanI4 `ioco` tretmanS2)
    putStrLn $ show $ (tretmanI4 `ioco` tretmanS3)
    putStrLn $ show $ (tretmanI4 `ioco` tretmanS4)
