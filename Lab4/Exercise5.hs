import Data.List
import LTS
import Test.QuickCheck

out :: IOLTS -> State -> [Label]
out (_, _, outputLabels, labeledTransitions, _) curState =
    map second (filter filterOutputLabels labeledTransitions)  --  Return map of output labels from requested state
        where second             = \(_, label, _) -> label     --  Return second element of a triple tuple
              filterOutputLabels = \(state, label, _) ->       --  Return TransitionLabels that contain outputlabels and requested state
                label `elem` outputLabels && state == curState

-- ioco ::
