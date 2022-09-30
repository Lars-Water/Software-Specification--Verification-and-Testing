import Data.List
import LTS
import Test.QuickCheck
import Exercise4

out :: IOLTS -> State -> [State]
out (_, _, outputLabels, labeledTransitions, _) curState =
    map thd (filter (\ (state, label, _) -> label `elem` outputLabels && state == curState) labeledTransitions)

-- ioco ::
