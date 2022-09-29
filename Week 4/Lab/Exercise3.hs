-- This file is property of the group Notorious Fortunate Panda © 2022
-- Time spent on this exercise was: 45 minutes 

module Exercise2 ()where
import Data.List
import Test.QuickCheck
import Test.QuickCheck.Gen
import LTS

straces :: IOLTS -> [Trace] 
straces (_,_,_,traces,start) = 

labelExtractor :: [LabeledTransition] -> [Int] -> Label  [Label]
labelExtractor [] _ = []
labelExtractor (x:xs) state oldLabel = map (\(_,label,_) -> oldLabel ++ label) (labelTransitionSplitter (x:xs) (extractState x))  : labelExtractor xs states (oldLabel ++ label)

extractState :: LabeledTransition -> Int
extractState (state,_,_) = state

-- Split a list of labeltransitions into a sublist where the first state is the given state
-- e.g. ([(S0, a ,S1), (S0, a ,S2), (S1, x ,S3), (S2, y ,S4)] -> S0 -> [(S0, a ,S1), (S0, a ,S2)]
labelTransitionSplitter :: [LabeledTransition] -> Int -> [LabeledTransition]
labelTransitionSplitter [] _ = []
labelTransitionSplitter (x:xs) state = filter (\(from,_,_) -> from == state ) (x:xs)