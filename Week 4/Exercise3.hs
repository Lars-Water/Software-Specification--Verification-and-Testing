-- This file is property of the group Notorious Fortunate Panda © 2022
-- Time spent on this exercise was: 6.5 hours
module Exercise3 where
import LTS
import Test.QuickCheck
import Exercise2
import Data.List

-- This helper function is used to filter transitions based on the origin State of the transition
filterTransitionsByStartState :: [LabeledTransition] -> State -> [LabeledTransition]
filterTransitionsByStartState transitions originState = filter (\(from,_,_) -> from == originState) transitions

straces :: IOLTS -> [Trace]
straces (states,_,_,transitions,start) = do
    nub (bfs transitions [(start,[])])

-- This is a Breadth-first Search function, used to recursively traverse through an IOLTS. It gets a list of
-- all transitions of the model and a queue of States and their corresponding Traces as parameters and returns
-- a list of all Traces in the model. It starts off by retrieving all of the States that are directly linked
-- to the first State in the queue. After that, the function returns the Trace of the first State in the queue,
-- appended to the return value of the bfs function which gets recursively called, wherein the original queue's
-- first element gets removed and the retrieved States earlier on in the function get appended to the queue,
-- along with their Traces
bfs :: [LabeledTransition] -> [(State,Trace)] -> [Trace]
bfs transitions queue | null queue = []
                      | otherwise = do
    let curState = fst (head queue)
    let newStates = filterTransitionsByStartState transitions curState
    let curStateTrace = snd (head queue)
    curStateTrace : bfs transitions (tail queue ++ stateIdentifier (map (createNewQueueAdditions curStateTrace) newStates) curState)

stateIdentifier :: [(State,Trace)] -> State -> [(State,Trace)]
stateIdentifier queue curState | null queue = queue
                               | curState == fst (head queue) = tail queue ++ [head queue]
                               | otherwise = queue

-- This helper function returns a tuple value of a State and the Trace path that is needed to reach the State.
createNewQueueAdditions :: Trace -> LabeledTransition -> (State,Trace)
createNewQueueAdditions curStateTrace (from,label,to) =
    (to,curStateTrace ++ [label])

stracesCheck :: IOLTS -> Bool
stracesCheck (s,inputs,outputs,t,i) = all (==True) (map (\x -> x `elem` inputs || x `elem` outputs) (tail (concat (straces ((s,inputs,outputs,t,i)) ))))

main3 :: IO ()
main3 = do
    putStrLn "Exercise 3"
    putStrLn "For this test we have implemented our IOLTS generator of Exercise2"
    putStrLn "We check the straces by confirming that all the labels in the traces are either inputs or outputs of the IOLTS"
    putStrLn "However do keep in mind that straces are potentially infinite, so this test unfortunately does get stuck"
    putStrLn "However in order to use quickCheck this seemed the only sensible way to test this function"
    quickCheck $ forAll ltsGen stracesCheck
