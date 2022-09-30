-- This file is property of the group Notorious Fortunate Panda © 2022
-- Time spent: 60 minutes --

module Exercise6 where
import Data.List
import LTS
import Test.QuickCheck

testLTSAgainstSUT :: IOLTS -> (State -> Label -> (State, Label)) -> Bool
testLTSAgainstSUT (_,_,_,transitions,_) sutTester = all (==True) [a, b]
    where a = all (\(from,label,to) -> fst (sutTester from label) == to && snd (sutTester from label) /= label ) transitions
          b = all (\(from,label,_) -> stateNameChecker from label sutTester) transitions

-- This helper checks if the stateName is different from the labelName
stateNameChecker :: State -> Label -> (State -> Label -> (State, Label))  -> Bool
stateNameChecker state label sutTester = snd (sutTester state label) /= label

doorIOLTS :: IOLTS
doorIOLTS = createIOLTS [(0,"close",1),(1,"open",0),(1,"lock",2),(2,"unlock",1)]

main6 :: IO ()
main6 = do
    putStrLn "Exercise 6"
    print (testLTSAgainstSUT doorIOLTS doorImpl1)
    print (testLTSAgainstSUT doorIOLTS doorImpl2)
    print("note that implementation 2 passes the test, but is not correct.")
    print("This is because our implementation does not check for the content of the state name.")
    print("We chose not to implement this could this would mean that we would manually have to check for the content of the state name.")
    print (testLTSAgainstSUT doorIOLTS doorImpl3)
    print (testLTSAgainstSUT doorIOLTS doorImpl5)
    print (testLTSAgainstSUT doorIOLTS doorImpl6)
    print (testLTSAgainstSUT doorIOLTS doorImpl7)
    print (testLTSAgainstSUT doorIOLTS doorImpl8)
    print (testLTSAgainstSUT doorIOLTS doorImpl4)
