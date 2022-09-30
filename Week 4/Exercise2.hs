-- This file is property of the group Notorious Fortunate Panda © 2022
-- Time spent: 270 minutes --

module Exercise2 where
import Exercise1
import Data.List
import Test.QuickCheck
import Test.QuickCheck.Gen
import Control.Monad (replicateM)
import LTS

-- Helper function to transform two chars ("?", "a") to a Label ?a
tupToStr :: (Char,Char) ->  Label
tupToStr (x,y) = [x,y]

-- Helper function to randomly choose either ? or ! to determine in or output
inOrOut :: Gen Char
inOrOut = do
    m <- choose (False, True)
    return $ if m then '?' else '!'

-- Generate a random states
statesGen :: Gen [State]
statesGen  = do
    n <- choose (1,4)  -- number of states is at most 5 to keep overview
    return [0..n]

-- Generate a random alphabet
labelsGen :: Gen [Label]
labelsGen = do
    n <- choose (1, 4) -- choice of possible labels is small to keep it simple so we have an overview
    inAndOut <- replicateM n inOrOut -- generate a list of random in or outputs
    return $ map tupToStr (zip inAndOut (take n ['a'..'z'])) -- zip the label type with the alphabet

-- Generate a random transition
transitionGen :: Gen [LabeledTransition]
transitionGen  = do
    states <- statesGen
    labels <- labelsGen
    return [(s1, l, s2) | s1 <- states, s2 <- states, l <- labels]

-- Generate a random IOLTS
ltsGen :: Gen IOLTS
ltsGen  = do
    transitions <- transitionGen
    return (createIOLTS transitions)

main2 :: IO ()
main2 = do
    print ("To test our validateLTS function we have to generate a random IOLTS")
    print ("We did this by allowing quickCheck to generate a few random IOLTS")
    print ("After which we tested for all the IOLTS if the validateLTS would hold")
    quickCheck $ forAll ltsGen validateLTS
