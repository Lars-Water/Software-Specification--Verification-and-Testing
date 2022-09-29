-- This file is property of the group Notorious Fortunate Panda © 2022
-- Time spent on this exercise was: 45 minutes 

module Exercise2 ()where
import Data.List
import Test.QuickCheck
import Test.QuickCheck.Gen
import LTS

tupToStr :: (Gen Char,Char) -> Gen Label
tupToStr (x,y) = do
    x <- x
    return [x,y]

inOrOut :: Gen Char
inOrOut = do
    m <- choose (False, True)
    return $ if m then '?' else '!'


-- Generate a random states
statesGen :: Gen [State]
statesGen  = do
    n <- choose (1,4)  -- number of states is at most 5 to keep overview
    return [0..n]
-- ['?', '?', '!', '?', '!']
-- [a,b,c,d,e]
-- []
-- Generate a random alphabet
labelsGen :: Gen [Label]
labelsGen = do
    n <- choose (1, 4) -- length of labels
    zipped <- sequence $ map tupToStr (map (take n (repeat inOrOut)) (take n ['a'..'z'])) -- zip the label type with the alphabet
    return zipped-- note our labels are for now very simple where the most complex is just one char

transitionGen :: Gen [LabeledTransition]
transitionGen  = do
    states <- statesGen
    labels <- labelsGen
    return [(s1, l, s2) | s1 <- states, s2 <- states, l <- labels]

ltsGen :: Gen IOLTS
ltsGen  = do
    transitions <- transitionGen
    return (createIOLTS transitions)

