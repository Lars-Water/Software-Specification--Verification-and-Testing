import Data.List
import LTS
import Test.QuickCheck


{-
    Invalid IOLTS:

    -   Intersection of Input and Output lables is not equal to empty set; L-i ∩ L-u ≠ ∅
    -   Empty Input label set
    -   Empty Output label set
-}

validateLTS :: IOLTS -> Bool
validateLTS (_, inputs, outputs, _, _) = inputs /= [] &&
                                         outputs /= [] &&
                                         intersect inputs outputs == []