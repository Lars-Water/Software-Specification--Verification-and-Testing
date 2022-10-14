-- This file is property of the group Notorious Fortunate Panda © 2022
-- Time spent: 10 minutes --

{-
    There is a difference between the symmetric closure of the transitive closure of relation R, and
    the transitive closure of the symmetric closure of relation R.

    An example to illustrate this difference is as follows:

    Define R = {(1,2)}

    Transitive closure of R -> {(1,2)}
        Subsequently, the symmetric closure that follows is R ∪ R^-1 -> {(1,2),(2,1)}

    Symmetric closure of R -> {(1,2),(2,1)}
        Subsequently, the transitive closure that follows -> {(1,2),(2,1),(1,1),(2,2)}

    This example implies there is difference in order of closure on relation R.
-}
