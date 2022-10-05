import Lab1

{-
    Time spent on this exercise: 60 minutes.

    We are not sure how to succesfully solve for this exercise. Furthermore we are uncertain about the implementation
    of the accuses function. Accusation is ambiguous in what is actually being accused for: stealing or lying.

    The implementation now returns a single Boolean value if an accusation is in the context of stealing (Mattew and Peter),
    and we are quite confident with this part of the implementation.

    Furthermore, the function calls upon the function recursively when a statement is in the context of lying, as these
    statements follow down a path to a final statement for stealing.
    If the accused boy argument is part of the accusers statement, this statement is syntactically formed in a conditional
    statement.

    Nevertheless, the recursive functions appear to be called upon inaccurately. The only accused boy ever
    passed on is analoguous to the accuser of the recursive function. We used pattern matching in the function
    to try and recreate the accuser's statement as they accuse only a specified number of boys, but this pattern
    matching actually results in the faulty recursive function call.
-}


accuses ::  Boy -> Boy -> Bool
accuses accuser accused
    | accuser == Matthew && (accused == Carl || accused == Matthew) = False -- Matthew does not accuse Carl and himself.
    | accuser == Peter && (accused == Matthew || accused == Jack)   = True  -- Peter accuses Matthew or Jack.
    | accuser == Jack && (accused == Matthew || accused == Peter)   = not $ (accuses Matthew accused) && (accuses Peter accused) -- Matthew and Peter are lying.
    | accuser == Arnold && (accused == Matthew || accused == Peter) = accuses Matthew accused /= accuses Peter accused -- Matthew and Peter are NOT saying the same.
    | accuser == Carl && accused == Arnold                          = not $ accuses Arnold accused  -- Arnold is lying.
    | otherwise = False


accusers ::  Boy -> [Boy]
accusers accused = [accuser | accuser <- boys, accuses accuser accused]