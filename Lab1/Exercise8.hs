import Lab1

{-
    Determine the accusation of one boy about another boy.
-}
accuses ::  Boy -> Boy -> Bool
accuses accuser accused
    | accuser == Matthew && (accused == Carl || accused == Matthew) = False -- Matthew does not accuse Carl and himself.
    | accuser == Peter && (accused == Matthew || accused == Jack)   = True  -- Peter accuses Matthew or Jack.
    | accuser == Jack && (accused == Matthew || accused == Peter)   = not $ (accuses Matthew accused) && (accuses Peter accused) -- Matthew and Peter are lying.
    | accuser == Arnold && (accused == Matthew || accused == Peter) = accuses Matthew accused /= accuses Peter accused -- Matthew and Peter are NOT saying the same.
    | accuser == Carl && accused == Arnold                          = not $ accuses Arnold accused  -- Arnold is lying.
    | otherwise = False

{-
    Select the boys that accuse the requested boy. All possible boys are
    determined for their accusation by use of the accuses function. If this
    function returns true the boy is appended to the list.

    accused: Boy accused
-}
accusers ::  Boy -> [Boy]
accusers accused = [accuser | accuser <- boys, accuses accuser accused]