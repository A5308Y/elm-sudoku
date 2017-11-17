module OnlyPossibleFiller exposing (run)


run board =
    case Array.toList (Array.filter tryingFilter (Array.indexedMap (,) board)) of
        [] ->
            Array.indexedMap (fillEntryIfOnlyOnePossibility board) board

        _ ->
            board


fillEntryIfOnlyOnePossibility board index entry =
    case possibleNumbersForIndex board index of
        [ onlyPossibility ] ->
            PreFilled onlyPossibility

        _ ->
            entry
