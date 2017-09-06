module BetterSolver exposing (solve)

import Array
import Board
import Types exposing (..)


solve : Board -> Board
solve board =
    let
        debug =
            Debug.log "board" (Board.toNotation board)
    in
    if isSolution board then
        board
    else
        solve (fillOneEntry board)


fillOneEntry : Board -> Board
fillOneEntry board =
    Array.indexedMap (selectEntry board) board


selectEntry board index entry =
    case entry of
        Empty ->
            case possibleNumbersForIndex board index of
                firstPossibleNumber :: otherPossibleNumbers ->
                    case List.head otherPossibleNumbers of
                        Nothing ->
                            UserFilled firstPossibleNumber

                        Just firstOtherPossibleNumber ->
                            entry

                [] ->
                    Empty

        _ ->
            entry


isSolution : Board -> Bool
isSolution board =
    board
        |> Array.toList
        |> List.all (\field -> field /= Empty)


possibleNumbersByIndex : Board -> Array.Array (List Number)
possibleNumbersByIndex board =
    board
        |> Array.indexedMap (\index entry -> possibleNumbersForIndex board index)


possibleNumbersForIndex : Board -> Int -> List Number
possibleNumbersForIndex board index =
    validEntries
        |> List.filter (numberNotTaken board index)


numberNotTaken : Board -> Int -> Number -> Bool
numberNotTaken board index entry =
    not <| List.member entry (Board.numbersToCheck index board)


validEntries : List Number
validEntries =
    [ One, Two, Three, Four, Five, Six, Seven, Eight, Nine ]
