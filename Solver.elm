module Solver exposing (solve)

import Array
import Board
import Types exposing (..)


solve : List Board -> List Board
solve boards =
    let
        firstBoard =
            List.head boards

        debug =
            case firstBoard of
                Nothing ->
                    ""

                Just board ->
                    Debug.log "checking board" (Board.toNotation board)
    in
    case firstBoard of
        Nothing ->
            []

        Just board ->
            if isSolution board then
                [ board ]
            else
                board
                    |> possibleBoards
                    |> solve


isSolution : Board -> Bool
isSolution board =
    board
        |> Array.toList
        |> List.all (\field -> field /= Empty)


possibleBoards : Board -> List Board
possibleBoards board =
    board
        |> possibleNumbersByIndex
        |> Array.indexedMap (fillNumberFromPossibleNumbers board)
        |> Array.filter (\board -> possibleNumbersByIndex board /= Array.empty)
        |> Array.toList


fillNumberFromPossibleNumbers : Board -> Int -> List Number -> Board
fillNumberFromPossibleNumbers board index possibleNumbers =
    case List.head possibleNumbers of
        Nothing ->
            Array.empty

        Just number ->
            Array.set index (UserFilled number) board


possibleNumbersByIndex : Board -> Array.Array (List Number)
possibleNumbersByIndex board =
    board
        |> Array.indexedMap (possibleNumbersForIndex board)


possibleNumbersForIndex : Board -> Int -> FieldState -> List Number
possibleNumbersForIndex board index fieldState =
    case fieldState of
        Empty ->
            validEntries
                |> List.filter (numberNotTaken board index)

        _ ->
            []


numberNotTaken : Board -> Int -> Number -> Bool
numberNotTaken board index entry =
    not <| List.member entry (Board.numbersToCheck index board)


validEntries : List Number
validEntries =
    [ One, Two, Three, Four, Five, Six, Seven, Eight, Nine ]
