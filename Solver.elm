module Solver exposing (solveBoard)

import Array
import Board
import Types exposing (..)


solveBoard board =
    case solve [ board ] of
        [ board ] ->
            board

        _ ->
            Board.empty


solve : List Board -> List Board
solve boards =
    case boards of
        [ board ] ->
            if isSolution board then
                [ board ]
            else
                board
                    |> possibleBoards
                    |> solve

        firstBoard :: otherBoards ->
            if isSolution firstBoard then
                [ firstBoard ]
            else
                solve otherBoards

        [] ->
            []


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
    case possibleNumbers of
        [] ->
            Array.empty

        [ number ] ->
            Array.set index (UserFilled number) board

        firstNumber :: otherNumbers ->
            Array.set index (UserFilled firstNumber) board


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
