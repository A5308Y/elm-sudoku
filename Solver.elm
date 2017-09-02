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
    List.all (\field -> field /= Empty) (Array.toList board)


possibleBoards : Board -> List Board
possibleBoards board =
    board
        |> possibleNumbersByIndex
        |> Array.indexedMap (fillNumberFromPossibleNumbers board)
        |> Array.filter (\board -> possibleNumbersByIndex board /= Array.empty)
        |> Array.toList


fillNumberFromPossibleNumbers : Board -> Int -> List (Maybe Number) -> Board
fillNumberFromPossibleNumbers board index possibleNumbers =
    case List.head possibleNumbers of
        Nothing ->
            Array.empty

        Just maybeNumber ->
            case maybeNumber of
                Nothing ->
                    Array.empty

                Just number ->
                    Array.set index (UserFilled number) board


possibleNumbersByIndex : Board -> Array.Array (List (Maybe Number))
possibleNumbersByIndex board =
    board
        |> Array.indexedMap (possibleNumbersForIndex board)


possibleNumbersForIndex : Board -> Int -> FieldState -> List (Maybe Number)
possibleNumbersForIndex board index fieldState =
    case fieldState of
        Empty ->
            validEntries
                |> List.filter (numberNotTaken board index)

        Editing _ ->
            validEntries
                |> List.filter (numberNotTaken board index)

        _ ->
            []


numberNotTaken : Board -> Int -> Maybe Number -> Bool
numberNotTaken board index entry =
    not <| List.member entry (Board.numbersToCheck index board)


validEntries : List (Maybe Number)
validEntries =
    [ Just One
    , Just Two
    , Just Three
    , Just Four
    , Just Five
    , Just Six
    , Just Seven
    , Just Eight
    , Just Nine
    ]
