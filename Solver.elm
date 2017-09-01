module Solver exposing (solve)

import Array
import Board
import Types exposing (..)


solve : Board -> List Board
solve board =
    if includesSolution (possibleBoards board) then
        board
            |> possibleBoards
            |> Array.toList
            |> List.head
            |> Maybe.withDefault Board.empty
            |> List.singleton
    else
        board
            |> possibleBoards
            |> Array.toList
            |> List.concatMap solve


includesSolution boards =
    List.any
        (\board -> List.all (\field -> field /= Empty) (Array.toList board))
        (Array.toList boards)


possibleBoards board =
    board
        |> possibleNumbersByIndex
        |> Array.indexedMap (nextPossibleNumbers board)
        |> Array.filter (\board -> possibleNumbersByIndex board /= Array.empty)


nextPossibleNumbers board index possibleNumbers =
    case List.head possibleNumbers of
        Nothing ->
            Array.empty

        Just maybeNumber ->
            case maybeNumber of
                Nothing ->
                    Array.empty

                Just number ->
                    Array.set index (UserFilled number) board



--possibleNumbersByIndex : Board -> List Maybe


possibleNumbersByIndex board =
    board
        |> Array.indexedMap (possibleNumbersForIndex board)


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


numberNotTaken board index entry =
    not <| List.member entry (Board.numbersToCheck index board)


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
