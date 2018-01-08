module Solver exposing (isImpossible, possibleNumbersForIndex, solve)

import Array
import Backtracker exposing (backtrack)
import Board
import Types exposing (..)


solve : Board -> Board
solve board =
    if isSolution board then
        convertTryingToPrefilled board
    else if isImpossible board then
        Array.repeat 81 Impossible
    else
        case findNextEmptyEntry board of
            Nothing ->
                board

            Just ( index, entry ) ->
                if entry == Impossible then
                    (index - 1)
                        |> backtrack board
                        |> solve
                else
                    case possibleNumbersForIndex board index of
                        [] ->
                            index
                                |> backtrack board
                                |> solve

                        firstPossibleNumber :: otherPossibleNumbers ->
                            board
                                |> Array.set index (Trying firstPossibleNumber otherPossibleNumbers)
                                |> solve


convertTryingToPrefilled : Board -> Board
convertTryingToPrefilled board =
    Array.map tryingToPrefilledConverter board


findNextEmptyEntry : Board -> Maybe ( Int, FieldState )
findNextEmptyEntry board =
    board
        |> Array.indexedMap (,)
        |> Array.filter emptyFilter
        |> Array.toList
        |> List.head


emptyFilter : ( Int, FieldState ) -> Bool
emptyFilter ( _, entry ) =
    case entry of
        Empty ->
            True

        Impossible ->
            True

        _ ->
            False


isSolution : Board -> Bool
isSolution board =
    board
        |> Array.toList
        |> List.all fieldSolvedFilter


isImpossible : Board -> Bool
isImpossible board =
    (board
        |> Array.toList
        |> List.all impossibleFilter
    )
        || Board.hasError board


impossibleFilter : FieldState -> Bool
impossibleFilter field =
    case field of
        Impossible ->
            True

        _ ->
            False


fieldSolvedFilter : FieldState -> Bool
fieldSolvedFilter field =
    case field of
        UserFilled _ ->
            True

        PreFilled _ ->
            True

        Trying _ _ ->
            True

        _ ->
            False


tryingToPrefilledConverter : FieldState -> FieldState
tryingToPrefilledConverter field =
    case field of
        Trying number _ ->
            PreFilled number

        _ ->
            field


possibleNumbersForIndex : Board -> Int -> List Number
possibleNumbersForIndex board index =
    validNumbers
        |> List.filter (numberPossible board index)


numberPossible : Board -> Int -> Number -> Bool
numberPossible board index number =
    not <| List.member number (Board.numbersToCheck index board)


validNumbers : List Number
validNumbers =
    [ One, Two, Three, Four, Five, Six, Seven, Eight, Nine ]
