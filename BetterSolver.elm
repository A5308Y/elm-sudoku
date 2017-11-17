module BetterSolver exposing (possibleNumbersForIndex, solve)

import Array
import Backtracker exposing (backtrack)
import Board
import Types exposing (..)


solve : Board -> Board
solve board =
    let
        debug =
            Debug.log "board" (Board.toNotation board)
    in
    if isSolution board then
        convertTryingToPrefilled board
    else if isImpossible board then
        board
    else
        case findNextEmptyEntry board of
            Nothing ->
                board

            Just ( index, entry ) ->
                if entry == Impossible then
                    backtrack board (index - 1)
                        |> solve
                else
                    case possibleNumbersForIndex board index of
                        [] ->
                            backtrack board index
                                |> solve

                        firstPossibleNumber :: otherPossibleNumbers ->
                            board
                                |> Array.set index (Trying firstPossibleNumber otherPossibleNumbers)
                                |> solve


convertTryingToPrefilled board =
    Array.map
        (\entry ->
            case entry of
                Trying number otherPossibleNumbers ->
                    PreFilled number

                _ ->
                    entry
        )
        board


boardlogger board =
    let
        debug =
            Debug.log "board" (Board.toNotation board)
    in
    board


findNextEmptyEntry : Board -> Maybe ( Int, FieldState )
findNextEmptyEntry board =
    board
        |> Array.indexedMap (,)
        |> Array.filter emptyFilter
        |> Array.toList
        |> List.head


emptyFilter : ( Int, FieldState ) -> Bool
emptyFilter ( index, entry ) =
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
    board
        |> Array.toList
        |> List.all impossibleFilter


impossibleFilter field =
    case field of
        Impossible ->
            True

        _ ->
            False


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


possibleNumbersForIndex : Board -> Int -> List Number
possibleNumbersForIndex board index =
    validEntries
        |> List.filter (numberPossible board index)


numberPossible : Board -> Int -> Number -> Bool
numberPossible board index number =
    not <| List.member number (Board.numbersToCheck index board)


validEntries : List Number
validEntries =
    [ One, Two, Three, Four, Five, Six, Seven, Eight, Nine ]
