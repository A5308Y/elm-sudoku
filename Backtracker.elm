module Backtracker exposing (backtrack)

import Array
import Types exposing (..)


backtrack board index =
    let
        maybeFirstTryingEntry =
            board
                |> Array.toList
                |> List.indexedMap (,)
                |> List.reverse
                |> List.filter tryingFilter
                |> List.head
    in
    case maybeFirstTryingEntry of
        Nothing ->
            Array.repeat 81 Impossible

        Just firstTryingEntry ->
            firstTryingEntry
                |> tryNextEntry (convertImpossibleToEmpty board)


tryNextEntry : Board -> ( Int, FieldState ) -> Board
tryNextEntry board ( index, tryEntry ) =
    let
        updatedTryEntry =
            case tryEntry of
                Trying number otherPossibleNumbers ->
                    case otherPossibleNumbers of
                        chosenNumber :: numbersLeft ->
                            Trying chosenNumber numbersLeft

                        [] ->
                            Impossible

                _ ->
                    tryEntry
    in
    Array.set index updatedTryEntry board


tryingFilter : ( Int, FieldState ) -> Bool
tryingFilter ( index, entry ) =
    case entry of
        Trying _ otherPossibleNumbers ->
            True

        _ ->
            False


convertImpossibleToEmpty board =
    Array.map
        (\entry ->
            case entry of
                Impossible ->
                    Empty

                _ ->
                    entry
        )
        board
