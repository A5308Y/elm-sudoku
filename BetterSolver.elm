module BetterSolver exposing (possibleNumbersForIndex, solve)

import Array
import Board
import Types exposing (..)


solve : Board -> Board
solve board =
    let
        nextEmptyEntry =
            board
                |> boardlogger
                |> findNextEmptyEntry
    in
    if isSolution board then
        board
    else
        case nextEmptyEntry of
            Nothing ->
                board

            Just ( index, entry ) ->
                case possibleNumbersForIndex board index of
                    [] ->
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
                                    |> tryNextEntry board
                                    |> solve

                    firstPossibleNumber :: otherPossibleNumbers ->
                        board
                            |> Array.set index (Trying firstPossibleNumber otherPossibleNumbers)
                            |> solve



--Trying Impossible
--Wenn vorwärts überschreiben
--Wenn rückwärts nach oben geben


boardlogger board =
    let
        debug =
            Debug.log "board" (Board.toNotation board)
    in
    board


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

        --not <| List.isEmpty otherPossibleNumbers
        _ ->
            False


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

        _ ->
            False


isSolution : Board -> Bool
isSolution board =
    board
        |> Array.toList
        |> List.all fieldSolvedFilter


fieldSolvedFilter field =
    case field of
        UserFilled _ ->
            True

        PreFilled _ ->
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
