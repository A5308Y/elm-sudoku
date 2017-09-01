module Solver exposing (solve)

import Array
import Board
import Set
import Types exposing (..)


--solve : Board -> Board


solve board =
    let
        possibleNumbersByIndex =
            board
                |> Array.indexedMap
                    (\index fieldState ->
                        case fieldState of
                            Empty ->
                                List.filter
                                    (\entry ->
                                        not <| List.member entry (Board.numbersToCheck index board)
                                    )
                                    validEntries

                            Editing _ ->
                                List.filter
                                    (\entry ->
                                        not <| List.member entry (Board.numbersToCheck index board)
                                    )
                                    validEntries

                            _ ->
                                []
                    )
                |> Array.toList
    in
    possibleNumbersByIndex


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



--    let
--        numbersPresent =
--            board
--                |> Dict.filter filledOutFieldFilter
--                |> Dict.values
--                |> List.filterMap stateToNumberMapper
--        numbersLeft =
--            List.range 1 9
--                |> List.map (numbersLeftMapper numbersPresent)
--        numbersLeftDict =
--            Dict.fromList numbersLeft
--        fieldsLeft =
--            board
--                |> Dict.filter (\_ state -> state == Empty)
--                |> Dict.keys
--        correctBoards =
--            fieldsLeft
--                |> List.concatMap (possibleBoards board (Dict.keys numbersLeftDict))
--                |> List.filter (not << hasAnyError)
--    in
--    if List.length fieldsLeft == 1 then
--        correctBoards
--    else
--        correctBoards
--            |> List.concatMap solve
--possibleBoards : Board -> List Int -> Int -> List Board
--possibleBoards board numbersLeft position =
--    numbersLeft
--        |> List.map (\number -> Dict.insert position (PreFilled number) board)
--numbersLeftMapper : List a -> a -> ( a, Int )
--numbersLeftMapper numbersPresent numberInField =
--    let
--        matchingNumbers =
--            List.filter (\number -> number == numberInField) numbersPresent
--    in
--    ( numberInField, 9 - List.length matchingNumbers )
--filledOutFieldFilter : a -> State -> Bool
--filledOutFieldFilter _ state =
--    case state of
--        PreFilled _ ->
--            True
--        UserFilled _ ->
--            True
--        _ ->
--            False
--stateToNumberMapper : State -> Maybe Int
--stateToNumberMapper state =
--    case state of
--        UserFilled number ->
--            Just number
--        PreFilled number ->
--            Just number
--        _ ->
--            Nothing
--hasAnyError : Board -> Bool
--hasAnyError board =
--    False
--Dict.map
--    (\position state ->
--        case state of
--            UserFilled number ->
--                hasError board position number
--            PreFilled number ->
--                hasError board position number
--            _ ->
--                False
--    )
--    board
--    |> Dict.values
--    |> List.any identity
