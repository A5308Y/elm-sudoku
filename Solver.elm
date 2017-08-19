module Solver exposing (hasAnyError, hasError, possibleBoards, solve)

import Dict exposing (Dict)
import Types exposing (..)


solve : Board -> List Board
solve board =
    let
        numbersPresent =
            board
                |> Dict.filter filledOutFieldFilter
                |> Dict.values
                |> List.filterMap stateToNumberMapper

        numbersLeft =
            List.range 1 9
                |> List.map (numbersLeftMapper numbersPresent)

        numbersLeftDict =
            Dict.fromList numbersLeft

        fieldsLeft =
            board
                |> Dict.filter (\_ state -> state == Empty)
                |> Dict.keys

        correctBoards =
            fieldsLeft
                |> List.concatMap (possibleBoards board (Dict.keys numbersLeftDict))
                |> List.filter (not << hasAnyError)
    in
    if List.length fieldsLeft == 1 then
        correctBoards
    else
        correctBoards
            |> List.concatMap solve


possibleBoards : Board -> List Int -> Position -> List Board
possibleBoards board numbersLeft position =
    numbersLeft
        |> List.map (\number -> Dict.insert position (PreFilled number) board)


numbersLeftMapper : List a -> a -> ( a, Int )
numbersLeftMapper numbersPresent numberInField =
    let
        matchingNumbers =
            List.filter (\number -> number == numberInField) numbersPresent
    in
    ( numberInField, 9 - List.length matchingNumbers )


filledOutFieldFilter : a -> State -> Bool
filledOutFieldFilter _ state =
    case state of
        PreFilled _ ->
            True

        UserFilled _ ->
            True

        _ ->
            False


stateToNumberMapper : State -> Maybe Int
stateToNumberMapper state =
    case state of
        UserFilled number ->
            Just number

        PreFilled number ->
            Just number

        _ ->
            Nothing


hasAnyError : Board -> Bool
hasAnyError board =
    Dict.map
        (\position state ->
            case state of
                UserFilled number ->
                    hasError board position number

                PreFilled number ->
                    hasError board position number

                _ ->
                    False
        )
        board
        |> Dict.values
        |> List.any identity


hasError : Dict.Dict ( Int, Int ) State -> ( Int, Int ) -> Int -> Bool
hasError model position number =
    isDuplicatePresent number (findColumn model position)
        || isDuplicatePresent number (findRow model position)
        || isDuplicatePresent number (findBox model position)


findColumn : Board -> Position -> Dict Position State
findColumn model ( givenXPosition, givenYPosition ) =
    Dict.filter (\( xPosition, yPosition ) _ -> xPosition == givenXPosition) model


findRow : Board -> Position -> Dict Position State
findRow model ( givenXPosition, givenYPosition ) =
    Dict.filter (\( xPosition, yPosition ) _ -> yPosition == givenYPosition) model


findBox : Board -> Position -> Dict Position State
findBox model ( givenXPosition, givenYPosition ) =
    Dict.filter
        (\( xPosition, yPosition ) _ ->
            ((yPosition - 1) // 3 == (givenYPosition - 1) // 3)
                && ((xPosition - 1) // 3 == (givenXPosition - 1) // 3)
        )
        model


isDuplicatePresent : Int -> Dict.Dict Position State -> Bool
isDuplicatePresent givenNumber fields =
    let
        numberCount =
            fields
                |> Dict.values
                |> List.filter (numberPresentInField givenNumber)
                |> List.length
    in
    numberCount > 1


numberPresentInField : Int -> State -> Bool
numberPresentInField givenNumber state =
    case state of
        UserFilled number ->
            givenNumber == number

        PreFilled number ->
            givenNumber == number

        _ ->
            False
