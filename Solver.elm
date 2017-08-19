module Solver exposing (hasError, solve)

import Dict
import Types exposing (..)


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

        fieldsLeft =
            board
                |> Dict.filter (\_ state -> state == Empty)
    in
    board



--try out one number in one field -> is there an error? -> try next number, then next field


numbersLeftMapper numbersPresent numberInField =
    let
        matchingNumbers =
            List.filter (\number -> number == numberInField) numbersPresent
    in
    [ numberInField, 9 - List.length matchingNumbers ]


filledOutFieldFilter _ state =
    case state of
        PreFilled _ ->
            True

        UserFilled _ ->
            True

        _ ->
            False


stateToNumberMapper state =
    case state of
        UserFilled number ->
            Just number

        PreFilled number ->
            Just number

        _ ->
            Nothing


hasError model position number =
    isDuplicatePresent number (findColumn model position)
        || isDuplicatePresent number (findRow model position)
        || isDuplicatePresent number (findBox model position)


findColumn model ( givenXPosition, givenYPosition ) =
    Dict.filter (\( xPosition, yPosition ) _ -> xPosition == givenXPosition) model


findRow model ( givenXPosition, givenYPosition ) =
    Dict.filter (\( xPosition, yPosition ) _ -> yPosition == givenYPosition) model


findBox model ( givenXPosition, givenYPosition ) =
    Dict.filter
        (\( xPosition, yPosition ) _ ->
            ((yPosition - 1) // 3 == (givenYPosition - 1) // 3)
                && ((xPosition - 1) // 3 == (givenXPosition - 1) // 3)
        )
        model


isDuplicatePresent givenNumber fields =
    let
        numberCount =
            fields
                |> Dict.filter (numberPresentInField givenNumber)
                |> Dict.size
    in
    numberCount > 1


numberPresentInField givenNumber _ state =
    case state of
        UserFilled number ->
            givenNumber == number

        PreFilled number ->
            givenNumber == number

        _ ->
            False
