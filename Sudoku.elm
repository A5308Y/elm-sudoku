module Main exposing (..)

import Dict
import Grid
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Solver exposing (hasError)
import Types exposing (..)


main =
    beginnerProgram { model = initModel, view = view, update = update }


initModel : Model
initModel =
    Grid.fromNotation
        "..18972462485619376794.385113578246946793512.89214637572631958.514278693983654712"


view model =
    div []
        [ button [ onClick Solve ] [ text "Solve" ]
        , div [] (Dict.values (Dict.map (fieldView model) model))
        ]


fieldView model position state =
    case state of
        Empty ->
            div [ fieldStyle position "white", onClick (SetEditing position state) ] []

        PreFilled number ->
            let
                backgroundColor =
                    if hasError model position number then
                        "red"
                    else
                        "grey"
            in
            div
                [ fieldStyle position backgroundColor ]
                [ text (toString number) ]

        UserFilled number ->
            let
                backgroundColor =
                    if hasError model position number then
                        "red"
                    else
                        "white"
            in
            div
                [ fieldStyle position backgroundColor, onClick (SetEditing position state) ]
                [ text (toString number) ]

        Editing maybeNumber ->
            let
                valueAttribute =
                    case maybeNumber of
                        Nothing ->
                            []

                        Just number ->
                            [ value (toString number) ]
            in
            div
                [ fieldStyle position "white" ]
                [ input
                    (valueAttribute
                        ++ [ onInput (SetNumber position)
                           , style [ ( "width", "20px" ) ]
                           , autofocus True
                           ]
                    )
                    []
                ]


fieldStyle ( xPosition, yPosition ) backgroundColor =
    style
        [ ( "width", toString boxSize ++ "px" )
        , ( "height", toString boxSize ++ "px" )
        , ( "box-shadow", "2px 0 0 0 #888, 0 2px 0 0 #888, 2px 2px 0 0 #888, 2px 0 0 0 #888 inset, 0 2px 0 0 #888 inset" )
        , ( "position", "absolute" )
        , ( "left", toString (xPosition * boxSize) ++ "px" )
        , ( "top", toString (yPosition * boxSize) ++ "px" )
        , ( "display", "flex" )
        , ( "align-items", "center" )
        , ( "justify-content", "center" )
        , ( "background-color", backgroundColor )
        ]


type Msg
    = SetEditing Position State
    | SetNumber Position String
    | Solve


update msg model =
    case msg of
        SetNumber position numberInput ->
            case String.toInt numberInput of
                Ok number ->
                    if number <= 9 && number >= 1 then
                        model
                            |> Dict.insert position (UserFilled number)
                    else
                        model

                Err _ ->
                    model

        SetEditing position state ->
            case state of
                PreFilled number ->
                    model
                        |> Dict.insert position (Editing (Just number))

                UserFilled number ->
                    model
                        |> Dict.insert position (Editing (Just number))

                Empty ->
                    model
                        |> Dict.insert position (Editing Nothing)

                _ ->
                    model

        Solve ->
            model
                |> Solver.solve
                |> List.head
                |> Maybe.withDefault Grid.empty


boxSize =
    50
