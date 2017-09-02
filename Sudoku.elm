module Main exposing (..)

import Array
import Board
import Char
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Solver
import Types exposing (..)


main : Program Never Model Msg
main =
    beginnerProgram { model = initModel, view = view, update = update }


initModel : Model
initModel =
    Board.fromNotation
        "..18972462485619376794.3851.3578246946793512.89214637572631958.514278693983654712"



--Board.solvableBoard
--|> Board.fromNotation


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Solve ] [ text "Solve" ]
        , div [] (Array.toList (Array.indexedMap (renderField model) model))
        ]


renderField : Model -> Int -> FieldState -> Html Msg
renderField model index state =
    case state of
        Empty ->
            div [ fieldStyle index "white", onClick (SetEditing index state) ] []

        PreFilled number ->
            div
                [ fieldStyle index "grey" ]
                [ text (numberToString number) ]

        UserFilled number ->
            let
                backgroundColor =
                    case Array.get index (Board.errors model) of
                        Nothing ->
                            "white"

                        Just hasError ->
                            if hasError then
                                "red"
                            else
                                "white"
            in
            div
                [ fieldStyle index backgroundColor, onClick (SetEditing index state) ]
                [ text (numberToString number) ]

        Editing maybeNumber ->
            let
                valueAttribute =
                    case maybeNumber of
                        Nothing ->
                            []

                        Just number ->
                            [ value (numberToString number) ]
            in
            div
                [ fieldStyle index "white" ]
                [ input
                    (valueAttribute
                        ++ [ onInput (SetNumber index)
                           , style [ ( "width", "20px" ) ]
                           , autofocus True
                           ]
                    )
                    []
                ]


fieldStyle : Int -> String -> Attribute Msg
fieldStyle index backgroundColor =
    let
        ( xPosition, yPosition ) =
            positionFromIndex index
    in
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
    = SetEditing Int FieldState
    | SetNumber Int String
    | Solve


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetNumber index numberInput ->
            case String.toList numberInput of
                [ char ] ->
                    if Char.isDigit char then
                        case Board.charToNumber char of
                            Nothing ->
                                model

                            Just number ->
                                model
                                    |> Array.set index (UserFilled number)
                    else
                        model
                            |> Array.set index Empty

                _ ->
                    model
                        |> Array.set index Empty

        SetEditing index state ->
            case state of
                PreFilled number ->
                    model
                        |> Array.set index (Editing (Just number))

                UserFilled number ->
                    model
                        |> Array.set index (Editing (Just number))

                Empty ->
                    model
                        |> Array.set index (Editing Nothing)

                _ ->
                    model

        Solve ->
            model
                |> Solver.solve
                |> List.head
                |> Maybe.withDefault Board.empty


boxSize : number
boxSize =
    50


numberToString : Number -> String
numberToString number =
    case number of
        One ->
            "1"

        Two ->
            "2"

        Three ->
            "3"

        Four ->
            "4"

        Five ->
            "5"

        Six ->
            "6"

        Seven ->
            "7"

        Eight ->
            "8"

        Nine ->
            "9"


positionFromIndex index =
    ( rem index 9 + 1, (index // 9) + 1 )
