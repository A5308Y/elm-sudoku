module Main exposing (..)

import Array
import Board
import Char
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Keyboard
import Solver
import Types exposing (..)


main : Program Never Model Msg
main =
    program
        { init = ( initModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


initModel : Model
initModel =
    { editing = Nothing, board = Board.solveTest }


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Solve ] [ text "Solve" ]
        , button [ onClick Clear ] [ text "Clear" ]
        , div [] (Array.toList (Array.indexedMap (renderField model) model.board))
        ]


renderField : Model -> Int -> FieldState -> Html Msg
renderField model index state =
    case state of
        Empty ->
            let
                backgroundColor =
                    case model.editing of
                        Nothing ->
                            "white"

                        Just editedIndex ->
                            if editedIndex == index then
                                "green"
                            else
                                "white"
            in
            div [ fieldStyle index backgroundColor, onClick (SetEditing index) ] []

        PreFilled number ->
            div
                [ fieldStyle index "grey" ]
                [ text (Board.numberToString number) ]

        UserFilled number ->
            let
                backgroundColor =
                    case Array.get index (Board.errors model.board) of
                        Nothing ->
                            "white"

                        Just hasError ->
                            if hasError then
                                "red"
                            else
                                case model.editing of
                                    Nothing ->
                                        "white"

                                    Just editedIndex ->
                                        if editedIndex == index then
                                            "green"
                                        else
                                            "white"
            in
            div
                [ fieldStyle index backgroundColor, onClick (SetEditing index) ]
                [ text (Board.numberToString number) ]


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
    = SetEditing Int
    | Solve
    | Clear
    | KeyPressed Char


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        board =
            model.board
    in
    case msg of
        KeyPressed char ->
            case model.editing of
                Nothing ->
                    ( model, Cmd.none )

                Just index ->
                    ( { model | board = setNumber board index char }, Cmd.none )

        SetEditing index ->
            ( { model | editing = Just index }, Cmd.none )

        Solve ->
            let
                updatedBoard =
                    [ board ]
                        |> Solver.solve
                        |> List.head
                        |> Maybe.withDefault Board.empty
            in
            ( { model | board = updatedBoard }, Cmd.none )

        Clear ->
            let
                updatedBoard =
                    board
                        |> Array.map
                            (\entry ->
                                case entry of
                                    PreFilled _ ->
                                        entry

                                    _ ->
                                        Empty
                            )
            in
            ( { model | board = updatedBoard }, Cmd.none )


setNumber : Board -> Int -> Char -> Board
setNumber board index numberInput =
    if Char.isDigit numberInput then
        case Board.charToNumber numberInput of
            Nothing ->
                board

            Just number ->
                board
                    |> Array.set index (UserFilled number)
    else
        board
            |> Array.set index Empty


boxSize : number
boxSize =
    50


positionFromIndex : Int -> ( Int, Int )
positionFromIndex index =
    ( rem index 9 + 1, (index // 9) + 1 )


subscriptions : Model -> Sub Msg
subscriptions model =
    Keyboard.presses (\code -> KeyPressed (Char.fromCode code))
