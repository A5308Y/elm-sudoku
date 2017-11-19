module Main exposing (..)

import Array
import BetterSolver
import Board
import Char
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Keyboard
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
    { editing = Nothing, board = Board.empty }


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Solve ] [ text "Solve" ]
        , button [ onClick Clear ] [ text "Clear User Input" ]
        , button [ onClick (LoadBoard Board.empty) ] [ text "Empty" ]
        , button [ onClick (LoadBoard Board.simple) ] [ text "Simple" ]
        , button [ onClick (LoadBoard Board.easy) ] [ text "Easy" ]
        , button [ onClick (LoadBoard Board.hard) ] [ text "Hard" ]
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
                [ fieldStyle index "lightgrey" ]
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

        Impossible ->
            div
                [ fieldStyle index "red" ]
                [ text "x" ]

        Trying number otherPossibleNumbers ->
            div
                [ fieldStyle index "white" ]
                [ div [] [ text (Board.numberToString number) ]
                , div [] [ text (toString (List.map Board.numberToString otherPossibleNumbers)) ]
                ]


fieldStyle : Int -> String -> Attribute Msg
fieldStyle index backgroundColor =
    let
        ( xPosition, yPosition ) =
            positionFromIndex index

        borderCss =
            [ Top, Right, Bottom, Left ]
                |> List.filter (conditionFilter index)
                |> border
    in
    style
        [ ( "width", toString boxSize ++ "px" )
        , ( "height", toString boxSize ++ "px" )
        , borderCss
        , ( "position", "absolute" )
        , ( "left", toString (xPosition * boxSize) ++ "px" )
        , ( "top", toString (yPosition * boxSize) ++ "px" )
        , ( "display", "flex" )
        , ( "align-items", "center" )
        , ( "justify-content", "center" )
        , ( "background-color", backgroundColor )
        ]


conditionFilter index borderPosition =
    case borderPosition of
        Top ->
            index < 9 || index > 26 && index < 36 || index > 53 && index < 63

        Left ->
            index % 3 == 0

        Right ->
            index % 9 == 8

        Bottom ->
            index > 71


border positions =
    ( "box-shadow"
    , String.join ", "
        [ "2px 0 0 0 #" ++ color positions Right
        , "0 2px 0 0 #" ++ color positions Bottom
        , "2px 0 0 0 #" ++ color positions Left ++ " inset"
        , "0 2px 0 0 #" ++ color positions Top ++ " inset"
        ]
    )


color positions position =
    if List.member position positions then
        "000"
    else
        "777"


type BorderPosition
    = Top
    | Right
    | Bottom
    | Left


type Msg
    = SetEditing Int
    | Solve
    | Clear
    | KeyPressed Char
    | LoadBoard Board


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        board =
            model.board
    in
    case msg of
        LoadBoard board ->
            ( { model | board = board }, Cmd.none )

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
                    board
                        |> BetterSolver.solve
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
