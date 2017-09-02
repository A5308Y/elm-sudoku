module Types exposing (..)

import Array exposing (Array)


type Number
    = One
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine


type FieldState
    = PreFilled Number
    | UserFilled Number
    | Empty


type alias Board =
    Array FieldState


type alias Model =
    { editing : Maybe Int, board : Board }
