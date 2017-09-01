module Types exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)


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
    | Editing (Maybe Number)


type alias Board =
    Array FieldState


type alias Model =
    Board
