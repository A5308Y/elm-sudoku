module Types exposing (..)

import Dict exposing (Dict)


type alias Position =
    ( Int, Int )


type State
    = PreFilled Int
    | UserFilled Int
    | Empty
    | Editing (Maybe Int)


type alias Model =
    Dict Position State
