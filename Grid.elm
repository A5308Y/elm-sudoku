module Grid exposing (empty, fromNotation)

import Dict
import Types exposing (..)


empty : Model
empty =
    List.range 1 9
        |> List.concatMap emptyColumn
        |> Dict.fromList


fromNotation gridCode =
    gridCode
        |> String.toList
        |> List.indexedMap charToField
        |> Dict.fromList


emptyColumn x =
    List.range 1 9
        |> List.map (\y -> ( ( x, y ), Empty ))


charToField index number =
    let
        position =
            positionFromIndex index
    in
    if number == '.' then
        ( position, Empty )
    else
        case String.toInt (String.fromChar number) of
            Ok number ->
                ( position, PreFilled number )

            _ ->
                ( position, Empty )


positionFromIndex index =
    ( rem index 9 + 1, (index // 9) + 1 )


solvableGrid =
    "....9......8....3767.4..8...3..8246...........9214..7...6..9.8451....6......5...."


solvedGrid =
    "351897246248561937679423851135782469467935128892146375726319584514278693983654712"
