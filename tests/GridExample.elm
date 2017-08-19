module GridExample exposing (..)

import Dict
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Grid
import Test exposing (..)
import Types exposing (..)


suite : Test
suite =
    describe "Grid"
        [ describe ".fromNotation"
            [ test "returns the board given by the example" <|
                \_ ->
                    let
                        exampleNotation =
                            "351897246248561937679423851135782469467935128892146375726319584514278693983654712"
                    in
                    Expect.equal (Grid.fromNotation exampleNotation) boardForExampleNotation
            , test "represents . as Empty" <|
                \_ ->
                    let
                        exampleNotationWithEmptyField =
                            ".51897246248561937679423851135782469467935128892146375726319584514278693983654712"
                    in
                    Expect.equal
                        (Grid.fromNotation exampleNotationWithEmptyField)
                        (Dict.insert ( 1, 1 ) Empty boardForExampleNotation)
            ]
        ]


boardForExampleNotation =
    Dict.fromList
        [ ( ( 1, 1 ), PreFilled 3 )
        , ( ( 2, 1 ), PreFilled 5 )
        , ( ( 3, 1 ), PreFilled 1 )
        , ( ( 4, 1 ), PreFilled 8 )
        , ( ( 5, 1 ), PreFilled 9 )
        , ( ( 6, 1 ), PreFilled 7 )
        , ( ( 7, 1 ), PreFilled 2 )
        , ( ( 8, 1 ), PreFilled 4 )
        , ( ( 9, 1 ), PreFilled 6 )
        , ( ( 1, 2 ), PreFilled 2 )
        , ( ( 2, 2 ), PreFilled 4 )
        , ( ( 3, 2 ), PreFilled 8 )
        , ( ( 4, 2 ), PreFilled 5 )
        , ( ( 5, 2 ), PreFilled 6 )
        , ( ( 6, 2 ), PreFilled 1 )
        , ( ( 7, 2 ), PreFilled 9 )
        , ( ( 8, 2 ), PreFilled 3 )
        , ( ( 9, 2 ), PreFilled 7 )
        , ( ( 1, 3 ), PreFilled 6 )
        , ( ( 2, 3 ), PreFilled 7 )
        , ( ( 3, 3 ), PreFilled 9 )
        , ( ( 4, 3 ), PreFilled 4 )
        , ( ( 5, 3 ), PreFilled 2 )
        , ( ( 6, 3 ), PreFilled 3 )
        , ( ( 7, 3 ), PreFilled 8 )
        , ( ( 8, 3 ), PreFilled 5 )
        , ( ( 9, 3 ), PreFilled 1 )
        , ( ( 1, 4 ), PreFilled 1 )
        , ( ( 2, 4 ), PreFilled 3 )
        , ( ( 3, 4 ), PreFilled 5 )
        , ( ( 4, 4 ), PreFilled 7 )
        , ( ( 5, 4 ), PreFilled 8 )
        , ( ( 6, 4 ), PreFilled 2 )
        , ( ( 7, 4 ), PreFilled 4 )
        , ( ( 8, 4 ), PreFilled 6 )
        , ( ( 9, 4 ), PreFilled 9 )
        , ( ( 1, 5 ), PreFilled 4 )
        , ( ( 2, 5 ), PreFilled 6 )
        , ( ( 3, 5 ), PreFilled 7 )
        , ( ( 4, 5 ), PreFilled 9 )
        , ( ( 5, 5 ), PreFilled 3 )
        , ( ( 6, 5 ), PreFilled 5 )
        , ( ( 7, 5 ), PreFilled 1 )
        , ( ( 8, 5 ), PreFilled 2 )
        , ( ( 9, 5 ), PreFilled 8 )
        , ( ( 1, 6 ), PreFilled 8 )
        , ( ( 2, 6 ), PreFilled 9 )
        , ( ( 3, 6 ), PreFilled 2 )
        , ( ( 4, 6 ), PreFilled 1 )
        , ( ( 5, 6 ), PreFilled 4 )
        , ( ( 6, 6 ), PreFilled 6 )
        , ( ( 7, 6 ), PreFilled 3 )
        , ( ( 8, 6 ), PreFilled 7 )
        , ( ( 9, 6 ), PreFilled 5 )
        , ( ( 1, 7 ), PreFilled 7 )
        , ( ( 2, 7 ), PreFilled 2 )
        , ( ( 3, 7 ), PreFilled 6 )
        , ( ( 4, 7 ), PreFilled 3 )
        , ( ( 5, 7 ), PreFilled 1 )
        , ( ( 6, 7 ), PreFilled 9 )
        , ( ( 7, 7 ), PreFilled 5 )
        , ( ( 8, 7 ), PreFilled 8 )
        , ( ( 9, 7 ), PreFilled 4 )
        , ( ( 1, 8 ), PreFilled 5 )
        , ( ( 2, 8 ), PreFilled 1 )
        , ( ( 3, 8 ), PreFilled 4 )
        , ( ( 4, 8 ), PreFilled 2 )
        , ( ( 5, 8 ), PreFilled 7 )
        , ( ( 6, 8 ), PreFilled 8 )
        , ( ( 7, 8 ), PreFilled 6 )
        , ( ( 8, 8 ), PreFilled 9 )
        , ( ( 9, 8 ), PreFilled 3 )
        , ( ( 1, 9 ), PreFilled 9 )
        , ( ( 2, 9 ), PreFilled 8 )
        , ( ( 3, 9 ), PreFilled 3 )
        , ( ( 4, 9 ), PreFilled 6 )
        , ( ( 5, 9 ), PreFilled 5 )
        , ( ( 6, 9 ), PreFilled 4 )
        , ( ( 7, 9 ), PreFilled 7 )
        , ( ( 8, 9 ), PreFilled 1 )
        , ( ( 9, 9 ), PreFilled 2 )
        ]
