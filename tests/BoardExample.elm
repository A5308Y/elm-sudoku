module BoardExample exposing (..)

import Array
import Board
import Expect exposing (Expectation)
import Test exposing (..)
import Types exposing (..)


suite : Test
suite =
    describe "Board"
        [ describe ".loadRandom"
            [ test "converts [ (0, 1) ] to a board with a One in the first position" <|
                \_ ->
                    let
                        board =
                            Board.fromNotation "1................................................................................"
                    in
                    Expect.equal (Board.loadRandom [ ( 0, 1 ) ]) board
            , test "converts [ (0, 1), (1, 3) ] to a board with a One in the first position and a Three in the second postion" <|
                \_ ->
                    let
                        board =
                            Board.fromNotation "13..............................................................................."
                    in
                    Expect.equal (Board.loadRandom [ ( 0, 1 ), ( 1, 3 ) ]) board
            ]
        ]
