module BetterSolverExample exposing (..)

import Array
import BetterSolver
import Board
import Dict
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Types exposing (..)


suite : Test
suite =
    describe "BetterSolver"
        [ describe ".solve"
            [ test "solves a board with one empty field" <|
                \_ ->
                    let
                        board =
                            Board.fromNotation ".51897246248561937679423851135782469467935128892146375726319584514278693983654712"
                    in
                    Expect.true "Numbers differ" (sameNumbers (BetterSolver.solve board) solvedBoard)
            , test "solves a board with two empty fields" <|
                \_ ->
                    let
                        board =
                            Board.fromNotation "..1897246248561937679423851135782469467935128892146375726319584514278693983654712"
                    in
                    Expect.true "Numbers differ" (sameNumbers (BetterSolver.solve board) solvedBoard)
            , test "solves a board with three empty fields" <|
                \_ ->
                    let
                        board =
                            Board.fromNotation "..1897246248561937679423851135782469467935128892146375726319.84514278693983654712"
                    in
                    Expect.true "Numbers differ" (sameNumbers (BetterSolver.solve board) solvedBoard)
            , test "solves a board with five empty fields" <|
                \_ ->
                    let
                        board =
                            Board.fromNotation "..189724624856193767942385113578246.467935128892146375726319.8451427869.983654712"
                    in
                    Expect.true "Numbers differ" (sameNumbers (BetterSolver.solve board) solvedBoard)
            , test "solves a given easy solvable board" <|
                \_ ->
                    let
                        board =
                            Board.solvableEasy
                    in
                    Expect.true "Numbers differ" (sameNumbers (BetterSolver.solve board) solvedBoard)
            , test "solves a given solvable board" <|
                \_ ->
                    let
                        board =
                            Board.solvable
                    in
                    Expect.true "Numbers differ" (sameNumbers (BetterSolver.solve board) solvedBoard)
            ]
        ]


solvedBoard =
    Board.fromNotation "351897246248561937679423851135782469467935128892146375726319584514278693983654712"


sameNumbers leftBoard rightBoard =
    List.map2
        sameNumber
        (Array.toList leftBoard)
        (Array.toList rightBoard)
        |> List.all identity


sameNumber leftEntry rightEntry =
    case leftEntry of
        Empty ->
            rightEntry == Empty

        UserFilled number ->
            rightEntry == UserFilled number || rightEntry == PreFilled number

        PreFilled number ->
            rightEntry == UserFilled number || rightEntry == PreFilled number
