module SolverExample exposing (..)

import Dict
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Grid
import Solver
import Test exposing (..)
import Types exposing (..)


suite : Test
suite =
    describe "Solver"
        [ describe ".solve"
            [ test "solves a board with one empty field" <|
                \_ ->
                    let
                        board =
                            Grid.fromNotation ".51897246248561937679423851135782469467935128892146375726319584514278693983654712"

                        solvedBoard =
                            Grid.fromNotation "351897246248561937679423851135782469467935128892146375726319584514278693983654712"
                    in
                    Expect.equal (Solver.solve board) solvedBoard
            , todo "solves a board with two empty fields"

            --<|
            --    \_ ->
            --        let
            --            board =
            --                Grid.fromNotation "..1897246248561937679423851135782469467935128892146375726319584514278693983654712"
            --            solvedBoard =
            --                Grid.fromNotation "351897246248561937679423851135782469467935128892146375726319584514278693983654712"
            --        in
            --        Expect.equal (Solver.solve board) solvedBoard
            ]
        ]
