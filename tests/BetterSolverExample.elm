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
            [ --test "solves a board with one empty field" <|
              --   \_ ->
              --       let
              --           board =
              --               Board.fromNotation ".51897246248561937679423851135782469467935128892146375726319584514278693983654712"
              --       in
              --       Expect.true "Numbers differ" (sameNumbers (BetterSolver.solve board) solvedBoard)
              --, test "solves a board with two empty fields" <|
              --    \_ ->
              --        let
              --            board =
              --                Board.fromNotation "..1897246248561937679423851135782469467935128892146375726319584514278693983654712"
              --        in
              --        Expect.true "Numbers differ" (sameNumbers (BetterSolver.solve board) solvedBoard)
              --, test "solves a board with three empty fields" <|
              --    \_ ->
              --        let
              --            board =
              --                Board.fromNotation "..1897246248561937679423851135782469467935128892146375726319.84514278693983654712"
              --        in
              --        Expect.true "Numbers differ" (sameNumbers (BetterSolver.solve board) solvedBoard)
              --, test "solves a board with five empty fields" <|
              --    \_ ->
              --        let
              --            board =
              --                Board.fromNotation "..189724624856193767942385113578246.467935128892146375726319.8451427869.983654712"
              --        in
              --        Expect.true "Numbers differ"
              --            (sameNumbers (BetterSolver.solve board) solvedBoard)
              --, test "solves a given easy solvable board" <|
              --  \_ ->
              --      let
              --          board =
              --              Board.solvableEasy
              --      in
              --      Expect.true "Numbers differ" (sameNumbers (BetterSolver.solve board) solvedBoard)
              test "solves a given solvable board" <|
                \_ ->
                    let
                        board =
                            Board.solvable
                    in
                    Expect.true "Numbers differ" (sameNumbers (BetterSolver.solve board) solvedBoard)
            ]

        --, describe ".possibleNumbersForIndex"
        --    [ test "returns possible numbers for the given index" <|
        --        \_ ->
        --            let
        --                board =
        --                    Board.fromNotation "..1897246248561937679423851135782469467935128892146375726319584514278693983654712"
        --            in
        --            Expect.equalLists (BetterSolver.possibleNumbersForIndex board 0) [ Three ]
        --    ]
        ]


solvedBoard =
    Board.fromNotation "351897246248561937679423851135782469467935128892146375726319584514278693983654712"


sameNumbers leftBoard rightBoard =
    List.map2
        sameNumber
        (Array.toList leftBoard)
        (Array.toList rightBoard)
        |> List.all identity


sameNumber : FieldState -> FieldState -> Bool
sameNumber leftEntry rightEntry =
    case leftEntry of
        Empty ->
            rightEntry == Empty

        Impossible ->
            rightEntry == Impossible

        UserFilled leftNumber ->
            case rightEntry of
                UserFilled rightNumber ->
                    leftNumber == rightNumber

                PreFilled rightNumber ->
                    leftNumber == rightNumber

                Trying rightNumber _ ->
                    leftNumber == rightNumber

                Impossible ->
                    False

                Empty ->
                    False

        PreFilled leftNumber ->
            case rightEntry of
                UserFilled rightNumber ->
                    leftNumber == rightNumber

                PreFilled rightNumber ->
                    leftNumber == rightNumber

                Trying rightNumber _ ->
                    leftNumber == rightNumber

                Impossible ->
                    False

                Empty ->
                    False

        Trying leftNumber leftOtherNumbers ->
            case rightEntry of
                UserFilled rightNumber ->
                    leftNumber == rightNumber

                PreFilled rightNumber ->
                    leftNumber == rightNumber

                Trying rightNumber rightOtherNumbers ->
                    leftNumber == rightNumber

                Impossible ->
                    False

                Empty ->
                    False
