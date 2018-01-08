module SolverExample exposing (..)

import Array
import Board
import Dict
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Solver
import Test exposing (..)
import Types exposing (..)


suite : Test
suite =
    describe "Solver"
        [ describe ".possibleNumbersForIndex"
            [ test "returns possible numbers for the given index" <|
                \_ ->
                    let
                        board =
                            Board.fromNotation "..1897246248561937679423851135782469467935128892146375726319584514278693983654712"
                    in
                    Expect.equalLists (Solver.possibleNumbersForIndex board 0) [ Three ]
            ]
        , describe ".solve"
            [ test "Solver solves a board with one empty field" <|
                \_ ->
                    let
                        board =
                            Board.fromNotation ".51897246248561937679423851135782469467935128892146375726319584514278693983654712"
                    in
                    Expect.true "Numbers differ" (sameNumbers (Solver.solve board) solvedBoard)
            , test "Solver solves a board with two empty fields" <|
                \_ ->
                    let
                        board =
                            Board.fromNotation "..1897246248561937679423851135782469467935128892146375726319584514278693983654712"
                    in
                    Expect.true "Numbers differ" (sameNumbers (Solver.solve board) solvedBoard)
            , test "Solver solves a board with three empty fields" <|
                \_ ->
                    let
                        board =
                            Board.fromNotation "..1897246248561937679423851135782469467935128892146375726319.84514278693983654712"
                    in
                    Expect.true "Numbers differ" (sameNumbers (Solver.solve board) solvedBoard)
            , test "Solver solves a board with five empty fields" <|
                \_ ->
                    let
                        board =
                            Board.fromNotation "..189724624856193767942385113578246.467935128892146375726319.8451427869.983654712"
                    in
                    Expect.true "Numbers differ" (sameNumbers (Solver.solve board) solvedBoard)
            , test "Solver solves a given board that's solvable without backtracking" <|
                \_ ->
                    let
                        board =
                            Board.fromNotation "..189..4.2.8.619....94.3.51.3..824...67...12.8.21........3.9.8.514........36..712"
                    in
                    Expect.true "Numbers differ" (sameNumbers (Solver.solve board) solvedBoard)
            , test "Solver solves a board with thirty-five empty fields that needs backtracking" <|
                \_ ->
                    let
                        board =
                            Board.fromNotation ".......4624.56.......4.38..13.7.246.46793.128.9214.3.572.319.845.427.69.98.6.4..."
                    in
                    Expect.true "Numbers differ" (sameNumbers (Solver.solve board) solvedBoard)
            , test "Solver solves a board with forty empty fields that needs backtracking" <|
                \_ ->
                    let
                        board =
                            Board.fromNotation ".......462..56.......4.38..13.7.246.4..93.128.9214.3.572.31..845.42..69.98.6.4..."
                    in
                    Expect.true "Numbers differ" (sameNumbers (Solver.solve board) solvedBoard)
            , test "Solver solves a board with forty-five empty fields that needs backtracking" <|
                \_ ->
                    let
                        board =
                            Board.fromNotation ".......462..56.......4.38..13.7.24..4..93.128.9..4.3.5.2.31..8.5.42..69.98.6.4..."
                    in
                    Expect.true "Numbers differ" (sameNumbers (Solver.solve board) solvedBoard)
            , test "Solver solves a board with forty-six empty fields that needs backtracking" <|
                \_ ->
                    let
                        board =
                            Board.fromNotation ".......462..5........4.38..13.7.24..4..93.128.9..4.3.5.2.31..8.5.42..69.98.6.4..."
                    in
                    Expect.true "Numbers differ" (sameNumbers (Solver.solve board) solvedBoard)
            , test "Solver solves a board with forty-seven empty fields that needs backtracking" <|
                \_ ->
                    let
                        board =
                            Board.fromNotation ".......462..5........4.38..13.7.24..4..93.128.9..4.3.5.2.31..8.5.42..69..8.6.4..."
                    in
                    Expect.true "Numbers differ" (sameNumbers (Solver.solve board) solvedBoard)
            , test "Solver solves a board with forty-eight empty fields that needs backtracking" <|
                \_ ->
                    let
                        board =
                            Board.fromNotation ".......462..5........4.38..13.7.24..4..93.12..9..4.3.5.2.31..8.5.42..69..8.6.4..."
                    in
                    Expect.true "Numbers differ" (sameNumbers (Solver.solve board) solvedBoard)
            , test "solves a given solvable board from https://www.sudoku-solutions.com" <|
                \_ ->
                    let
                        board =
                            Board.fromNotation "....9......8....3767.4..8...3..8246...........9214..7...6..9.8451....6......5...."
                    in
                    Expect.true "Numbers differ" (sameNumbers (Solver.solve board) solvedBoard)
            , test "solves a given easy solvable board from https://www.sudoku-solutions.com" <|
                \_ ->
                    let
                        board =
                            Board.easy
                    in
                    Expect.true
                        "Numbers differ"
                        (sameNumbers
                            (Solver.solve board)
                            (Board.fromNotation "749831526526497183183265749978123654654789312312546978467918235235674891891352467")
                        )
            , test "solves a second given easy solvable board from https://www.sudoku-solutions.com" <|
                \_ ->
                    let
                        board =
                            Board.fromNotation
                                ".........8327.6..9....23.8..152..9..9...6...2.28...54..6.83....2..4....7...6....4"
                    in
                    Expect.true
                        "Numbers differ"
                        (sameNumbers
                            (Solver.solve board)
                            (Board.fromNotation "596184723832756419174923685615247938943568172728391546467839251289415367351672894")
                        )
            , test "solves a given simple solvable board from https://www.sudoku-solutions.com" <|
                \_ ->
                    let
                        board =
                            Board.simple
                    in
                    Expect.true "Numbers differ"
                        (sameNumbers
                            (Solver.solve board)
                            (Board.fromNotation "479831652158462793632579148785613924213947865946285371867354219324196587591728436")
                        )
            , test "solves a given medium solvable board from https://www.sudoku-solutions.com" <|
                \_ ->
                    let
                        board =
                            Board.fromNotation "94.1.3..2.5....6432.37...1...98....4.2.....6.6....72...1...94....5....2.8..6.1.5."
                    in
                    Expect.true "Numbers differ"
                        (sameNumbers (Solver.solve board)
                            (Board.fromNotation "946153872157298643283746519579862134421935768638417295312579486765384921894621357")
                        )
            ]
        ]


solvedBoard : Board
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
