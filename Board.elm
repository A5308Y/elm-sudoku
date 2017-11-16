module Board exposing (..)

import Array
import Types exposing (..)


empty : Board
empty =
    Array.repeat 81 Empty


fromNotation : String -> Board
fromNotation boardCode =
    boardCode
        |> String.toList
        |> Array.fromList
        |> Array.map charToFieldState


toNotation : Board -> String
toNotation board =
    board
        |> Array.map fieldStateToString
        |> Array.toList
        |> String.concat


solvableWithoutBacktracking : Board
solvableWithoutBacktracking =
    fromNotation
        "..189..4.2.8.619....94.3.51.3..824...67...12.8.21........3.9.8.514........36..712"


solvable : Board
solvable =
    fromNotation
        "....9......8....3767.4..8...3..8246...........9214..7...6..9.8451....6......5...."


solved : Board
solved =
    fromNotation
        "351897246248561937679423851135782469467935128892146375726319584514278693983654712"


solvableEasy : Board
solvableEasy =
    fromNotation
        "...8.1.2..26.9.1......6.7........654.........312........7.1......5.7.89..9.3.2..."


solvableSimple : Board
solvableSimple =
    fromNotation
        ".79.3.65....4.2.......7.....8.....2.2.3...8.5.4.....7.....5.......1.6....91.2.43."


charToFieldState : Char -> FieldState
charToFieldState char =
    case charToNumber char of
        Nothing ->
            Empty

        Just number ->
            PreFilled number


fieldStateToString : FieldState -> String
fieldStateToString state =
    case state of
        UserFilled number ->
            numberToString number

        PreFilled number ->
            numberToString number

        Empty ->
            "."

        Trying number otherNumbers ->
            numberToString number

        Impossible ->
            "x"


charToNumber : Char -> Maybe Number
charToNumber char =
    case char of
        '1' ->
            Just One

        '2' ->
            Just Two

        '3' ->
            Just Three

        '4' ->
            Just Four

        '5' ->
            Just Five

        '6' ->
            Just Six

        '7' ->
            Just Seven

        '8' ->
            Just Eight

        '9' ->
            Just Nine

        _ ->
            Nothing


numberToString : Number -> String
numberToString number =
    case number of
        One ->
            "1"

        Two ->
            "2"

        Three ->
            "3"

        Four ->
            "4"

        Five ->
            "5"

        Six ->
            "6"

        Seven ->
            "7"

        Eight ->
            "8"

        Nine ->
            "9"


errors : Board -> Array.Array Bool
errors board =
    board
        |> Array.indexedMap (indexHasError board)


getFieldState : Board -> Int -> Maybe Number
getFieldState board index =
    case Array.get index board of
        Nothing ->
            Nothing

        Just state ->
            case state of
                UserFilled number ->
                    Just number

                PreFilled number ->
                    Just number

                Trying number _ ->
                    Just number

                Impossible ->
                    Nothing

                Empty ->
                    Nothing


indexHasError : Board -> Int -> FieldState -> Bool
indexHasError board index state =
    case state of
        UserFilled number ->
            board
                |> numbersToCheck index
                |> List.member number

        _ ->
            False


numbersToCheck : Int -> Board -> List Number
numbersToCheck index board =
    board
        |> indexesToCheck index
        |> List.map (getFieldState board)
        |> List.filterMap identity


indexesToCheck : Int -> a -> List Int
indexesToCheck index board =
    (boxIndexes index ++ columnIndexes index ++ rowIndexes index)
        |> List.filter (not << (==) index)


columnIndexes : Int -> List Int
columnIndexes index =
    List.range 0 8
        |> List.map (\count -> count * 9 + rem index 9)


rowIndexes : Int -> List Int
rowIndexes index =
    List.range ((index // 9) * 9) (8 + (index // 9) * 9)


boxIndexes : Int -> List number
boxIndexes index =
    let
        boxStartIndex =
            if List.member (index // 3) [ 0, 3, 6 ] then
                0
            else if List.member (index // 3) [ 1, 4, 7 ] then
                3
            else if List.member (index // 3) [ 2, 5, 8 ] then
                6
            else if List.member (index // 3) [ 9, 12, 15 ] then
                27
            else if List.member (index // 3) [ 10, 13, 16 ] then
                30
            else if List.member (index // 3) [ 11, 14, 17 ] then
                33
            else if List.member (index // 3) [ 18, 21, 24 ] then
                54
            else if List.member (index // 3) [ 19, 22, 25 ] then
                57
            else if List.member (index // 3) [ 20, 23, 26 ] then
                60
            else
                0
    in
    [ boxStartIndex + 0
    , boxStartIndex + 1
    , boxStartIndex + 2
    , boxStartIndex + 9
    , boxStartIndex + 10
    , boxStartIndex + 11
    , boxStartIndex + 18
    , boxStartIndex + 19
    , boxStartIndex + 20
    ]
