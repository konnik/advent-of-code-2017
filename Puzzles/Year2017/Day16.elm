module Puzzles.Year2017.Day16 exposing (..)

import AdventOfCode.Puzzle exposing (Puzzle, PuzzleSolver, TestSuite, TestResult)

import List.Extra as LE exposing ((!!))
import Dict exposing (Dict)

puzzle : Puzzle
puzzle = ( 2017, 16, "Permutation Promenade", tests, part1, part2 )

tests : TestSuite
tests = []

type Move 
    = Spin Int
    | Exchange Int Int 
    | Partner Char Char
    | Error


initialState : List Char
initialState = String.toList "abcdefghijklmnop"

part1 : PuzzleSolver
part1 input = 
    parseInput input
        |> executeMoves initialState
        |> String.fromList 

part2 : PuzzleSolver
part2 input = 
    parseInput input
        |> loop 0 Dict.empty initialState
        |> String.fromList 

loop : Int -> Dict (List Char) Int -> List Char -> List Move -> List Char 
loop n history input moves = 
    if n == 1000000000 then
        input
    else
        case Dict.get input history of
            Nothing ->
                loop 
                    (n+1) 
                    (Dict.insert input n history)
                    (executeMoves input moves)
                    moves
            Just x -> 
                let 
                    loopLength = n-x
                    stepsLeft = (1000000000 - n) 
                    numLoops = stepsLeft // loopLength
                    nextN = n + (numLoops * loopLength) + 1 
                in
                    loop
                        nextN 
                        Dict.empty
                        (executeMoves input moves)
                        moves


executeMoves : List Char -> List Move -> List Char
executeMoves input moves = List.foldl move input moves

move : Move -> List Char -> List Char
move move list = 
    case move of
        Spin n -> spin n list
        Exchange a b -> exchange a b list
        Partner ac bc -> partner ac bc list 
        Error -> list

spin : Int -> List Char -> List Char
spin n list = 
    let 
        last = List.drop ((List.length list)-n) list 
        first = List.take  ((List.length list)-n) list
    in List.append last first 

exchange : Int -> Int -> List Char -> List Char
exchange a b list = 
    let 
        ac = Maybe.withDefault '.' (list !! a)
        bc = Maybe.withDefault '.' (list !! b)
    in
        LE.setAt b ac list
            |> Maybe.withDefault list 
            |> LE.setAt a bc 
            |> Maybe.withDefault list 

partner : Char -> Char -> List Char -> List Char
partner ac bc list = 
    let     
        a = Maybe.withDefault 0 (LE.elemIndex ac list)
        b = Maybe.withDefault 0 (LE.elemIndex bc list)
    in
        LE.setAt b ac list
            |> Maybe.withDefault list 
            |> LE.setAt a bc 
            |> Maybe.withDefault list 
        

parseInput : String -> List Move
parseInput input =
    String.split "," input
        |> List.map parseMove

parseMove : String -> Move
parseMove str = 
    case String.toList str of
        's'::rest -> Spin (Result.withDefault 0 (String.toInt (String.fromList rest)))
        'x'::rest -> 
            let 
                elems = (String.fromList rest) |> String.split "/" |> List.map toInt
            in 
                Exchange (Maybe.withDefault 0 (elems !! 0)) (Maybe.withDefault 0 (elems !! 1))
        'p'::rest -> Partner (Maybe.withDefault '.' (rest !! 0)) (Maybe.withDefault '.' (rest !! 2))
        _ -> Error

toInt : String -> Int
toInt = Result.withDefault 0 << String.toInt
