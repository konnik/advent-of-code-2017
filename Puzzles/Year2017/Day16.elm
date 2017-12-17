module Puzzles.Year2017.Day16 exposing (..)

import AdventOfCode.Puzzle exposing (Puzzle, PuzzleSolver, TestSuite, TestResult)

import List.Extra as LE exposing ((!!))
import Dict exposing (Dict)
import Array exposing (Array)

puzzle : Puzzle
puzzle = ( 2017, 16, "Permutation Promenade", tests, part1, part2 )

tests : TestSuite
tests = []

type Move 
    = Spin Int
    | Exchange Int Int 
    | Partner Char Char
    | Error

type alias State = Array Char

initialState : Array Char
initialState = Array.fromList (String.toList "abcdefghijklmnop")

part1 : PuzzleSolver
part1 input = 
    parseInput input
        |> executeMoves initialState
        |> String.fromList << Array.toList

part2 : PuzzleSolver
part2 input = 
    parseInput input
        |> loop 0 Dict.empty initialState
        |> String.fromList << Array.toList

loop : Int -> Dict String Int -> Array Char -> List Move -> Array Char 
loop n history input moves = 
    if n == 1000000000 then
        input
    else
        case Dict.get (String.fromList (Array.toList input)) history of
            Nothing ->
                loop 
                    (n+1) 
                    (Dict.insert (String.fromList (Array.toList input)) n history)
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


executeMoves : Array Char -> List Move -> Array Char
executeMoves input moves = List.foldl move input moves

move : Move -> Array Char -> Array Char
move move list = 
    case move of
        Spin n -> spin n list
        Exchange a b -> exchange a b list
        Partner ac bc -> partner ac bc list 
        Error -> list

spin : Int -> Array Char -> Array Char
spin n list = 
    let 
        last = Array.slice -n (Array.length list) list
        first = Array.slice 0 -n list
    in Array.append last first 

exchange : Int -> Int -> Array Char -> Array Char
exchange a b list = 
    let 
        ac = Maybe.withDefault '.' (Array.get a list)
        bc = Maybe.withDefault '.' (Array.get b list)
    in
        Array.set b ac list |> Array.set a bc

partner : Char -> Char -> Array Char -> Array Char
partner ac bc list = 
    Array.map (\ x -> 
        if x == ac then bc
        else if x == bc then ac
        else x
    ) list

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
