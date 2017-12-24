module Puzzles.Year2017.Day24 exposing (..)

import AdventOfCode.Puzzle exposing (Puzzle, PuzzleSolver, TestSuite, TestResult)
import List.Extra as LE exposing (remove, maximumBy)
import Tuple exposing (first, second)

puzzle : Puzzle
puzzle = ( 2017, 24, "Electromagnetic Moat", tests, part1, part2 )

tests : TestSuite
tests = 
    [ ( part1 "0/2\n2/2\n2/3\n3/4\n3/5\n0/1\n10/1\n9/10" == "31",  "Test part 1" )
    , ( part2 "0/2\n2/2\n2/3\n3/4\n3/5\n0/1\n10/1\n9/10" == "19",  "Test part 2" )
    ]

type alias Component = (Int, Int)
type alias Strength = Int
type alias Length = Int
type alias SelectionStrategy = List (Length, Strength) -> (Length, Strength)

part1 : PuzzleSolver
part1 input = 
    parseInput input
        |> findBridgeStartingWith 0 selectStrongest
        |> second
        |> toString

part2 : PuzzleSolver
part2 input = 
    parseInput input
        |> findBridgeStartingWith 0 selectLongest
        |> second
        |> toString


findBridgeStartingWith : Int -> SelectionStrategy -> List Component -> (Length, Strength)
findBridgeStartingWith portType selectionStrategy components = 
    let
        candidates = componentsMatching portType components

        connect : Component -> (Length, Strength)
        connect c =
            let
                (lengthRest, strengthRest) = findBridgeStartingWith (second c) selectionStrategy (remove (normalize c) components)
            in
                (1 + lengthRest, strengthOf c + strengthRest)
    in
        if candidates == [] then
            (0,0)
        else
            candidates
                |> List.map connect 
                |> selectionStrategy

selectStrongest : SelectionStrategy
selectStrongest values = 
    values
        |> maximumBy (second)
        |> Maybe.withDefault (-1,-1)

selectLongest : SelectionStrategy
selectLongest values = 
    values
        |> List.maximum
        |> Maybe.withDefault (-1,-1)


flipToMatch : Int -> Component -> Component
flipToMatch portType (a, b) = 
    if (a == portType) then
        (a,b)
    else 
        (b,a)

normalize : Component -> Component
normalize (a,b) = 
    if (a <= b) then
        (a,b)
    else 
        (b,a)

componentsMatching : Int -> List Component -> List Component
componentsMatching portType components = 
    let
        canConnect (a,b) = a == portType || b == portType
    in
        components
            |> List.filter canConnect
            |> List.map (flipToMatch portType)

strengthOf : Component -> Int
strengthOf (a,b) = a + b

parseInput : String -> List Component
parseInput input =
    let 
        parseLine line = 
            case String.split "/" line of 
                [a,b] -> normalize (toInt a, toInt b)
                _ -> Debug.log ("Parse error: " ++ line) (-1,-1)
    in
        String.lines input
            |> List.map parseLine
        
toInt : String -> Int
toInt = Result.withDefault 0 << String.toInt
