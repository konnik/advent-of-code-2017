module Puzzles.Year2017.Day24 exposing (..)

import AdventOfCode.Puzzle exposing (Puzzle, PuzzleSolver, TestSuite, TestResult)
import List.Extra as LE

puzzle : Puzzle
puzzle = ( 2017, 24, "Puzzle Title", tests, part1, part2 )

tests : TestSuite
tests = 
    [-- ( part1 "test-input" == "expected-output",  "Test part 1" )
    --, ( part2 "test-input" == "expected-output",  "Test part 2" )
    ]

type alias Component = (Int, Int)

part1 : PuzzleSolver
part1 input = 
    let 
        allComponents = parseInput input
    in
        allComponents
            |> startCandidates
            |> List.map (findStrongest allComponents)
            |> maxOf
            |> toString

part2 : PuzzleSolver
part2 input = 
    let 
        allComponents = parseInput input
    in
        allComponents
            |> startCandidates
            |> List.map (findLongest allComponents)
            |> selectLongesBridge
            |> Tuple.second
            |> toString

findStrongest : List Component -> Component -> Int
findStrongest components from = 
    let
        rest = LE.remove (sort from) components

        connectWith : Component -> Int
        connectWith to =  findStrongest rest (flipToMatch from to)

    in
        (strengthOf from) 
        + maxOf (List.map connectWith (candidates from rest))
        
findLongest : List Component  -> Component -> (Int, Int)
findLongest components from = 
    let
        rest = LE.remove (sort from) components
        matchingComponents = candidates from rest

        connectWith : Component -> (Int, Int)
        connectWith to =  findLongest rest (flipToMatch from to)

    in
        if matchingComponents == [] then
            (1, strengthOf from)
        else
            let
                (restLen, restStrength) = 
                    selectLongesBridge (List.map connectWith (candidates from rest))
            in
                (1 + restLen, (strengthOf from) + restStrength)

selectLongesBridge : List (Int, Int) -> (Int, Int) 
selectLongesBridge list = 
    list 
        |> List.sort 
        |> List.reverse 
        |> List.head
        |> Maybe.withDefault (-1,-1)

flipToMatch : Component -> Component -> Component
flipToMatch (a,b) (c,d) = 
    if (b == c) then
        (c,d)
    else 
        (d,c)

sort : Component -> Component
sort (a,b) = 
    if (a <= b) then
        (a,b)
    else 
        (b,a)

startCandidates : List Component -> List Component
startCandidates components = 
    candidates (-1,0) components

candidates : Component -> List Component -> List Component
candidates (_,connectTo) components = 
    components 
        |> List.filter (\ (c,d) -> c == connectTo || d == connectTo)

strengthOf : Component -> Int
strengthOf (a,b) = a + b

parseInput : String -> List Component
parseInput input =
    let 
        parseLine line = 
            case String.split "/" line of 
                [a,b] -> sort (toInt a, toInt b)
                _ -> Debug.log ("Parse error: " ++ line) (-1,-1)
    in
        String.lines input
            |> List.map parseLine
        
toInt : String -> Int
toInt = Result.withDefault 0 << String.toInt

maxOf : List Int -> Int
maxOf = Maybe.withDefault 0 << List.maximum
