module Puzzles.Year2017.Day21 exposing (..)

import AdventOfCode.Puzzle exposing (Puzzle, PuzzleSolver, TestSuite, TestResult)
import List.Extra as LE
import Dict exposing (Dict)

puzzle : Puzzle
puzzle = ( 2017, 21, "Fractal Art", tests, part1, part2 )


testGrid = [[1,2,3,4],[5,6,7,8],[9,10,11,12],[13,14,15,16]]
testGridSplitted = [
    [
        [1,2],
        [5,6]
    ],
    [
        [3,4],
        [7,8]
    ],
    [
        [9,10],
        [13,14]
    ],
    [
        [11,12],
        [15,16]
    ]]

grid6x6 = List.range 1 6 |> List.map (\x -> List.range (x*10+1) (x*10+6))

tests : TestSuite
tests = 
    [
      ( split testGrid == testGridSplitted,  "Split grid 4x4" )
    , ( split [[1,2],[3,4]] == [[[1,2],[3,4]]],  "Split grid 2x2" )
    , ( split [[1,2,3],[4,5,6],[7,8,9]] == [[[1,2,3],[4,5,6],[7,8,9]]],  "Split grid 3x3" )
    , ( part2 "test-input" == "expected-output",  "Test part 2" )
    ]

type alias Pixel = Char
type alias Grid a = List (List a)
type alias Rules = Dict (Grid Char) (Grid Char)

part1 : PuzzleSolver
part1 input = 
    let 
        enhance = next (parseInput input)
    in
        initialGrid
            |> enhance |> Debug.log "grid" 
            |> enhance  |> Debug.log "grid" 
            |> enhance  |> Debug.log "grid" 
            |> enhance  |> Debug.log "grid" 
            |> enhance  |> Debug.log "grid" 
            |> List.concat  |> List.filter (\x -> x=='#') |> List.length
            |> toString

part2 : PuzzleSolver
part2 input = 
    "not implemented"

initialGrid : Grid Char
initialGrid = [['.', '#', '.'], ['.', '.', '#'], ['#', '#', '#']]

testRules : Rules 
testRules = parseInput "../.# => ##./#../...\n.#./..#/### => #..#/..../..../#..#"

next : Rules -> Grid Char -> Grid Char
next rules grid = 
    let
        enhance g = 
            case Dict.get g rules of
                Just x -> x 
                Nothing -> Debug.log ("No match for pattern ") g
    in
        grid
            |> split
            |> List.map enhance
            |> join


parseInput : String -> Rules
parseInput input = 
    String.lines input 
        |> List.map parseLine
        |> List.concat
        |> Dict.fromList

parseLine : String -> List (Grid Char, Grid Char)
parseLine line = 
    let 
        parseGrid x = List.map (String.toList) (String.split "/" x)

        (left, right) = 
            case String.split " => " line of
                [a,b] -> (parseGrid a, parseGrid b)
                _ -> Debug.log ("Parse error: " ++ line) ([[]],[[]])
    in
        permutations left 
            |> List.map (\ x -> (x,right))

permutations : Grid a -> List (Grid a)
permutations grid = 
    [ grid
    , (transpose >> flipv) grid
    , (transpose >> fliph) grid
    , (flipv >> fliph) grid
    , flipv grid
    , (flipv >> transpose >> flipv) grid
    , (flipv >> transpose >> fliph) grid
    , (flipv >> flipv >> fliph) grid
    ]

transpose : Grid a -> Grid a
transpose = LE.transpose

flipv : Grid a -> Grid a
flipv = List.reverse

fliph : Grid a -> Grid a
fliph = List.map List.reverse

join : List (Grid a) -> Grid a
join gridList =  
    let
        gridsPerSide = round(sqrt (toFloat (List.length gridList)))
    in
        gridList 
            |> List.map (LE.groupsOf gridsPerSide) 
            |> List.map List.concat 
            |> LE.transpose 
            |> List.map (LE.groupsOf gridsPerSide) 
            |> List.map (List.map List.concat) 
            |> LE.transpose 
            |> List.concat


split : Grid a -> List (Grid a)
split grid = 
    let 
        targetSize = 
            if (List.length grid) % 3 == 0 then 
                3
            else
                2
        splitForSize n = List.concat << (List.map (List.map LE.transpose << LE.groupsOf n << LE.transpose)) << LE.groupsOf n
    in
        splitForSize targetSize grid
    