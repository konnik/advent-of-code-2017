module Puzzles.Year2017.Day21 exposing (..)

import AdventOfCode.Puzzle exposing (Puzzle, PuzzleSolver, TestSuite, TestResult)
import List.Extra as LE

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

part1 : PuzzleSolver
part1 input = 
    "not implemented"

part2 : PuzzleSolver
part2 input = 
    "not implemented"


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
        targetSize = 2 + (List.length grid % 2) 
        splitForSize n = List.concat << (List.map (List.map LE.transpose << LE.groupsOf n << LE.transpose)) << LE.groupsOf n
    in
        splitForSize targetSize grid
    