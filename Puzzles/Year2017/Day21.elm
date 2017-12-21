module Puzzles.Year2017.Day21 exposing (..)

import AdventOfCode.Puzzle exposing (Puzzle, PuzzleSolver, TestSuite, TestResult)
import List.Extra as LE
import Dict exposing (Dict)

puzzle : Puzzle
puzzle = ( 2017, 21, "Fractal Art", tests, part1, part2 )

-- tests

grid : Int -> Grid Int
grid size = List.range 1 size |> List.map (\x -> List.range (x*10+1) (x*10+size))


testRules : Rules 
testRules = parseInput "../.# => ##./#../...\n.#./..#/### => #..#/..../..../#..#"

tests : TestSuite
tests = 
    [ ((join << split << grid) 2 == grid 2, "Split / Join 2x2 grid" )
    , ((join << split << grid) 3 == grid 3, "Split / Join 3x3 grid" )
    , ((join << split << grid) 4 == grid 4, "Split / Join 4x4 grid" )
    , ((join << split << grid) 6 == grid 6, "Split / Join 6x6 grid" )
    , ((join << split << grid) 8 == grid 8, "Split / Join 8x8 grid" )
    ]

-- solution for day 21

type alias Pixel = Char
type alias Grid a = List (List a)
type alias Rules = Dict (Grid Char) (Grid Char)

part1 : PuzzleSolver
part1 input = 
    initialGrid
        |> run 5 (enhanceWithRules (parseInput input))
        |> countPixels
        |> toString

part2 : PuzzleSolver
part2 input = 
    initialGrid
        |> run 18 (enhanceWithRules (parseInput input))
        |> countPixels
        |> toString

initialGrid : Grid Char
initialGrid = [['.', '#', '.'], ['.', '.', '#'], ['#', '#', '#']]

run : Int -> (Grid a -> Grid a) -> Grid a -> Grid a
run n enhance grid = 
    if n == 0 then
        grid
    else
        run (n-1) enhance (enhance grid)

enhanceWithRules : Rules -> Grid Char -> Grid Char
enhanceWithRules rules grid = 
    let
        enhance g = 
            case Dict.get g rules of
                Just x -> x 
                Nothing -> Debug.log ("No match for pattern ") g
    in
        grid
           |> split
           |> List.map (List.map enhance)
           |> join

countPixels : Grid Char -> Int
countPixels grid = 
    grid
        |> List.concat
        |> List.filter (\x -> x=='#') 
        |> List.length

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

join : Grid (Grid a) -> Grid a
join grid =  
    grid
        |> LE.transpose 
        |> List.map (List.concat) 
        |> LE.transpose 
        |> List.map (List.concat)


split : Grid a -> Grid (Grid a)
split grid = 
    let 
        targetSize = 
            if (List.length grid) % 2 == 0 then 
                2
            else
                3
    in
        grid
            |> List.map (LE.groupsOf targetSize) 
            |> LE.transpose 
            |> List.map (LE.groupsOf targetSize) 
            |> LE.transpose

permutations : Grid a -> List (Grid a)
permutations grid = 
    [ grid -- 0 
    , (transpose >> flipv) grid -- 90 left
    , (transpose >> fliph) grid -- 90 right 
    , (flipv >> fliph) grid -- 180
    , fliph grid -- flipped 0 
    , (fliph >> transpose >> flipv) grid -- flipped 90 left
    , (fliph >> transpose >> fliph) grid -- flipped 90 right 
    , (fliph >> flipv >> fliph) grid -- flipped 180 
    ]

transpose : Grid a -> Grid a
transpose = LE.transpose

flipv : Grid a -> Grid a
flipv = List.reverse

fliph : Grid a -> Grid a
fliph = List.map List.reverse


