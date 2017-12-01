module Puzzles.Year2015.Day2 exposing (..)

import AdventOfCode.Puzzle exposing (Puzzle, PuzzleSolver, TestSuite, TestResult)


puzzle : Puzzle
puzzle = (2015,2,"I Was Told There Would Be No Math", tests, part1, part2)

tests : TestSuite
tests = [ (paperAmountFor  "1x1x10" == 43,  "paper - 1x1x10")
        , (paperAmountFor  "2x3x4" == 58,   "paper - 2x3x4")
        , (ribbonAmountFor "2x3x4" == 34,   "ribbon - 2x3x4")
        , (ribbonAmountFor  "1x1x10" == 14,  "ribbon - 1x1x10")
        ]

type alias Box = (Int, Int, Int)
type alias Sides = (Int, Int, Int)
type alias Perimeter = (Int, Int, Int)


part1 : PuzzleSolver
part1 input = 
    String.lines input 
        |> List.map paperAmountFor 
        |> List.sum 
        |> toString

part2 : PuzzleSolver
part2 input = 
    String.lines input 
        |> List.map ribbonAmountFor
        |> List.sum 
        |> toString

int : String -> Int
int = (Result.withDefault 0) << String.toInt

paperAmountFor : String -> Int
paperAmountFor boxString = 
    boxString
        |> parseBox
        |> sideAreas
        |> paperArea

paperArea : Sides -> Int
paperArea (a, b, c) = 
    2*(a+b+c) + (min a (min b c))

parseBox : String -> Box
parseBox str = case String.split "x" str of
            [a,b,c] -> (int a, int b, int c)
            _ -> (0,0,0)

sideAreas : Box -> Sides
sideAreas (a,b,c) = (a*b, a*c, b*c)

ribbonAmountFor : String -> Int
ribbonAmountFor boxString = 
    boxString
        |> parseBox
        |> ribbonLength

sidePerimeters : Box -> Perimeter  
sidePerimeters (a,b,c) = (2*(a+b), 2*(a+c), 2*(b+c))

ribbonLength : Box -> Int
ribbonLength box = 
    (minPerimeter (sidePerimeters box)) + (bowLength box)

bowLength : Box -> Int
bowLength (a,b,c) = a*b*c

minPerimeter : Perimeter -> Int
minPerimeter (a,b,c) = (min a (min b c))
