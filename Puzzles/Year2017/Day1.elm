module Puzzles.Year2017.Day1 exposing (..)

import AdventOfCode.Puzzle exposing (Puzzle, PuzzleSolver, TestSuite, TestResult)

puzzle : Puzzle
puzzle = (2017,1,"???", tests, part1, part2)

tests : TestSuite
tests = []

part1 : PuzzleSolver
part1 input = 
    input 
        |> parseInput
        |> makePairs
        |> sumTwins
        |> toString 

part2 : PuzzleSolver
part2 input = "not implemented"


parseInput : String -> List Int 
parseInput input = 
    (String.toList input)
        |> List.map String.fromChar
        |> List.map String.toInt
        |> List.map (Result.withDefault 0)

sumTwins : List (Int,Int) -> Int
sumTwins list = 
    list 
        |> List.filter isTwin
        |> List.map (\(a,_) -> a)
        |> List.sum

isTwin : (Int, Int) -> Bool
isTwin (a,b) = a == b

makePairs : List a -> List (a,a)
makePairs list = 
    List.map2 (,) list (rotateLeft list) 

rotateLeft : List a -> List a
rotateLeft list = 
    (List.drop 1 list) ++ (List.take 1 list)



{- 
1122 produces a sum of 3 (1 + 2) because the first digit (1) matches the second digit and the third digit (2) matches the fourth digit.
1111 produces 4 because each digit (all 1) matches the next.
1234 produces 0 because no digit matches the next.
91212129 produces 9 because the only digit that matches the next one is the last digit, 9.
-}