module Puzzles.Year2017.Day1 exposing (..)

import AdventOfCode.Puzzle exposing (Puzzle, PuzzleSolver, TestSuite, TestResult)

puzzle : Puzzle
puzzle = (2017,1,"Inverse Captcha", tests, part1, part2)

tests : TestSuite
tests = [ (part1 "1122" == "3", "Part 1 - 1122 produces a sum of 3 (1 + 2) because the first digit (1) matches the second digit and the third digit (2) matches the fourth digit.")
        , (part1 "1111" == "4", "Part 1 - 1111 produces 4 because each digit (all 1) matches the next")
        , (part1 "1234" == "0", "Part 1 - 1234 produces 0 because no digit matches the next.")
        , (part1 "91212129" == "9", "Part 1 - 91212129 produces 9 because the only digit that matches the next one is the last digit, 9.")
        
        , (part2 "1212" == "6", "Part 2 - 1212 produces 6: the list contains 4 items, and all four digits match the digit 2 items ahead.")
        , (part2 "1221" == "0", "Part 2 - 1221 produces 0, because every comparison is between a 1 and a 2.")
        , (part2 "123425" == "4", "Part 2 - 123425 produces 4, because both 2s match each other, but no other digit has a match.")
        , (part2 "123123" == "12", "Part 2 - 123123 produces 12.")
        , (part2 "12131415" == "4", "Part 2 - 12131415 produces 4.")
        ]

{- 
1212 produces 6: the list contains 4 items, and all four digits match the digit 2 items ahead.
1221 produces 0, because every comparison is between a 1 and a 2.
123425 produces 4, because both 2s match each other, but no other digit has a match.
123123 produces 12.
12131415 produces 4.
-}

part1 : PuzzleSolver
part1 input = 
    input 
        |> parseInput
        |> makePairs 1
        |> sumTwins
        |> toString 

part2 : PuzzleSolver
part2 input =
    input 
        |> parseInput
        |> makePairs ((String.length input)//2)
        |> sumTwins
        |> toString 


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

makePairs : Int -> List a -> List (a,a)
makePairs n list = 
    List.map2 (,) list (rotateLeft n list) 

rotateLeft : Int -> List a -> List a
rotateLeft n list = 
    (List.drop n list) ++ (List.take n list)
