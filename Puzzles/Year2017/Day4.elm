module Puzzles.Year2017.Day4 exposing (..)

import AdventOfCode.Puzzle exposing (Puzzle, PuzzleSolver, TestSuite, TestResult)

import String exposing (lines)
import Set exposing (Set)

puzzle : Puzzle
puzzle = (2017,4,"High-Entropy Passphrases", tests, part1, part2)

tests : TestSuite
tests = [ (part1 "aa bb cc dd ee" == "1",  "Part 1 - a")
        , (part1 "aa bb cc dd aa" == "0",  "Part 1 - b")
        , (part1 "aa bb cc dd aaa" == "1",  "Part 1 - c")
        , (part2 "abcde fghij" == "1",  "Part 2 - a")
        , (part2 "abcde xyz ecdab" == "0",  "Part 2 - b")
        , (part2 "a ab abc abd abf abj" == "1",  "Part 2 - c")
        , (part2 "iiii oiii ooii oooi oooo" == "1",  "Part 2 - d")
        , (part2 "oiii ioii iioi iiio" == "0",  "Part 2 - e")
        ]

part1 : PuzzleSolver
part1 input = count (lines input) withoutDuplicates |> toString

part2 : PuzzleSolver
part2 input = count (lines input) withoutAnagrams |> toString

count : List String -> (List String -> Bool) -> Int
count lines predicate = 
    lines 
        |> List.map String.words
        |> List.filter predicate 
        |> List.length

withoutDuplicates : List String -> Bool
withoutDuplicates list =
        Set.size (Set.fromList list) == (List.length list)

withoutAnagrams : List String -> Bool
withoutAnagrams line = 
        line
        |> List.map (String.split "") 
        |> List.map List.sort
        |> List.map String.concat
        |> withoutDuplicates
