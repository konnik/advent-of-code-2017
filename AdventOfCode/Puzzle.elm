module AdventOfCode.Puzzle exposing (..)

-- public interface needed to implement a 
-- advent of code puzzle

type alias Puzzle = (Year, Day, Desc, PuzzleSolver, TestSuite)

type alias Year = Int
type alias Day = Int
type alias Desc = String

type alias PuzzleInput = String

type alias PuzzleSolver = String -> String
type alias TestSuite = List TestResult
type alias TestResult = (Bool, String)


