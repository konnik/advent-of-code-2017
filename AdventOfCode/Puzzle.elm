module AdventOfCode.Puzzle exposing (..)

-- public interface needed to implement a 
-- advent of code puzzle

type alias Puzzle = (Year, Day, Desc, TestSuite, PuzzleSolver, PuzzleSolver)

type alias Year = Int
type alias Day = Int
type alias Desc = String

type alias PuzzleInput = String
type alias PuzzleAnswer = String

type alias PuzzleSolver = PuzzleInput -> PuzzleAnswer
type alias TestSuite = List TestResult
type alias TestResult = (Bool, String)


