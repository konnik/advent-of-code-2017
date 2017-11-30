module AdventOfCode.Types exposing (..)

import RemoteData exposing (WebData,RemoteData(..))

type alias Id = String
type alias Name = String
type alias Year = Int
type alias Day = Int
type alias Desc = String

type alias Puzzle = (Year, Day, Desc, Solver, TestRunner)
type alias PuzzleInput = String

type alias Solver = String -> String
type alias TestResult = (Bool, String)
type alias TestRunner = List TestResult


type alias Model = { puzzles : List Puzzle,
                     selected : Maybe Puzzle,
                     input : WebData PuzzleInput,
                     answer : Maybe String }
type Msg 
    = NoOp 
    | PuzzleSelected Puzzle
    | OnPuzzleInputFetched Puzzle (WebData PuzzleInput)
    | SolvePuzzle Solver PuzzleInput
