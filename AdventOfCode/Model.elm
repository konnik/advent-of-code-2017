module AdventOfCode.Model exposing (..)

import AdventOfCode.Puzzle exposing (..)
import RemoteData exposing (WebData,RemoteData(..))

type alias Model = { puzzles : List Puzzle,
                     selected : Maybe Puzzle,
                     input : WebData PuzzleInput,
                     answer : Maybe String }
type Msg 
    = NoOp 
    | PuzzleSelected Puzzle
    | OnPuzzleInputFetched Puzzle (WebData PuzzleInput)
    | SolvePuzzle PuzzleSolver PuzzleInput
