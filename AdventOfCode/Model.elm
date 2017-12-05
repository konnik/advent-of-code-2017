module AdventOfCode.Model exposing (..)

import AdventOfCode.Puzzle exposing (..)
import RemoteData exposing (WebData,RemoteData(..))
import Time exposing (Time)

type alias Model = { puzzles : List Puzzle
                    , selected : Maybe Puzzle
                    , input : WebData PuzzleInput
                    , answer : Maybe String
                    , showTests : Bool
                    , showPuzzleInput: Bool
                    , showDebug: Bool
                    , time : Maybe Time
                    }
type Msg 
    = NoOp 
    | PuzzleSelected Puzzle
    | OnPuzzleInputFetched Puzzle (WebData PuzzleInput)
    | SolvePuzzle PuzzleSolver PuzzleInput
    | OnPuzzleStart PuzzleSolver PuzzleInput Time
    | OnPuzzleFinish Time Time
    | ToggleTests
    | TogglePuzzleInput
    | ToggleDebug
