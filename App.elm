module App exposing (main)

import Html
import AdventOfCode.Main exposing (update, init, subscriptions)
import AdventOfCode.View exposing (view)
import AdventOfCode.Model exposing (Msg, Model)
import AdventOfCode.Puzzle exposing (Puzzle)

import Puzzles.Year2015.Day1
import Puzzles.Year2015.Day2
import Puzzles.Year2015.Day3
import Puzzles.Year2015.Day7

import Puzzles.Year2017.Day1
import Puzzles.Year2017.Day2
import Puzzles.Year2017.Day3

allPuzzles : List Puzzle
allPuzzles = [ Puzzles.Year2015.Day1.puzzle
             , Puzzles.Year2015.Day2.puzzle
             , Puzzles.Year2015.Day3.puzzle
             , Puzzles.Year2015.Day7.puzzle
             , Puzzles.Year2017.Day1.puzzle 
             , Puzzles.Year2017.Day2.puzzle 
             , Puzzles.Year2017.Day3.puzzle 
             ]

main : Program Never Model Msg
main =
    Html.program
        { init = init allPuzzles
        , view = view
        , update = update
        , subscriptions = subscriptions
    }