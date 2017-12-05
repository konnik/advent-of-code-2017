module App exposing (main)

import Navigation exposing (program)

import AdventOfCode.Main exposing (update, init, subscriptions)
import AdventOfCode.View exposing (view)
import AdventOfCode.Router exposing (router)
import AdventOfCode.Model exposing (Msg, Model)
import AdventOfCode.Puzzle exposing (Puzzle)

import Puzzles.Year2015
import Puzzles.Year2017

allPuzzles : List Puzzle
allPuzzles =
    List.concat
        [ Puzzles.Year2015.puzzles
        , Puzzles.Year2017.puzzles
        ]


main : Program Never Model Msg
main =
    program router
        { init = init allPuzzles
        , view = view
        , update = update
        , subscriptions = subscriptions
    }