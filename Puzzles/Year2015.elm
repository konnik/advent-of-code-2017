module Puzzles.Year2015 exposing (puzzles)

import AdventOfCode.Puzzle exposing (Puzzle)

import Puzzles.Year2015.Day1
import Puzzles.Year2015.Day2
import Puzzles.Year2015.Day3
import Puzzles.Year2015.Day7

puzzles : List Puzzle
puzzles =
    [ Puzzles.Year2015.Day1.puzzle 
    , Puzzles.Year2015.Day2.puzzle 
    , Puzzles.Year2015.Day3.puzzle 
    , Puzzles.Year2015.Day7.puzzle 
    ]

