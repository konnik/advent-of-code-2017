module Puzzles.Year2017.Day15 exposing (..)

import AdventOfCode.Puzzle exposing (Puzzle, PuzzleSolver, TestSuite, TestResult)
import Bitwise


puzzle : Puzzle
puzzle = ( 2017, 15, "Dueling Generators", tests, part1, part2 )

tests : TestSuite
tests = 
    [ ( part1 "test-input" == "expected-output",  "Test part 1" )
    , ( part2 "test-input" == "expected-output",  "Test part 2" )
    ]

--Generator A starts with 783
--Generator B starts with 325

type alias Generator a = a -> a
type alias Pair a = (a, a)
type alias Matcher a = (a, a) -> Bool


part1 : PuzzleSolver
part1 input =
    countWith 
        matchingLowest16bits 
        ( pairwise 
            (generatorWithFactor 16807) 
            (generatorWithFactor 48271)
        ) 
        40000000 0 
        (783, 325)
    |> toString

part2 : PuzzleSolver
part2 input = 
    countWith 
        matchingLowest16bits 
        ( pairwise 
            (multipleOf 4 (generatorWithFactor 16807))
            (multipleOf 8 (generatorWithFactor 48271))
        )  
        5000000 0 
        (783, 325)
    |> toString

generatorWithFactor : Int -> Generator Int
generatorWithFactor factor prev = rem (prev * factor) 2147483647

multipleOf  : Int -> Generator Int -> Generator Int
multipleOf n generator prev = 
    let 
        x = generator prev
    in
        if rem x n == 0 then
            x
        else
            multipleOf n generator x

pairwise : Generator Int -> Generator Int -> Generator (Pair Int)
pairwise generatorA generatorB (prevA, prevB) =
    (generatorA prevA, generatorB prevB)

countWith : Matcher a -> Generator (Pair a) -> Int -> Int -> Pair a -> Int
countWith matcher generator iterations counter prev =
    if iterations == 0 then
        counter
    else
        let 
            next = generator prev
            newCount = if matcher next then 
                            counter + 1
                        else
                            counter 
        in
            countWith matcher generator (iterations-1) newCount next
    


matchingLowest16bits : Matcher Int
matchingLowest16bits (a, b) = (Bitwise.and 0xffff a) == (Bitwise.and 0xffff b)
