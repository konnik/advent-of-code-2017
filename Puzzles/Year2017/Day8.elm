module Puzzles.Year2017.Day8 exposing (..)

import AdventOfCode.Puzzle exposing (Puzzle, PuzzleSolver, TestSuite, TestResult)

import Dict exposing (Dict) 

puzzle : Puzzle
puzzle = ( 2017, 8, "I Heard You Like Registers", tests, part1, part2 )

tests : TestSuite
tests = 
    [ ( part1 testInput == "1",  "Test part 1" )
    , ( part2 testInput == "10",  "Test part 2" )
    ]

testInput : String
testInput = """b inc 5 if a > 1
a inc 1 if b < 5
c dec -10 if a >= 1
c inc -20 if c == 10"""

type alias ModifyOp = Int -> Int -> Int
type alias CompareOp = Int -> Int -> Bool
type alias Register = String
type alias Value = Int
type alias Line = (Register, ModifyOp, Value, Register, CompareOp, Value )
type alias Registers = Dict String Int

part1 : PuzzleSolver
part1 input = 
    let 
        (max, registers) = parseInput input
            |> List.foldl execLine (0,Dict.empty)
    in 
        registers
            |> List.maximum << Dict.values
            |> Maybe.withDefault 0 
            |> toString

part2 : PuzzleSolver
part2 input = 
    let 
        (max, registers) = parseInput input
            |> List.foldl execLine (0,Dict.empty)
    in 
        max
            |> toString


registers : (Int, Registers) -> Registers
registers (_, reg) = reg

execLine : Line -> (Int, Registers) -> (Int, Registers)
execLine line (maxReg, registers) = 
    let
        (reg, modifyOp, value, compReg, compOp, compVal) = line
    in
        if compOp (getReg compReg registers) compVal then
            let
                updatedRegisters = setReg reg (modifyOp (getReg reg registers) value) registers
            in
                (max maxReg (getReg reg updatedRegisters), updatedRegisters)
        else
            (maxReg, registers)

modifyOp : String -> ModifyOp
modifyOp op = 
    case op of
        "inc" -> (+)
        "dec" -> (-)
        _ -> Debug.crash ("Illegal op: " ++ op)


compareOp : String -> CompareOp
compareOp op = 
    case op of
        ">" -> (>)
        "<" -> (<)
        ">=" -> (>=)
        "<=" -> (<=)
        "==" -> (==)
        "!=" -> (/=)
        _ -> Debug.crash ("Illegal comparison op: " ++ op)

parseInput : String -> List Line
parseInput input = 
    String.lines input
        |> List.map parseLine


parseLine : String -> Line
parseLine line =
    -- Example: njb dec 612 if s > -8
    case String.split " " line of
        [a,b,c,_,e,f,g] -> (a, modifyOp b, toInt c, e, compareOp f, toInt g)
        _ -> Debug.crash ("Parse error: " ++ line)

toInt : String -> Int
toInt = Result.withDefault 0 << String.toInt


setReg : String  -> Int -> Registers -> Registers
setReg = Dict.insert

getReg : String -> Registers -> Int
getReg name registers = 
    case Dict.get name registers of
        Nothing -> 0
        Just val -> val

