module Puzzles.Year2017.Day20 exposing (..)

import AdventOfCode.Puzzle exposing (Puzzle, PuzzleSolver, TestSuite, TestResult)
import Set exposing (Set)
import List.Extra as LE exposing (groupWhile) 

puzzle : Puzzle
puzzle = ( 2017, 20, "Particle Swarm", tests, part1, part2 )

tests : TestSuite
tests = []

type alias Vector = (Int, Int, Int)
type alias Particle = (Vector, Vector, Vector) 

part1 : PuzzleSolver
part1 input = 
    parseInput input
    |> indexOfParticleWithLeastAcceleration
    |> toString
    
part2 : PuzzleSolver
part2 input =
    parseInput input
    |> integrateNumberOfTimes 200
    |> List.length
    |> toString
 

integrateNumberOfTimes : Int -> List Particle -> List Particle
integrateNumberOfTimes n particles = 
    if n <= 0 then 
        particles 
    else
        integrateNumberOfTimes (n-1) (removeCollisions (List.map integrate particles))

integrate : Particle -> Particle
integrate (p, v, a) = 
    let
        add (x0,y0,z0) (x1,y1,z1) = (x0+x1, y0+y1, z0+z1)

        v2 = add v a
        p2 = add p v2
    in
        (p2, v2, a)

removeCollisions : List Particle -> List Particle
removeCollisions particles = 
    let
        positionOf (p, _,_) = p
        duplicatePositions = duplicates (List.map positionOf particles)
        hasCollided (p,_,_) = Set.member p duplicatePositions
    in
        List.filter (not << hasCollided) particles


indexOfParticleWithLeastAcceleration : List Particle -> Int
indexOfParticleWithLeastAcceleration list = 
    let 
        manhattanDistance (x,y,z) = abs x + abs y + abs z
    in
        list 
            |> List.indexedMap (,)
            |> List.sortBy (\(i, (p, v, a)) -> manhattanDistance a)
            |> List.head 
            |> Maybe.withDefault (-1,((0,0,0),(0,0,0), (0,0,0)))
            |> Tuple.first


parseInput : String -> List Particle
parseInput input = 
    String.lines input 
    |> List.map parseLine

parseLine : String -> Particle
parseLine line = 
    let
        filterChars c = Set.member c (Set.fromList ['<', '>', 'p', 'v', 'a', '='])
        filteredLine =  String.fromList (List.filter (not << filterChars) (String.toList line))
        vector str = 
            case String.split "," str of
                [x,y,z] -> (toInt x, toInt y , toInt z) 
                _ -> Debug.log ("Parse error: " ++ str)   (-999,-999,-999)
    in
        case String.split ", " filteredLine of
            [p, v, a] ->  (vector p, vector v, vector a)
            _ -> Debug.log ("Parse error: " ++ line) ((0,0,0),(0,0,0),(0,0,0))
        
toInt : String -> Int
toInt = Result.withDefault 0 << String.toInt


duplicates : List comparable -> Set comparable
duplicates list = 
    histogram list 
        |> List.filter (\ (_, count) -> count > 1)
        |> List.map Tuple.first
        |> Set.fromList

histogram : List comparable -> List (comparable, Int)
histogram list = 
    let
        frequency : List a -> Maybe (a, Int)
        frequency list = Maybe.map (\ x -> (x, List.length list)) (List.head list)
    in
        List.sort list
            |> LE.group
            |> List.filterMap frequency


