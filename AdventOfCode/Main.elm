module AdventOfCode.Main exposing (init, update, subscriptions )

import AdventOfCode.Puzzle exposing (..)
import AdventOfCode.Model exposing (..)

import RemoteData exposing (WebData,RemoteData(..))

import Time exposing (Time)
import Task
import Http
import RemoteData exposing (WebData,RemoteData(..))
import Navigation exposing (Location)

init : List Puzzle -> Location -> ( Model, Cmd Msg )
init puzzles location = 
    (
        { puzzles = puzzles
        , selected = Nothing
        , input = NotAsked
        , answer = Nothing
        , showTests = True
        , showPuzzleInput = False
        , showDebug = False
        , useCustomInput = False
        , customInput = ""
        , time = Nothing
        }
        , fetchPuzzleByLocation puzzles location
    )

inputUrl : Int -> Int -> String
inputUrl year day = 
    "inputs/" ++ (toString year) ++ "/day-" ++ (toString day) ++ ".txt"

puzzleFor : Int -> Int -> Puzzle -> Bool
puzzleFor year day (year2, day2, _, _, _, _) = year == year2 && day == day2

fetchPuzzleByLocation : List Puzzle -> Location -> Cmd Msg
fetchPuzzleByLocation puzzles location = 
    case findPuzzleByHash puzzles location.hash of
        Nothing -> Cmd.none
        Just puzzle -> fetchPuzzleInput puzzle

findPuzzleByHash : List Puzzle -> String -> Maybe Puzzle
findPuzzleByHash puzzles hash =
    case String.split "-" (String.dropLeft 1 hash) of
        [year,"day",day] ->
            let
                year2 = Result.withDefault 0 (String.toInt year)
                day2 = Result.withDefault 0 (String.toInt day)
                matchingPuzzles = List.filter (puzzleFor year2 day2) puzzles
            in
                case matchingPuzzles of 
                    [puzzle] -> Just puzzle
                    _ -> Nothing
        _ -> Nothing

fetchPuzzleInput : Puzzle -> Cmd Msg
fetchPuzzleInput puzzle = 
    let 
        (year, day, _, _, _, _) = puzzle
    in
        Http.getString (inputUrl year day)
            |> RemoteData.sendRequest
            |> Cmd.map (OnPuzzleInputFetched puzzle)

update : Msg -> Model -> (Model, Cmd Msg)
update cmd model = 
    case cmd of
        NoOp -> 
            model ! []
        OnNavigation location ->
            model ! [fetchPuzzleByLocation model.puzzles location]
        PuzzleSelected puzzle -> 
            {model | answer = Nothing, selected = Nothing, input = NotAsked } ! [fetchPuzzleInput puzzle]
        OnPuzzleInputFetched puzzle webdata -> 
            {model | input = webdata, selected = Just puzzle} ! []
        SolvePuzzle solver input -> 
            { model | time = Nothing } ! [ Task.perform (OnPuzzleStart solver input) Time.now]
        OnPuzzleStart solver input startTime -> 
            {model | answer = Just (solver input) } ! [Task.perform (OnPuzzleFinish startTime) Time.now]
        OnPuzzleFinish startTime finishTime -> 
            {model | time = Just (finishTime - startTime) } ! []
        ToggleTests -> 
            {model | showTests = not model.showTests } ! []
        TogglePuzzleInput -> 
            {model | showPuzzleInput = not model.showPuzzleInput } ! []
        ToggleDebug -> 
            {model | showDebug = not model.showDebug } ! []
        ToggleCustomInput -> 
            {model | useCustomInput = not model.useCustomInput } ! []
        OnCustomInput text -> 
            {model | customInput = text } ! []
        

subscriptions : Model -> Sub Msg
subscriptions model = 
    Sub.none



