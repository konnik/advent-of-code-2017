module AdventOfCode.Main exposing (init, update, subscriptions )

import AdventOfCode.Puzzle exposing (..)
import AdventOfCode.Model exposing (..)

import RemoteData exposing (WebData,RemoteData(..))

import Http
import RemoteData exposing (WebData,RemoteData(..))

init : List Puzzle -> ( Model, Cmd Msg )
init puzzles = ({ puzzles = puzzles
        , selected = Nothing
        , input = NotAsked
        , answer = Nothing
        , showTests = True
        , showPuzzleInput = False
        , showDebug = False
        }, Cmd.none)

inputUrl : Int -> Int -> String
inputUrl year day = 
    "inputs/" ++ (toString year) ++ "/day-" ++ (toString day) ++ ".txt"

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
        PuzzleSelected puzzle -> 
            {model | answer = Nothing, selected = Nothing, input = NotAsked } ! [fetchPuzzleInput puzzle]
        OnPuzzleInputFetched puzzle webdata -> 
            {model | input = webdata, selected = Just puzzle} ! []
        SolvePuzzle solver input -> 
            {model | answer = Just (solver input) } ! []
        ToggleTests -> 
            {model | showTests = not model.showTests } ! []
        TogglePuzzleInput -> 
            {model | showPuzzleInput = not model.showPuzzleInput } ! []
        ToggleDebug -> 
            {model | showDebug = not model.showDebug } ! []
        

subscriptions : Model -> Sub Msg
subscriptions model = 
    Sub.none



