module Types exposing (..)


type alias Solver = String -> String

type alias TestResult = (Bool, String)
type alias TestRunner = List TestResult