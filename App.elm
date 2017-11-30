module App exposing (main)

import Html
import AdventOfCode.Main exposing (update, init, subscriptions)
import AdventOfCode.View exposing (view)
import AdventOfCode.Types exposing (Msg, Model)

main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
    }