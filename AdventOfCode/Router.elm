module AdventOfCode.Router exposing (router)

import Navigation exposing (Location)
import AdventOfCode.Model exposing (Msg, Msg (..))

router : Location -> Msg
router location = OnNavigation location