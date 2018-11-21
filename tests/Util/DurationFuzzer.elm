module Util.DurationFuzzer exposing (duration)

import Fuzz exposing (Fuzzer)
import MusicTheory.Duration as Duration exposing (Duration)
import Util.Fuzzer


duration : Fuzzer Duration
duration =
    Util.Fuzzer.fromList
        [ Duration.oneHundredTwentyEighthNote
        , Duration.thirtySecondNote
        , Duration.sixteenthNote
        , Duration.quarterNote
        , Duration.halfNote
        , Duration.wholeNote
        , Duration.dotted Duration.oneHundredTwentyEighthNote
        , Duration.dotted Duration.thirtySecondNote
        , Duration.dotted Duration.sixteenthNote
        , Duration.dotted Duration.quarterNote
        , Duration.dotted Duration.halfNote
        , Duration.dotted Duration.wholeNote
        , Duration.doubleDotted Duration.oneHundredTwentyEighthNote
        , Duration.doubleDotted Duration.thirtySecondNote
        , Duration.doubleDotted Duration.sixteenthNote
        , Duration.doubleDotted Duration.quarterNote
        , Duration.doubleDotted Duration.halfNote
        , Duration.doubleDotted Duration.wholeNote
        , Duration.tripleDotted Duration.oneHundredTwentyEighthNote
        , Duration.tripleDotted Duration.thirtySecondNote
        , Duration.tripleDotted Duration.sixteenthNote
        , Duration.tripleDotted Duration.quarterNote
        , Duration.tripleDotted Duration.halfNote
        , Duration.tripleDotted Duration.wholeNote
        ]
