module Util.TupletFuzzer exposing (tuplet)

import Fuzz exposing (Fuzzer)
import MusicTheory.Tuplet exposing (..)
import Util.Fuzzer


tuplet : Fuzzer Tuplet
tuplet =
    Util.Fuzzer.fromList
        [ duplet
        , triplet
        , quadruplet
        , quintuplet
        , quintupletOverFour
        , quintupletOverThree
        , sextuplet
        , sextupletOverFour
        ]
