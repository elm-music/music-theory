module Util.KeyFuzzer exposing (key)

import Fuzz exposing (Fuzzer)
import MusicTheory.Key as Key exposing (Key)
import Util.Fuzzer
import Util.PitchClassFuzzer


key : Fuzzer Key
key =
    Fuzz.oneOf
        [ Fuzz.map Key.major Util.PitchClassFuzzer.pitchClass
        , Fuzz.map Key.minor Util.PitchClassFuzzer.pitchClass
        ]
