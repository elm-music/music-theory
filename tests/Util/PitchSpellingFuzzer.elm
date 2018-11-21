module Util.PitchSpellingFuzzer exposing (pitchSpelling)

import Fuzz exposing (Fuzzer)
import MusicTheory.Letter as Letter exposing (Letter(..))
import MusicTheory.Pitch.Spelling as Spelling exposing (PitchSpelling)
import MusicTheory.PitchClass.Spelling exposing (Accidental(..))
import Util.Fuzzer
import Util.OctaveFuzzer as Octave


pitchSpelling : Fuzzer PitchSpelling
pitchSpelling =
    Fuzz.map3 PitchSpelling
        (Util.Fuzzer.fromList [ C, D, E, F, G, A, B ])
        (Util.Fuzzer.fromList [ Sharp, Flat, Natural ])
        Octave.octave
