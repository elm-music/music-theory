module ScaleTests exposing (all)

import Expect
import Fuzz exposing (Fuzzer)
import MusicTheory.PitchClass as PC exposing (Accidental(..), Letter(..))
import MusicTheory.Scale as Scale
import Test exposing (..)


all : Test
all =
    describe "Scale Tests"
        [ test "C major pentatonic scale should have correct pitch classes" <|
            \_ ->
                Scale.nonDiatonicScale (PC.pitchClass C Natural) Scale.majorPentatonic
                    |> Expect.equal
                        (Debug.todo "don't know what to do here. I get a NonDiatonicScale which is opaque and has no functions that I can use it with.")
        ]
