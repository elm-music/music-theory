module KeyTests exposing (all)

import Expect
import Fuzz exposing (Fuzzer)
import MusicTheory.Key as Key
import MusicTheory.PitchClass exposing (Accidental(..), Letter(..), pitchClass)
import MusicTheory.Scale as Scale
import Test exposing (..)


all : Test
all =
    describe "Key Tests"
        [ test "C major key should have correct scale" <|
            \_ ->
                let
                    cMajorScale =
                        Scale.scale (pitchClass C Natural) Scale.major
                in
                Key.major (pitchClass C Natural)
                    |> Key.scale
                    |> Expect.equal cMajorScale
        ]
