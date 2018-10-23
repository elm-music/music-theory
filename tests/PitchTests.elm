module PitchTests exposing (all)

import Expect
import MusicTheory.Octave as Octave
import MusicTheory.Pitch as Pitch
import Test exposing (..)


all : Test
all =
    describe "Pitch Tests"
        [ test "dummy" <|
            \_ -> Expect.equal 1 1
        ]
