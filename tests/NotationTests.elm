module NotationTests exposing (all)

import Expect
import Fuzz exposing (Fuzzer)
import MusicTheory.Duration as Duration exposing (..)
import MusicTheory.Key as Key
import MusicTheory.Letter exposing (Letter(..))
import MusicTheory.Notation as Notation exposing (..)
import MusicTheory.Octave exposing (..)
import MusicTheory.Pitch as Pitch exposing (Pitch, flat, natural, pitch, sharp)
import MusicTheory.Pitch.Spelling as Spelling exposing (..)
import MusicTheory.PitchClass as PitchClass
import MusicTheory.PitchClass.Spelling exposing (Accidental(..))
import MusicTheory.TimeSignature as TimeSig
import MusicTheory.Tuplet as Tuplet
import Test exposing (..)


all : Test
all =
    describe "Notation Tests"
        [ test "triplet qn en mixed has correct duration" <|
            \_ ->
                tuplet Tuplet.triplet
                    [ note [] (cNatural four) quarterNote
                    , note [] (cNatural four) quarterNote
                    , note [] (cNatural four) eighthNote
                    , note [] (cNatural four) eighthNote
                    ]
                    |> voiceDuration
                    |> Expect.equal (halfNote |> Duration.toRational)
        , test "5 over 4 has correct duration" <|
            \_ ->
                tuplet Tuplet.quintupletOverFour
                    [ note [] (cNatural four) quarterNote
                    , note [] (cNatural four) quarterNote
                    , note [] (cNatural four) quarterNote
                    , note [] (cNatural four) quarterNote
                    , note [] (cNatural four) quarterNote
                    ]
                    |> voiceDuration
                    |> Expect.equal (wholeNote |> Duration.toRational)
        , test "nested tuplets has correct duration" <|
            \_ ->
                tuplet Tuplet.quintupletOverFour
                    [ note [] (cNatural four) quarterNote
                    , note [] (cNatural four) quarterNote
                    , note [] (cNatural four) quarterNote
                    , tuplet Tuplet.triplet
                        [ note [] (cNatural four) quarterNote
                        , note [] (cNatural four) quarterNote
                        , note [] (cNatural four) quarterNote
                        ]
                    ]
                    |> voiceDuration
                    |> Expect.equal (wholeNote |> Duration.toRational)
        ]
