module Notation.ResultTests exposing (all)

import Expect
import Fuzz exposing (Fuzzer)
import MusicTheory.Duration as Duration exposing (..)
import MusicTheory.Interval as Interval
import MusicTheory.Key as Key
import MusicTheory.Letter exposing (Letter(..))
import MusicTheory.Notation as Notation exposing (..)
import MusicTheory.Notation.Abc as Abc exposing (..)
import MusicTheory.Notation.Result exposing (traverse)
import MusicTheory.Octave exposing (..)
import MusicTheory.Pitch as Pitch exposing (Pitch, flat, natural, pitch, sharp)
import MusicTheory.Pitch.Enharmonic exposing (EnharmonicTransformationError)
import MusicTheory.Pitch.Spelling as Spelling exposing (..)
import MusicTheory.PitchClass as PitchClass
import MusicTheory.PitchClass.Spelling exposing (Accidental(..))
import MusicTheory.TimeSignature as TimeSignature
import MusicTheory.Tuplet as Tuplet
import Test exposing (..)


voicingsTwoFiveOne : Notation Pitch
voicingsTwoFiveOne =
    let
        voice1 =
            bar [ doubleLine ]
                [ chord [] [ pitch A natural four |> chordNote [], pitch F natural four |> chordNote [], pitch C natural four |> chordNote [] ] quarterNote
                , chord [] [ pitch B natural four |> chordNote [], pitch E natural four |> chordNote [], pitch G sharp four |> chordNote [], pitch B natural three |> chordNote [] ] eighthNote
                , chord [ tie ] [ pitch G natural four |> chordNote [], pitch D natural four |> chordNote [], pitch A natural three |> chordNote [] ] eighthNote
                , chord [] [ pitch G natural four |> chordNote [], pitch D natural four |> chordNote [], pitch A natural three |> chordNote [] ] halfNote
                ]

        voice2 =
            bar [ doubleLine ]
                [ chord [] [ pitch G natural three |> chordNote [], pitch D natural three |> chordNote [] ] quarterNote
                , note [] (pitch F natural three) eighthNote
                , note [ tie ] (pitch E natural three) eighthNote
                , note [] (pitch E natural three) halfNote
                ]
    in
    { title = Just "II V I Voicings"
    , composer = Nothing
    , key = Key.major (PitchClass.pitchClass C natural)
    , timeSignature = TimeSignature.timeSignature TimeSignature.Four TimeSignature.Quarter
    , tempo = Nothing
    , staffs = [ staff treble [ voice1 ], staff bass [ voice2 ] ]
    }


all : Test
all =
    describe "Notation Result Tests"
        [ test "voicings II V I test" <|
            \_ ->
                voicingsTwoFiveOne
                    |> traverse Spelling.simple
                    |> Result.map (fromNotation 4)
                    |> Expect.equal (Ok "T: II V I Voicings\nM: 4/4\n%%score (0) (1)\nV: 0 clef=treble\nV: 1 clef=bass\nK: C\n[V:0][=A2/1=F2/1=C] [=B1/1=E1/1^G1/1=B,] [=G1/1=D1/1=A,]- [=G4/1=D4/1=A,] ||\n[V:1][=G,2/1=D,] =F,1/1 =E,1/1- =E,4/1 ||")
        ]
