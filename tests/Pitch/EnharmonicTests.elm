module EnharmonicTests exposing (all)

import Expect
import Fuzz exposing (Fuzzer)
import MusicTheory.Internal.Pitch as Internal exposing (PitchError(..))
import MusicTheory.Internal.PitchClass as PitchClass
import MusicTheory.Letter exposing (Letter(..))
import MusicTheory.Octave as Octave exposing (OctaveError(..))
import MusicTheory.Pitch as Pitch
import MusicTheory.Pitch.Enharmonic as PitchEnharmonic
import MusicTheory.PitchClass.Enharmonic as PitchClassEnharmonic
import Test exposing (..)
import Util.OctaveFuzzer
import Util.PitchClassFuzzer


all : Test
all =
    describe "Pitch Tests"
        [ fuzz2 Util.PitchClassFuzzer.pitchClass Util.OctaveFuzzer.octave "simple enharmonic equivalent should have same number of semitones" <|
            \pitchClass octave ->
                let
                    pitch =
                        pitchClass |> Internal.fromPitchClass octave

                    expectedSemitones =
                        pitch |> Internal.semitones

                    expectedResult =
                        if expectedSemitones < 0 then
                            Err <| InvalidEnharmonicEquivalent (pitch |> Internal.pitchClass |> PitchClassEnharmonic.simple) (BelowValidRange -1)

                        else if expectedSemitones >= 108 then
                            Err <| InvalidEnharmonicEquivalent (pitch |> Internal.pitchClass |> PitchClassEnharmonic.simple) (AboveValidRange 9)

                        else
                            Ok expectedSemitones
                in
                pitch
                    |> PitchEnharmonic.simple
                    |> Result.map Internal.semitones
                    |> Expect.equal expectedResult
        , test "enharmonic equivalent of Ebbb4 as natural or sharp should be C#4" <|
            \_ ->
                Pitch.pitch E Pitch.tripleFlat Octave.four
                    |> Expect.equal (Pitch.pitch C Pitch.sharp Octave.four)
        , test "enharmonic equivalent of Cbb4 as natural or sharp should be B3" <|
            \_ ->
                Pitch.pitch C Pitch.doubleFlat Octave.four
                    |> Expect.equal (Pitch.pitch B Pitch.natural Octave.three)
        , test "enharmonic equivalent of F###2 as natural or flat should be Ab2" <|
            \_ ->
                Pitch.pitch F Pitch.tripleSharp Octave.two
                    |> Expect.equal (Pitch.pitch A Pitch.flat Octave.two)
        , test "enharmonic equivalent of B#7 as natural or flat should be C8" <|
            \_ ->
                Pitch.pitch B Pitch.sharp Octave.seven
                    |> Expect.equal (Pitch.pitch C Pitch.natural Octave.eight)
        ]
