module PitchTests exposing (all)

import Expect
import Fuzz exposing (Fuzzer)
import MusicTheory.Letter exposing (Letter(..))
import MusicTheory.Octave as Octave
import MusicTheory.Pitch as Pitch
import MusicTheory.Pitch.Enharmonic as PitchEnharmonic
import MusicTheory.PitchClass as PitchClass
import MusicTheory.PitchClass.Enharmonic as PitchClassEnharmonic
import Test exposing (..)
import Util.OctaveFuzzer
import Util.PitchClassFuzzer


all : Test
all =
    describe "Pitch Tests"
        [ fuzz2 Util.PitchClassFuzzer.pitchClass Util.OctaveFuzzer.octave "simple enharmonic should have same number of semitones" <|
            \pitchClass octave ->
                let
                    pitch =
                        pitchClass |> Pitch.fromPitchClass octave

                    expectedSemitones =
                        pitch |> Pitch.semitones

                    expectedResult =
                        if expectedSemitones < 0 then
                            Nothing

                        else if expectedSemitones >= 108 then
                            Nothing

                        else
                            Just expectedSemitones
                in
                pitch
                    |> PitchEnharmonic.simple
                    |> Maybe.map Pitch.semitones
                    |> Expect.equal expectedResult
        , test "semitones of B##4 should be 61 (4*12 (octave) + 11 (letter B) + 2 (double sharp))" <|
            \_ ->
                Pitch.fromPitchClass Octave.four (PitchClass.pitchClass B PitchClass.doubleSharp)
                    |> Pitch.semitones
                    |> Expect.equal 61
        , test "semitones of C#5 should be 61 (5*12 (octave) + 0 (letter C) + 1 (sharp))" <|
            \_ ->
                Pitch.fromPitchClass Octave.five (PitchClass.pitchClass C PitchClass.sharp)
                    |> Pitch.semitones
                    |> Expect.equal 61
        ]
