module PitchTests exposing (all)

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
        [ fuzz2 Util.PitchClassFuzzer.pitchClass Util.OctaveFuzzer.octave "simple enharmonic should have same number of semitones" <|
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
        , test "semitones of B##4 should be 61 (4*12 (octave) + 11 (letter B) + 2 (double sharp))" <|
            \_ ->
                Internal.fromPitchClass Octave.four (PitchClass.pitchClass B PitchClass.doubleSharp)
                    |> Internal.semitones
                    |> Expect.equal 61
        , test "semitones of C#5 should be 61 (5*12 (octave) + 0 (letter C) + 1 (sharp))" <|
            \_ ->
                Internal.fromPitchClass Octave.five (PitchClass.pitchClass C PitchClass.sharp)
                    |> Internal.semitones
                    |> Expect.equal 61
        , test "toString" <|
            \_ ->
                let
                    testCases =
                        [ ( Internal.pitch C Internal.natural Octave.four, "C4" )
                        , ( Internal.pitch A Internal.flat Octave.zero, "A♭0" )
                        , ( Internal.pitch G Internal.tripleSharp Octave.six, "A♯6" )
                        , ( Internal.pitch B Internal.sharp Octave.eight, "C9" )
                        , ( Internal.pitch C Internal.flat Octave.zero, "B-1" )
                        ]

                    input =
                        testCases |> List.map (Tuple.first >> Pitch.toString)

                    expected =
                        testCases |> List.map Tuple.second
                in
                Expect.equal input expected
        ]
