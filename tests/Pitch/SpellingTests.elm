module SpellingTests exposing (all)

import Expect
import Fuzz exposing (Fuzzer)
import MusicTheory.Internal.Pitch as Internal
import MusicTheory.Internal.PitchClass as PitchClass
import MusicTheory.Letter exposing (Letter(..))
import MusicTheory.Octave as Octave exposing (OctaveError(..))
import MusicTheory.Pitch as Pitch
import MusicTheory.Pitch.Enharmonic as PitchEnharmonic exposing (EnharmonicTransformationError(..))
import MusicTheory.Pitch.Spelling as Spelling
import MusicTheory.PitchClass.Enharmonic as PitchClassEnharmonic
import MusicTheory.PitchClass.Spelling exposing (Accidental(..))
import Test exposing (..)
import Util.PitchFuzzer


all : Test
all =
    describe "Spelling Tests"
        [ test "C###4 should be spelled as D#4" <|
            \_ ->
                Pitch.pitch C Pitch.tripleSharp Octave.four
                    |> Spelling.simple
                    |> Expect.equal (Ok <| { letter = D, accidental = Sharp, octave = Octave.four })
        , test "B#5 should be spelled as C6" <|
            \_ ->
                Pitch.pitch B Pitch.sharp Octave.five
                    |> Spelling.simple
                    |> Expect.equal (Ok <| { letter = C, accidental = Natural, octave = Octave.six })
        ]
