module PitchClass.EnharmonicTests exposing (all)

import Expect
import Fuzz
import Maybe.Extra
import MusicTheory.Interval as Interval
import MusicTheory.Letter exposing (Letter(..))
import MusicTheory.PitchClass as PitchClass
import MusicTheory.PitchClass.Enharmonic as Enharmonic
import MusicTheory.PitchClass.Spelling as Spelling exposing (Accidental(..))
import Test exposing (..)
import Util.IntervalFuzzer as IntervalFuzzer
import Util.PitchClassFuzzer as PitchClassFuzzer


all : Test
all =
    describe "Enharmonic Tests"
        [ fuzz PitchClassFuzzer.pitchClass "all enharmonic equivalents should have same number of semitones" <|
            \pc ->
                Enharmonic.equivalents pc
                    |> List.all (PitchClass.semitones >> (==) (PitchClass.semitones pc))
                    |> Expect.true "semitones should be equal"
        ]
