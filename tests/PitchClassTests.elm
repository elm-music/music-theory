module PitchClassTests exposing (all)

import Expect
import Fuzz
import Maybe.Extra
import Test exposing (..)
import TestHelpers exposing (intervalFuzzer)
import Tonal.Interval as Interval
import Tonal.PitchClass exposing (..)


numberToLetter n =
    case n of
        0 ->
            C

        1 ->
            D

        2 ->
            E

        3 ->
            F

        4 ->
            G

        5 ->
            A

        6 ->
            B

        other ->
            if other > 0 then
                numberToLetter (other - 7)

            else
                numberToLetter (other + 7)


numberToAccidental n =
    case n of
        (-3) ->
            TripleFlat

        (-2) ->
            DoubleFlat

        (-1) ->
            Flat

        0 ->
            Natural

        1 ->
            Sharp

        2 ->
            DoubleSharp

        3 ->
            TripleSharp

        other ->
            if other < -3 then
                numberToAccidental (other + 7)

            else
                numberToAccidental (other - 7)


pitchClassFuzzer =
    Fuzz.intRange 0 6
        |> Fuzz.andThen
            (\letterNumber ->
                Fuzz.intRange -3 3
                    |> Fuzz.map
                        (\accidentalNumber ->
                            pitchClass (numberToLetter letterNumber) (numberToAccidental accidentalNumber)
                        )
            )


isNaturalOrFlat : ( Letter, Accidental ) -> Bool
isNaturalOrFlat ( letter, accidental ) =
    case accidental of
        Natural ->
            True

        Flat ->
            True

        _ ->
            False


isNaturalOrSharp : ( Letter, Accidental ) -> Bool
isNaturalOrSharp ( letter, accidental ) =
    case accidental of
        Natural ->
            True

        Sharp ->
            True

        _ ->
            False


all : Test
all =
    describe "Pitch Class Tests"
        [ fuzz pitchClassFuzzer "a pitch classes semitones should be within -1 and 12" <|
            \pc ->
                semitones pc
                    |> Expect.all [ Expect.atLeast -3, Expect.atMost 14 ]
        , fuzz pitchClassFuzzer "asNaturalOrLoweredOnce has same number of semitones" <|
            \pc ->
                asNaturalOrLoweredOnce pc
                    |> Expect.all
                        [ fromTuple
                            >> semitones
                            >> Expect.equal (semitones pc % 12)
                        , isNaturalOrFlat >> Expect.true "accidental is natural or flat"
                        ]
        , fuzz pitchClassFuzzer "asNaturalOrRaisedOnce has same number of semitones" <|
            \pc ->
                asNaturalOrRaisedOnce pc
                    |> Expect.all
                        [ fromTuple
                            >> semitones
                            >> Expect.equal (semitones pc % 12)
                        , isNaturalOrSharp >> Expect.true "accidental is natural or sharp"
                        ]
        , fuzz2 pitchClassFuzzer (Fuzz.intRange -100 100) "transposeBySemitones and asNaturalOrRaisedOnce semitones is sum of pitch class' semitones and interval semitones" <|
            \pc n ->
                pc
                    |> transposeBySemitones n
                    |> asNaturalOrRaisedOnce
                    |> Expect.all
                        [ fromTuple
                            >> semitones
                            >> Expect.equal ((semitones pc + n) % 12)
                        , isNaturalOrSharp >> Expect.true "accidental is natural or sharp"
                        ]
        , fuzz2 pitchClassFuzzer (Fuzz.intRange -100 100) "transposeBySemitones and asNaturalOrLoweredOnce semitones is sum of pitch class' semitones and interval semitones" <|
            \pc n ->
                pc
                    |> transposeBySemitones n
                    |> asNaturalOrLoweredOnce
                    |> Expect.all
                        [ fromTuple
                            >> semitones
                            >> Expect.equal ((semitones pc + n) % 12)
                        , isNaturalOrFlat >> Expect.true "accidental is natural or flat"
                        ]
        , fuzz2 pitchClassFuzzer intervalFuzzer "transpose pitch class by interval, result should have correct number of semitones" <|
            \pc interval ->
                pc
                    |> transposeUp interval
                    |> semitones
                    |> flip (%) 12
                    |> Expect.equal ((Interval.semitones interval + semitones pc) % 12)
        , fuzz2 pitchClassFuzzer intervalFuzzer "transpose a pitch class up and down by the same interval should result in the original pitch class" <|
            \pc interval ->
                pc
                    |> transposeUp interval
                    |> transposeDown interval
                    |> Expect.equal pc
        , fuzz pitchClassFuzzer "exact on pitch classes that can be represented with a valid accidental should result in a Just" <|
            exact
                >> Maybe.Extra.isJust
                >> Expect.true "should be a Just"
        , fuzz pitchClassFuzzer "transpose pitch class up an octave should result in the original pitch class" <|
            \pc ->
                pc
                    |> transposeUp Interval.perfectOctave
                    |> Expect.equal pc
        , fuzz pitchClassFuzzer "transpose pitch class down an octave should result in the original pitch class" <|
            \pc ->
                pc
                    |> transposeDown Interval.perfectOctave
                    |> Expect.equal pc
        , fuzz pitchClassFuzzer "transpose pitch class up a perfect unison should result in the original pitch class" <|
            \pc ->
                pc
                    |> transposeUp Interval.perfectUnison
                    |> Expect.equal pc
        , fuzz pitchClassFuzzer "transpose pitch class down a perfect unison result in the original pitch class" <|
            \pc ->
                pc
                    |> transposeDown Interval.perfectUnison
                    |> Expect.equal pc
        , fuzz5 pitchClassFuzzer intervalFuzzer intervalFuzzer intervalFuzzer intervalFuzzer "transpose up and down 4 intervals, expect original pitch class " <|
            \pc i1 i2 i3 i4 ->
                pc
                    |> transposeUp i1
                    |> transposeUp i2
                    |> transposeUp i3
                    |> transposeUp i4
                    |> transposeDown i1
                    |> transposeDown i2
                    |> transposeDown i3
                    |> transposeDown i4
                    |> Expect.all [ Expect.equal pc, \pc -> Expect.true "there should be an exact representation" (pc |> exact |> Maybe.Extra.isJust) ]
        , fuzz2 pitchClassFuzzer intervalFuzzer "transpose up by interval and by semitones should yield enharmonic equivalent results" <|
            \pc interval ->
                pc
                    |> transposeUp interval
                    |> areEnharmonicEqual (pc |> transposeBySemitones (Interval.semitones interval))
                    |> Expect.true "should be enharmonic equivalent"
        , fuzz2 pitchClassFuzzer intervalFuzzer "transpose down by interval and by semitones should yield enharmonic equivalent results" <|
            \pc interval ->
                pc
                    |> transposeDown interval
                    |> areEnharmonicEqual (pc |> transposeBySemitones (Interval.semitones interval * -1))
                    |> Expect.true "should be enharmonic equivalent"
        ]
