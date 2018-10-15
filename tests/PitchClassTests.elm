module PitchClassTests exposing (all)

import Expect
import Fuzz
import IntervalFuzzer exposing (intervalFuzzer)
import Maybe.Extra
import MusicTheory.Accidental exposing (Accidental(..))
import MusicTheory.Interval as Interval
import MusicTheory.Letter exposing (Letter(..))
import MusicTheory.PitchClass exposing (..)
import MusicTheory.PitchClass.Spelling as Spelling
import Test exposing (..)


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
    if n == -3 then
        tripleFlat

    else if n == -2 then
        doubleFlat

    else if n == -1 then
        flat

    else if n == 0 then
        natural

    else if n == 1 then
        sharp

    else if n == 2 then
        doubleSharp

    else if n == 3 then
        tripleSharp

    else if n < -3 then
        numberToAccidental (n + 7)

    else
        numberToAccidental (n - 7)


pitchClassFuzzer =
    Fuzz.map2
        pitchClass
        (Fuzz.intRange 0 6 |> Fuzz.map numberToLetter)
        (Fuzz.intRange -3 3 |> Fuzz.map numberToAccidental)


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
                Spelling.naturalOrSingleFlat pc
                    |> Expect.all
                        [ Spelling.toPitchClass
                            >> semitones
                            >> Expect.equal (semitones pc |> modBy 12)
                        , isNaturalOrFlat >> Expect.true "accidental is natural or flat"
                        ]
        , fuzz pitchClassFuzzer "asNaturalOrRaisedOnce has same number of semitones" <|
            \pc ->
                Spelling.naturalOrSingleSharp pc
                    |> Expect.all
                        [ Spelling.toPitchClass
                            >> semitones
                            >> Expect.equal (semitones pc |> modBy 12)
                        , isNaturalOrSharp >> Expect.true "accidental is natural or sharp"
                        ]
        , fuzz2 pitchClassFuzzer (Fuzz.intRange -100 100) "transposeBySemitones and asNaturalOrRaisedOnce semitones is sum of pitch class' semitones and interval semitones" <|
            \pc n ->
                pc
                    |> transposeBySemitones n
                    |> Spelling.naturalOrSingleSharp
                    |> Expect.all
                        [ Spelling.toPitchClass
                            >> semitones
                            >> Expect.equal ((semitones pc + n) |> modBy 12)
                        , isNaturalOrSharp >> Expect.true "accidental is natural or sharp"
                        ]
        , fuzz2 pitchClassFuzzer (Fuzz.intRange -100 100) "transposeBySemitones and asNaturalOrLoweredOnce semitones is sum of pitch class' semitones and interval semitones" <|
            \pc n ->
                pc
                    |> transposeBySemitones n
                    |> Spelling.naturalOrSingleFlat
                    |> Expect.all
                        [ Spelling.toPitchClass
                            >> semitones
                            >> Expect.equal ((semitones pc + n) |> modBy 12)
                        , isNaturalOrFlat >> Expect.true "accidental is natural or flat"
                        ]
        , fuzz2 pitchClassFuzzer intervalFuzzer "transpose pitch class by interval, result should have correct number of semitones" <|
            \pc interval ->
                pc
                    |> transposeUp interval
                    |> semitones
                    |> modBy 12
                    |> Expect.equal ((Interval.semitones interval + semitones pc) |> modBy 12)
        , fuzz2 pitchClassFuzzer intervalFuzzer "transpose a pitch class up and down by the same interval should result in the original pitch class" <|
            \pc interval ->
                pc
                    |> transposeUp interval
                    |> transposeDown interval
                    |> Expect.equal pc
        , fuzz pitchClassFuzzer "exact on pitch classes that can be represented with a valid accidental should result in a Just" <|
            Spelling.exactSpelling
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
        , fuzz3 pitchClassFuzzer intervalFuzzer intervalFuzzer "transpose up and down 2 intervals, expect original pitch class " <|
            \pc i1 i2 ->
                pc
                    |> transposeUp i1
                    |> transposeUp i2
                    |> transposeDown i1
                    |> transposeDown i2
                    |> Expect.all
                        [ Expect.equal pc
                        , \result -> Expect.true "there should be an exact representation" (result |> Spelling.exactSpelling |> Maybe.Extra.isJust)
                        ]
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
