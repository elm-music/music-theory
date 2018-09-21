module IntervalTests exposing (all)

import Expect
import Fuzz exposing (Fuzzer)
import IntervalFuzzer exposing (intervalFuzzer)
import List.Extra
import MusicTheory.Interval as Interval
import Test exposing (..)


all : Test
all =
    describe "Interval Tests"
        [ test "A test" <|
            \_ ->
                Interval.all
                    |> List.map (\interval -> ( Interval.semitones interval, Interval.complementary interval |> Interval.semitones ))
                    |> List.all (\( a, b ) -> a + b == 12)
                    |> Expect.true "dflkfdskdfk"
        , test "complementary interval of a perfect union should be a perfect octave" <|
            \_ ->
                Interval.perfectUnison |> Interval.complementary |> Expect.equal Interval.perfectOctave
        , test "complementary interval of a diminished third  should be an augmented sixth" <|
            \_ ->
                Interval.diminishedThird |> Interval.complementary |> Expect.equal Interval.augmentedSixth
        , fuzz intervalFuzzer "complementary intervals semitones sum should be 12" <|
            \interval ->
                interval |> Interval.semitones |> Expect.equal (12 - (interval |> Interval.complementary |> Interval.semitones))
        ]
