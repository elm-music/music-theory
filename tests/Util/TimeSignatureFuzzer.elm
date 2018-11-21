module Util.TimeSignatureFuzzer exposing (timeSignature)

import Fuzz exposing (Fuzzer)
import MusicTheory.TimeSignature as TS exposing (BeatValue(..), NumberOfBeats(..), TimeSignature)
import Util.Fuzzer


timeSignature : Fuzzer TimeSignature
timeSignature =
    let
        beatValue =
            Util.Fuzzer.fromList
                [ Whole
                , Half
                , Quarter
                , Eighth
                , Sixteenth
                , ThirtySecond
                ]

        numberOfBeats =
            Util.Fuzzer.fromList
                [ One
                , Two
                , Three
                , Four
                , Five
                , Six
                , Seven
                , Eight
                , Nine
                , Ten
                , Eleven
                , Twelve
                , Thirteen
                , Fourteen
                , Fifteen
                , Sixteen
                , Seventeen
                , Eighteen
                , Nineteen
                , Twenty
                ]
    in
    Fuzz.map2 TS.timeSignature numberOfBeats beatValue
