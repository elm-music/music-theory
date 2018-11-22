module TimeSignatureTests exposing (all)

import Expect
import MusicTheory.TimeSignature as TimeSignature exposing (BeatValue(..), NumberOfBeats(..))
import Test exposing (..)


all : Test
all =
    describe "Time Signature Tests"
        [ test "get number of beats of an additive time signature (7/8 divided into 4+3/8)" <|
            \_ ->
                TimeSignature.additive Four [ Three ] Eighth
                    |> TimeSignature.numberOfBeatsInt
                    |> Expect.equal 7
        ]
