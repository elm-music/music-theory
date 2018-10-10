module KeyTests exposing (all)

import Expect
import Fuzz exposing (Fuzzer)
import MusicTheory.Key as Key
import MusicTheory.PitchClass as PC exposing (Accidental(..), Letter(..))
import MusicTheory.Scale as Scale
import Test exposing (..)


all : Test
all =
    describe "Key Tests"
        [ test "C major key should have correct pitch classes" <|
            \_ ->
                Key.major (PC.pitchClass C Natural)
                    |> Key.scaleDegrees
                    |> Expect.equal
                        { root = PC.pitchClass C Natural
                        , secondDegree = PC.pitchClass D Natural
                        , thirdDegree = PC.pitchClass E Natural
                        , fourthDegree = PC.pitchClass F Natural
                        , fifthDegree = PC.pitchClass G Natural
                        , sixthDegree = PC.pitchClass A Natural
                        , seventhDegree = PC.pitchClass B Natural
                        }
        ]
