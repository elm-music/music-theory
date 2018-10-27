module MusicTests exposing (all)

import Expect
import Fuzz exposing (Fuzzer)
import MusicTheory.Letter exposing (Letter(..))
import MusicTheory.Music exposing (..)
import MusicTheory.Octave exposing (five, four)
import MusicTheory.Pitch as Pitch exposing (Pitch, flat, natural, pitch, sharp)
import Test exposing (..)


all : Test
all =
    describe "Music Tests"
        [ test "get all pitches from a music" <|
            \_ ->
                let
                    twoFiveOne : Music Pitch
                    twoFiveOne =
                        let
                            dMinor =
                                Par (Primitives (Single (note quarter (pitch D natural four)))) <|
                                    Par (Primitives (Single (note quarter (pitch F natural four)))) <|
                                        Primitives (Single (note quarter (pitch A natural four)))

                            gMajor =
                                Par (Primitives (Single (note quarter (pitch D natural four)))) <|
                                    Par (Primitives (Single (note quarter (pitch G natural four)))) <|
                                        Primitives (Single (note quarter (pitch B natural four)))

                            cMajor =
                                Par (Primitives (Single (note quarter (pitch C natural four)))) <|
                                    Par (Primitives (Single (note quarter (pitch E natural four)))) <|
                                        Primitives (Single (note quarter (pitch G natural four)))
                        in
                        Seq dMinor <| Seq gMajor cMajor
                in
                twoFiveOne
                    |> toList
                    |> Expect.equal
                        [ pitch D natural four
                        , pitch F natural four
                        , pitch A natural four
                        , pitch D natural four
                        , pitch G natural four
                        , pitch B natural four
                        , pitch C natural four
                        , pitch E natural four
                        , pitch G natural four
                        ]
        ]
