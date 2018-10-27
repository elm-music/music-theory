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
                    singleNote duration pitch =
                        primitives (single (note duration pitch))

                    twoFiveOne : Music Pitch
                    twoFiveOne =
                        let
                            dMinor =
                                par (singleNote quarter (pitch D natural four)) <|
                                    par (singleNote quarter (pitch F natural four)) <|
                                        singleNote quarter (pitch A natural four)

                            gMajor =
                                par (singleNote quarter (pitch D natural four)) <|
                                    par (singleNote quarter (pitch G natural four)) <|
                                        singleNote quarter (pitch B natural four)

                            cMajor =
                                par (singleNote quarter (pitch C natural four)) <|
                                    par (singleNote quarter (pitch E natural four)) <|
                                        singleNote quarter (pitch G natural four)
                        in
                        seq dMinor <| seq gMajor cMajor
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
