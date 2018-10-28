module MusicTests exposing (all)

import Expect
import Fuzz exposing (Fuzzer)
import MusicTheory.Letter exposing (Letter(..))
import MusicTheory.Music exposing (..)
import MusicTheory.Octave exposing (five, four)
import MusicTheory.Pitch as Pitch exposing (Pitch, flat, natural, pitch, sharp)
import MusicTheory.Pitch.Spelling as Spelling
import MusicTheory.PitchClass.Spelling exposing (Accidental(..))
import Test exposing (..)


singleNote : Duration -> a -> Music a
singleNote duration a =
    primitives (single (note duration a))


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


all : Test
all =
    describe "Music Tests"
        [ test "get all pitches from a music" <|
            \_ ->
                twoFiveOne
                    |> map Spelling.simple
                    |> toList
                    |> Expect.equal
                        [ Ok { letter = D, accidental = Natural, octave = four }
                        , Ok { letter = F, accidental = Natural, octave = four }
                        , Ok { letter = A, accidental = Natural, octave = four }
                        , Ok { letter = D, accidental = Natural, octave = four }
                        , Ok { letter = G, accidental = Natural, octave = four }
                        , Ok { letter = B, accidental = Natural, octave = four }
                        , Ok { letter = C, accidental = Natural, octave = four }
                        , Ok { letter = E, accidental = Natural, octave = four }
                        , Ok { letter = G, accidental = Natural, octave = four }
                        ]
        ]
