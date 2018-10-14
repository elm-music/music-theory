module ScaleTests exposing (all)

import Expect
import Fuzz exposing (Fuzzer)
import MusicTheory.PitchClass exposing (Accidental(..), Letter(..), pitchClass)
import MusicTheory.Scale as Scale
import MusicTheory.ScaleClass as ScaleClass
import Test exposing (..)


all : Test
all =
    describe "Key Tests"
        [ test "C major scale should have C root" <|
            \_ ->
                Scale.scale (pitchClass C Natural) ScaleClass.major
                    |> Scale.root
                    |> Expect.equal (pitchClass C Natural)
        , test "C major" <|
            \_ ->
                let
                    pitchClassesInCMajor =
                        [ pitchClass C Natural
                        , pitchClass D Natural
                        , pitchClass E Natural
                        , pitchClass F Natural
                        , pitchClass G Natural
                        , pitchClass A Natural
                        , pitchClass B Natural
                        ]
                in
                Scale.scale (pitchClass C Natural) ScaleClass.major
                    |> Scale.toList
                    |> Expect.equal pitchClassesInCMajor
        , test "A minor" <|
            \_ ->
                let
                    pitchClassesInAMinor =
                        [ pitchClass A Natural
                        , pitchClass B Natural
                        , pitchClass C Natural
                        , pitchClass D Natural
                        , pitchClass E Natural
                        , pitchClass F Natural
                        , pitchClass G Natural
                        ]
                in
                Scale.scale (pitchClass A Natural) ScaleClass.minor
                    |> Scale.toList
                    |> Expect.equal pitchClassesInAMinor
        , test "C major pentatonic" <|
            \_ ->
                let
                    pitchClassesInCMajorPentatonic =
                        [ pitchClass C Natural
                        , pitchClass D Natural
                        , pitchClass E Natural
                        , pitchClass G Natural
                        , pitchClass A Natural
                        ]
                in
                Scale.scale (pitchClass C Natural) ScaleClass.majorPentatonic
                    |> Scale.toList
                    |> Expect.equal pitchClassesInCMajorPentatonic
        , test "A minor pentatonic" <|
            \_ ->
                let
                    pitchClassesInCMinorPentatonic =
                        [ pitchClass A Natural
                        , pitchClass C Natural
                        , pitchClass D Natural
                        , pitchClass E Natural
                        , pitchClass G Natural
                        ]
                in
                Scale.scale (pitchClass A Natural) ScaleClass.minorPentatonic
                    |> Scale.toList
                    |> Expect.equal pitchClassesInCMinorPentatonic
        , test "C whole tone" <|
            \_ ->
                let
                    pitchClassesInCWholeTone =
                        [ pitchClass C Natural
                        , pitchClass D Natural
                        , pitchClass E Natural
                        , pitchClass F Sharp
                        , pitchClass G Sharp
                        , pitchClass A Sharp
                        ]
                in
                Scale.scale (pitchClass C Natural) ScaleClass.wholeTone
                    |> Scale.toList
                    |> Expect.equal pitchClassesInCWholeTone
        , test "C diminished half-whole" <|
            \_ ->
                let
                    pitchClassesInCDiminishedHalfWhole =
                        [ pitchClass C Natural
                        , pitchClass D Flat
                        , pitchClass E Flat
                        , pitchClass F Flat
                        , pitchClass G Flat
                        , pitchClass A DoubleFlat
                        , pitchClass B DoubleFlat
                        , pitchClass C DoubleFlat
                        ]
                in
                Scale.scale (pitchClass C Natural) ScaleClass.diminishedHalfToneWholeTone
                    |> Scale.toList
                    |> Expect.equal pitchClassesInCDiminishedHalfWhole
        , test "C diminished whole-half" <|
            \_ ->
                let
                    pitchClassesInCDiminishedWholeHalf =
                        [ pitchClass C Natural
                        , pitchClass D Natural
                        , pitchClass E Flat
                        , pitchClass F Natural
                        , pitchClass G Flat
                        , pitchClass A Flat
                        , pitchClass B DoubleFlat
                        , pitchClass C Flat
                        ]
                in
                Scale.scale (pitchClass C Natural) ScaleClass.diminishedWholeToneHalfTone
                    |> Scale.toList
                    |> Expect.equal pitchClassesInCDiminishedWholeHalf
        ]
