module PitchClass.EnharmonicTests exposing (all)

import Expect
import MusicTheory.Accidental exposing (Accidental(..))
import MusicTheory.Letter exposing (Letter(..))
import MusicTheory.PitchClass as PitchClass
import MusicTheory.PitchClass.Enharmonic as Enharmonic
import Test exposing (..)


all : Test
all =
    describe "Pitch class enharmonic spelling test"
        [ test "C natural exact spelling" <|
            \_ ->
                PitchClass.pitchClass C Natural
                    |> Enharmonic.exactSpelling
                    |> Expect.equal (Just ( C, Natural ))
        , test "C# at most one accidental should be defined" <|
            \_ ->
                PitchClass.pitchClass C Sharp
                    |> Enharmonic.atMostOneAccidental
                    |> Expect.equal (Just ( C, Sharp ))
        , test "C with double sharp at most one accidental should not be defined" <|
            \_ ->
                PitchClass.pitchClass C DoubleSharp
                    |> Enharmonic.atMostOneAccidental
                    |> Expect.equal Nothing
        , test "C with double flat at most one accidental should not be defined" <|
            \_ ->
                PitchClass.pitchClass C DoubleFlat
                    |> Enharmonic.atMostOneAccidental
                    |> Expect.equal Nothing
        , test "simplified" <|
            \_ ->
                let
                    testCases =
                        [ ( PitchClass.pitchClass C TripleFlat, ( A, Natural ) )
                        , ( PitchClass.pitchClass C DoubleFlat, ( B, Flat ) )
                        , ( PitchClass.pitchClass C Flat, ( B, Natural ) )
                        , ( PitchClass.pitchClass C Natural, ( C, Natural ) )
                        , ( PitchClass.pitchClass C Sharp, ( C, Sharp ) )
                        , ( PitchClass.pitchClass C DoubleSharp, ( D, Natural ) )
                        , ( PitchClass.pitchClass C TripleSharp, ( D, Sharp ) )
                        , ( PitchClass.pitchClass D TripleFlat, ( B, Natural ) )
                        , ( PitchClass.pitchClass D DoubleFlat, ( C, Natural ) )
                        , ( PitchClass.pitchClass D Flat, ( D, Flat ) )
                        , ( PitchClass.pitchClass D Natural, ( D, Natural ) )
                        , ( PitchClass.pitchClass D Sharp, ( D, Sharp ) )
                        , ( PitchClass.pitchClass D DoubleSharp, ( E, Natural ) )
                        , ( PitchClass.pitchClass D TripleSharp, ( F, Natural ) )
                        , ( PitchClass.pitchClass E TripleFlat, ( D, Flat ) )
                        , ( PitchClass.pitchClass E DoubleFlat, ( D, Natural ) )
                        , ( PitchClass.pitchClass E Flat, ( E, Flat ) )
                        , ( PitchClass.pitchClass E Natural, ( E, Natural ) )
                        , ( PitchClass.pitchClass E Sharp, ( F, Natural ) )
                        , ( PitchClass.pitchClass E DoubleSharp, ( F, Sharp ) )
                        , ( PitchClass.pitchClass E TripleSharp, ( G, Natural ) )
                        , ( PitchClass.pitchClass F TripleFlat, ( D, Natural ) )
                        , ( PitchClass.pitchClass F DoubleFlat, ( E, Flat ) )
                        , ( PitchClass.pitchClass F Flat, ( E, Natural ) )
                        , ( PitchClass.pitchClass F Natural, ( F, Natural ) )
                        , ( PitchClass.pitchClass F Sharp, ( F, Sharp ) )
                        , ( PitchClass.pitchClass F DoubleSharp, ( G, Natural ) )
                        , ( PitchClass.pitchClass F TripleSharp, ( G, Sharp ) )
                        , ( PitchClass.pitchClass G TripleFlat, ( E, Natural ) )
                        , ( PitchClass.pitchClass G DoubleFlat, ( F, Natural ) )
                        , ( PitchClass.pitchClass G Flat, ( G, Flat ) )
                        , ( PitchClass.pitchClass G Natural, ( G, Natural ) )
                        , ( PitchClass.pitchClass G Sharp, ( G, Sharp ) )
                        , ( PitchClass.pitchClass G DoubleSharp, ( A, Natural ) )
                        , ( PitchClass.pitchClass G TripleSharp, ( A, Sharp ) )
                        , ( PitchClass.pitchClass A TripleFlat, ( G, Flat ) )
                        , ( PitchClass.pitchClass A DoubleFlat, ( G, Natural ) )
                        , ( PitchClass.pitchClass A Flat, ( A, Flat ) )
                        , ( PitchClass.pitchClass A Natural, ( A, Natural ) )
                        , ( PitchClass.pitchClass A Sharp, ( A, Sharp ) )
                        , ( PitchClass.pitchClass A DoubleSharp, ( B, Natural ) )
                        , ( PitchClass.pitchClass A TripleSharp, ( C, Natural ) )
                        , ( PitchClass.pitchClass B TripleFlat, ( A, Flat ) )
                        , ( PitchClass.pitchClass B DoubleFlat, ( A, Natural ) )
                        , ( PitchClass.pitchClass B Flat, ( B, Flat ) )
                        , ( PitchClass.pitchClass B Natural, ( B, Natural ) )
                        , ( PitchClass.pitchClass B Sharp, ( C, Natural ) )
                        , ( PitchClass.pitchClass B DoubleSharp, ( C, Sharp ) )
                        , ( PitchClass.pitchClass B TripleSharp, ( D, Natural ) )
                        ]

                    input =
                        testCases |> List.map Tuple.first

                    expected =
                        testCases |> List.map Tuple.second
                in
                input
                    |> List.map Enharmonic.simplified
                    |> Expect.equal expected
        ]
