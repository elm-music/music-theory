module Notation.AbcTests exposing (all)

import Expect
import Fuzz exposing (Fuzzer)
import MusicTheory.Duration as Duration exposing (..)
import MusicTheory.Interval as Interval
import MusicTheory.Key as Key
import MusicTheory.Letter exposing (Letter(..))
import MusicTheory.Notation as Notation exposing (..)
import MusicTheory.Notation.Abc as Abc exposing (..)
import MusicTheory.Octave exposing (..)
import MusicTheory.Pitch as Pitch exposing (Pitch, flat, natural, pitch, sharp)
import MusicTheory.Pitch.Enharmonic exposing (EnharmonicTransformationError)
import MusicTheory.Pitch.Spelling as Spelling exposing (..)
import MusicTheory.PitchClass as PitchClass
import MusicTheory.PitchClass.Spelling exposing (Accidental(..))
import MusicTheory.TimeSignature as TimeSignature
import MusicTheory.Tuplet as Tuplet
import Test exposing (..)


blockChords : Notation PitchSpelling
blockChords =
    let
        bfMin7 attrs =
            chord attrs [ chordNote [] (bFlat three), chordNote [] (dFlat four), chordNote [] (fNatural four) ]

        bDim attrs =
            chord attrs [ chordNote [] (bNatural three), chordNote [] (dNatural four), chordNote [] (fNatural four) ]

        afMaj6 attrs =
            chord attrs [ chordNote [] (cNatural four), chordNote [] (eFlat four), chordNote [] (fNatural four) ]

        voice1 =
            line
                [ bar []
                    [ note [] (aFlat four) quarterNote
                    , note [] (aFlat four) quarterNote
                    , note [] (aFlat four) eighthNote
                    , note [] (aFlat four) quarterNote
                    , note [ tie ] (aFlat four) eighthNote
                    ]
                , bar []
                    [ beam
                        [ note [] (aFlat four) eighthNote
                        , note [] (bFlat four) eighthNote
                        , note [] (aFlat four) eighthNote
                        , note [ tie ] (aFlat four) eighthNote
                        ]
                    , beam
                        [ note [] (aFlat four) eighthNote
                        , note [] (bFlat four) eighthNote
                        , note [] (aFlat four) eighthNote
                        , note [ tie ] (aFlat four) eighthNote
                        ]
                    ]
                , bar [ doubleLine ]
                    [ note [] (aFlat four) quarterNote
                    , beam
                        [ note [] (aFlat four) eighthNote
                        , note [] (aFlat four) eighthNote
                        ]
                    , rest [] halfNote
                    ]
                ]

        voice2 =
            line
                [ bar []
                    [ bfMin7 [] quarterNote
                    , bfMin7 [] quarterNote
                    , bDim [] eighthNote
                    , bDim [] quarterNote
                    , afMaj6 [ tie ] eighthNote
                    ]
                , bar []
                    [ afMaj6 [] (dotted quarterNote)
                    , bDim [ tie ] eighthNote
                    , bDim [] (dotted quarterNote)
                    , bfMin7 [ tie ] eighthNote
                    ]
                , bar [ doubleLine ]
                    [ bfMin7 [] quarterNote
                    , beam
                        [ bDim [] eighthNote
                        , afMaj6 [] eighthNote
                        ]
                    , rest [ Invisible ] halfNote
                    ]
                ]

        staff1 =
            staff treble [ voice1, voice2 ]
    in
    { title = Just "Block Chords"
    , composer = Nothing
    , key = Key.major (PitchClass.pitchClass C natural)
    , timeSignature = TimeSignature.four TimeSignature.quarters
    , tempo = Nothing
    , staffs = [ staff1 ]
    }


voicingsTwoFiveOne : Notation PitchSpelling
voicingsTwoFiveOne =
    let
        voice1 =
            bar [ doubleLine ]
                [ chord [] [ aNatural four |> chordNote [], fNatural four |> chordNote [], cNatural four |> chordNote [] ] quarterNote
                , chord [] [ bNatural four |> chordNote [], gSharp four |> chordNote [], eNatural four |> chordNote [], bNatural three |> chordNote [] ] eighthNote
                , chord [ tie ] [ gNatural four |> chordNote [], dNatural four |> chordNote [], aNatural three |> chordNote [] ] eighthNote
                , chord [] [ gNatural four |> chordNote [], dNatural four |> chordNote [], aNatural three |> chordNote [] ] halfNote
                ]

        voice2 =
            bar [ doubleLine ]
                [ chord [] [ gNatural three |> chordNote [], dNatural three |> chordNote [] ] quarterNote
                , note [] (fNatural three) eighthNote
                , note [ tie ] (eNatural three) eighthNote
                , note [] (eNatural three) halfNote
                ]
    in
    { title = Just "II V I Voicings"
    , composer = Nothing
    , key = Key.major (PitchClass.pitchClass C natural)
    , timeSignature = TimeSignature.four TimeSignature.quarters
    , tempo = Nothing
    , staffs = [ staff treble [ voice1 ], staff bass [ voice2 ] ]
    }


multipleLines : Notation PitchSpelling
multipleLines =
    let
        mkVoice p =
            List.repeat 10 (bar [] [ note [] p wholeNote ]) |> line
    in
    { title = Nothing
    , composer = Nothing
    , key = Key.major (PitchClass.pitchClass C natural)
    , timeSignature = TimeSignature.four TimeSignature.quarters
    , tempo = Nothing
    , staffs =
        [ staff treble
            [ cNatural four |> mkVoice
            , eNatural four |> mkVoice
            ]
        , staff bass [ aNatural two |> mkVoice ]
        , staff treble
            [ bNatural four |> mkVoice
            , dNatural five |> mkVoice
            ]
        ]
    }


all : Test
all =
    describe "Abc Tests"
        [ test "block chords to ABC notation" <|
            \_ ->
                let
                    expected =
                        "T: Block Chords\nM: 4/4\n%%score (0 1)\nV: 0 clef=treble\nV: 1 clef=treble\nK: C\n[V:0]_A2/1 _A2/1 _A1/1 _A2/1 _A1/1- | _A1/1_B1/1_A1/1_A1/1- _A1/1_B1/1_A1/1_A1/1-  | _A2/1 _A1/1_A1/1 z4/1 || \n[V:1][_B,2/1_D2/1=F] [_B,2/1_D2/1=F] [=B,1/1=D1/1=F] [=B,2/1=D2/1=F] [=C1/1_E1/1=F]- | [=C3/1_E3/1=F] [=B,1/1=D1/1=F]- [=B,3/1=D3/1=F] [_B,1/1_D1/1=F]- | [_B,2/1_D2/1=F] [=B,1/1=D1/1=F][=C1/1_E1/1=F] y4/1 || "
                in
                blockChords
                    |> Abc.fromNotation 4
                    |> Expect.equal expected
        , test "tuplet notation" <|
            \_ ->
                let
                    expected =
                        "M: 4/4\n%%score (0)\nV: 0 clef=treble\nK: C\n[V:0]=c1/1(3:2:3=c1/2=c1/2=c1/2"
                in
                beam
                    [ note [] (cNatural five) eighthNote
                    , tuplet Tuplet.triplet
                        [ note [] (cNatural five) sixteenthNote
                        , note [] (cNatural five) sixteenthNote
                        , note [] (cNatural five) sixteenthNote
                        ]
                    ]
                    |> fromVoice
                    |> Abc.fromNotation 4
                    |> Expect.equal expected
        , test "multiple staffs to ABC notation" <|
            \_ ->
                let
                    expected =
                        "T: II V I Voicings\nM: 4/4\n%%score (0) (1)\nV: 0 clef=treble\nV: 1 clef=bass\nK: C\n[V:0][=A2/1=F2/1=C] [=B1/1^G1/1=E1/1=B,] [=G1/1=D1/1=A,]- [=G4/1=D4/1=A,] ||\n[V:1][=G,2/1=D,] =F,1/1 =E,1/1- =E,4/1 ||"
                in
                voicingsTwoFiveOne
                    |> Abc.fromNotation 4
                    |> Expect.equal expected
        , test "line breaks" <|
            \_ ->
                let
                    expected =
                        "M: 4/4\n%%score (0 1) (2) (3 4)\nV: 0 clef=treble\nV: 1 clef=treble\nV: 2 clef=bass\nV: 3 clef=treble\nV: 4 clef=treble\nK: C\n[V:0]=C8/1 | =C8/1 | =C8/1 | =C8/1 |\n[V:1]=E8/1 | =E8/1 | =E8/1 | =E8/1 |\n[V:2]=A,,8/1 | =A,,8/1 | =A,,8/1 | =A,,8/1 |\n[V:3]=B8/1 | =B8/1 | =B8/1 | =B8/1 |\n[V:4]=d8/1 | =d8/1 | =d8/1 | =d8/1 |\n[V:0] =C8/1 | =C8/1 | =C8/1 | =C8/1 |\n[V:1] =E8/1 | =E8/1 | =E8/1 | =E8/1 |\n[V:2] =A,,8/1 | =A,,8/1 | =A,,8/1 | =A,,8/1 |\n[V:3] =B8/1 | =B8/1 | =B8/1 | =B8/1 |\n[V:4] =d8/1 | =d8/1 | =d8/1 | =d8/1 |\n[V:0] =C8/1 | =C8/1 | \n[V:1] =E8/1 | =E8/1 | \n[V:2] =A,,8/1 | =A,,8/1 | \n[V:3] =B8/1 | =B8/1 | \n[V:4] =d8/1 | =d8/1 | "
                in
                multipleLines
                    |> Abc.fromNotation 4
                    |> Expect.equal expected
        ]
