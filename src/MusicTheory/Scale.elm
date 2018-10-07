module MusicTheory.Scale exposing
    ( Scale
    , aeolian
    , diatonicScale
    , diminishedHalfWhole
    , diminishedWholeHalf
    , ionian
    , isDiatonic
    , major
    , majorPentatonic
    , minor
    , minorPentatonic
    , nonDiatonicScale
    , scale
    , toList
    , wholeTone
    )

import MusicTheory.Interval as Interval exposing (Interval)
import MusicTheory.PitchClass as PitchClass exposing (PitchClass)



-- Scale


scale : PitchClass -> ScaleClass -> Scale
scale root scaleClass =
    case scaleClass of
        DiatonicClass diatonicScaleClass ->
            Diatonic <| diatonicScale root diatonicScaleClass

        NonDiatonicClass nonDiatonicScaleClass ->
            NonDiatonic <| nonDiatonicScale root nonDiatonicScaleClass


isDiatonic : Scale -> Bool
isDiatonic theScale =
    case theScale of
        Diatonic _ ->
            True

        NonDiatonic _ ->
            False


toList : Scale -> List PitchClass
toList theScale =
    case theScale of
        Diatonic (DiatonicScale scaleDegrees) ->
            [ scaleDegrees.root
            , scaleDegrees.secondDegree
            , scaleDegrees.thirdDegree
            , scaleDegrees.fourthDegree
            , scaleDegrees.fifthDegree
            , scaleDegrees.sixthDegree
            , scaleDegrees.seventhDegree
            ]

        NonDiatonic nonDiatonic ->
            case nonDiatonic of
                PentatonicScale scaleDegrees ->
                    [ scaleDegrees.root
                    , scaleDegrees.secondDegree
                    , scaleDegrees.thirdDegree
                    , scaleDegrees.fourthDegree
                    , scaleDegrees.fifthDegree
                    ]

                HexatonicScale scaleDegrees ->
                    [ scaleDegrees.root
                    , scaleDegrees.secondDegree
                    , scaleDegrees.thirdDegree
                    , scaleDegrees.fourthDegree
                    , scaleDegrees.fifthDegree
                    , scaleDegrees.sixthDegree
                    ]

                HeptatonicScale scaleDegrees ->
                    [ scaleDegrees.root
                    , scaleDegrees.secondDegree
                    , scaleDegrees.thirdDegree
                    , scaleDegrees.fourthDegree
                    , scaleDegrees.fifthDegree
                    , scaleDegrees.sixthDegree
                    , scaleDegrees.seventhDegree
                    ]

                OctatonicScale scaleDegrees ->
                    [ scaleDegrees.root
                    , scaleDegrees.secondDegree
                    , scaleDegrees.thirdDegree
                    , scaleDegrees.fourthDegree
                    , scaleDegrees.fifthDegree
                    , scaleDegrees.sixthDegree
                    , scaleDegrees.eighthDegree
                    ]


type Scale
    = Diatonic DiatonicScale
    | NonDiatonic NonDiatonicScale


type DiatonicScale
    = DiatonicScale HeptatonicScaleDegrees


type NonDiatonicScale
    = PentatonicScale PentatonicScaleDegrees
    | HexatonicScale HexatonicScaleDegrees
    | HeptatonicScale HeptatonicScaleDegrees
    | OctatonicScale OctatonicScaleDegrees


type alias PentatonicScaleDegrees =
    { root : PitchClass
    , secondDegree : PitchClass
    , thirdDegree : PitchClass
    , fourthDegree : PitchClass
    , fifthDegree : PitchClass
    }


type alias HexatonicScaleDegrees =
    { root : PitchClass
    , secondDegree : PitchClass
    , thirdDegree : PitchClass
    , fourthDegree : PitchClass
    , fifthDegree : PitchClass
    , sixthDegree : PitchClass
    }


type alias HeptatonicScaleDegrees =
    { root : PitchClass
    , secondDegree : PitchClass
    , thirdDegree : PitchClass
    , fourthDegree : PitchClass
    , fifthDegree : PitchClass
    , sixthDegree : PitchClass
    , seventhDegree : PitchClass
    }


type alias OctatonicScaleDegrees =
    { root : PitchClass
    , secondDegree : PitchClass
    , thirdDegree : PitchClass
    , fourthDegree : PitchClass
    , fifthDegree : PitchClass
    , sixthDegree : PitchClass
    , seventhDegree : PitchClass
    , eighthDegree : PitchClass
    }


diatonicScale : PitchClass -> DiatonicScaleClass -> DiatonicScale
diatonicScale root (DiatonicScaleClass intervals) =
    DiatonicScale
        { root = root
        , secondDegree = PitchClass.transposeUp intervals.secondDegree root
        , thirdDegree = PitchClass.transposeUp intervals.thirdDegree root
        , fourthDegree = PitchClass.transposeUp intervals.fourthDegree root
        , fifthDegree = PitchClass.transposeUp intervals.fifthDegree root
        , sixthDegree = PitchClass.transposeUp intervals.sixthDegree root
        , seventhDegree = PitchClass.transposeUp intervals.seventhDegree root
        }


nonDiatonicScale : PitchClass -> NonDiatonicScaleClass -> NonDiatonicScale
nonDiatonicScale root scaleClass =
    case scaleClass of
        PentatonicScaleClass pentatonicScaleClassIntervals ->
            pentatonicScale root pentatonicScaleClassIntervals

        HexatonicScaleClass hexatonicScaleClassIntervals ->
            hexatonicScale root hexatonicScaleClassIntervals

        HeptatonicScaleClass heptatonicScaleClassIntervals ->
            heptatonicScale root heptatonicScaleClassIntervals

        OctatonicScaleClass octatonicScaleClassIntervals ->
            octatonicScale root octatonicScaleClassIntervals


pentatonicScale : PitchClass -> PentatonicScaleClassIntervals -> NonDiatonicScale
pentatonicScale root intervals =
    PentatonicScale
        { root = root
        , secondDegree = PitchClass.transposeUp intervals.secondDegree root
        , thirdDegree = PitchClass.transposeUp intervals.thirdDegree root
        , fourthDegree = PitchClass.transposeUp intervals.fourthDegree root
        , fifthDegree = PitchClass.transposeUp intervals.fifthDegree root
        }


hexatonicScale : PitchClass -> HexatonicScaleClassIntervals -> NonDiatonicScale
hexatonicScale root intervals =
    HexatonicScale
        { root = root
        , secondDegree = PitchClass.transposeUp intervals.secondDegree root
        , thirdDegree = PitchClass.transposeUp intervals.thirdDegree root
        , fourthDegree = PitchClass.transposeUp intervals.fourthDegree root
        , fifthDegree = PitchClass.transposeUp intervals.fifthDegree root
        , sixthDegree = PitchClass.transposeUp intervals.sixthDegree root
        }


heptatonicScale : PitchClass -> HeptatonicScaleClassIntervals -> NonDiatonicScale
heptatonicScale root intervals =
    HeptatonicScale
        { root = root
        , secondDegree = PitchClass.transposeUp intervals.secondDegree root
        , thirdDegree = PitchClass.transposeUp intervals.thirdDegree root
        , fourthDegree = PitchClass.transposeUp intervals.fourthDegree root
        , fifthDegree = PitchClass.transposeUp intervals.fifthDegree root
        , sixthDegree = PitchClass.transposeUp intervals.sixthDegree root
        , seventhDegree = PitchClass.transposeUp intervals.seventhDegree root
        }


octatonicScale : PitchClass -> OctatonicScaleClassIntervals -> NonDiatonicScale
octatonicScale root intervals =
    OctatonicScale
        { root = root
        , secondDegree = PitchClass.transposeUp intervals.secondDegree root
        , thirdDegree = PitchClass.transposeUp intervals.thirdDegree root
        , fourthDegree = PitchClass.transposeUp intervals.fourthDegree root
        , fifthDegree = PitchClass.transposeUp intervals.fifthDegree root
        , sixthDegree = PitchClass.transposeUp intervals.sixthDegree root
        , seventhDegree = PitchClass.transposeUp intervals.seventhDegree root
        , eighthDegree = PitchClass.transposeUp intervals.eighthDegree root
        }



-- ScaleClass


type ScaleClass
    = DiatonicClass DiatonicScaleClass
    | NonDiatonicClass NonDiatonicScaleClass


type DiatonicScaleClass
    = DiatonicScaleClass HeptatonicScaleClassIntervals


type NonDiatonicScaleClass
    = PentatonicScaleClass PentatonicScaleClassIntervals
    | HexatonicScaleClass HexatonicScaleClassIntervals
    | HeptatonicScaleClass HeptatonicScaleClassIntervals
    | OctatonicScaleClass OctatonicScaleClassIntervals


type alias PentatonicScaleClassIntervals =
    { secondDegree : Interval
    , thirdDegree : Interval
    , fourthDegree : Interval
    , fifthDegree : Interval
    }


type alias HexatonicScaleClassIntervals =
    { secondDegree : Interval
    , thirdDegree : Interval
    , fourthDegree : Interval
    , fifthDegree : Interval
    , sixthDegree : Interval
    }


type alias HeptatonicScaleClassIntervals =
    { secondDegree : Interval
    , thirdDegree : Interval
    , fourthDegree : Interval
    , fifthDegree : Interval
    , sixthDegree : Interval
    , seventhDegree : Interval
    }


type alias OctatonicScaleClassIntervals =
    { secondDegree : Interval
    , thirdDegree : Interval
    , fourthDegree : Interval
    , fifthDegree : Interval
    , sixthDegree : Interval
    , seventhDegree : Interval
    , eighthDegree : Interval
    }


major : ScaleClass
major =
    ionian


minor : ScaleClass
minor =
    aeolian


ionian : ScaleClass
ionian =
    DiatonicClass <|
        DiatonicScaleClass
            { secondDegree = Interval.majorSecond
            , thirdDegree = Interval.majorThird
            , fourthDegree = Interval.perfectFourth
            , fifthDegree = Interval.perfectFifth
            , sixthDegree = Interval.majorSixth
            , seventhDegree = Interval.majorSeventh
            }


aeolian : ScaleClass
aeolian =
    DiatonicClass <|
        DiatonicScaleClass
            { secondDegree = Interval.majorSecond
            , thirdDegree = Interval.minorThird
            , fourthDegree = Interval.perfectFourth
            , fifthDegree = Interval.perfectFifth
            , sixthDegree = Interval.minorSixth
            , seventhDegree = Interval.minorSeventh
            }


diminishedHalfWhole : ScaleClass
diminishedHalfWhole =
    NonDiatonicClass <|
        OctatonicScaleClass
            { secondDegree = Interval.minorSecond
            , thirdDegree = Interval.augmentedSecond
            , fourthDegree = Interval.majorThird
            , fifthDegree = Interval.augmentedFourth
            , sixthDegree = Interval.perfectFifth
            , seventhDegree = Interval.majorSixth
            , eighthDegree = Interval.minorSeventh
            }


diminishedWholeHalf : ScaleClass
diminishedWholeHalf =
    NonDiatonicClass <|
        OctatonicScaleClass
            { secondDegree = Interval.majorSecond
            , thirdDegree = Interval.minorThird
            , fourthDegree = Interval.perfectFourth
            , fifthDegree = Interval.diminishedFifth
            , sixthDegree = Interval.minorSixth
            , seventhDegree = Interval.diminishedSeventh
            , eighthDegree = Interval.majorSeventh
            }


wholeTone : ScaleClass
wholeTone =
    NonDiatonicClass <|
        HexatonicScaleClass
            { secondDegree = Interval.majorSecond
            , thirdDegree = Interval.majorThird
            , fourthDegree = Interval.augmentedFourth
            , fifthDegree = Interval.augmentedFifth
            , sixthDegree = Interval.augmentedSixth
            }


majorPentatonic : ScaleClass
majorPentatonic =
    NonDiatonicClass <|
        PentatonicScaleClass
            { secondDegree = Interval.majorSecond
            , thirdDegree = Interval.majorThird
            , fourthDegree = Interval.perfectFifth
            , fifthDegree = Interval.majorSixth
            }


minorPentatonic : ScaleClass
minorPentatonic =
    NonDiatonicClass <|
        PentatonicScaleClass
            { secondDegree = Interval.minorThird
            , thirdDegree = Interval.perfectFourth
            , fourthDegree = Interval.perfectFifth
            , fifthDegree = Interval.minorSeventh
            }
