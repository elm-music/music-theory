module MusicTheory.Scale exposing
    ( Scale
    , ScaleClass
    , aeolian
    , diminishedHalfWhole
    , diminishedWholeHalf
    , ionian
    , major
    , majorPentatonic
    , minor
    , minorPentatonic
    , root
    , scale
    , toList
    , wholeTone
    )

import MusicTheory.Interval as Interval exposing (Interval)
import MusicTheory.PitchClass as PitchClass exposing (PitchClass)



-- Scale


type Scale
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


scale : PitchClass -> ScaleClass -> Scale
scale scaleRoot scaleClass =
    case scaleClass of
        PentatonicScaleClass pentatonicScaleClassIntervals ->
            pentatonicScale scaleRoot pentatonicScaleClassIntervals

        HexatonicScaleClass hexatonicScaleClassIntervals ->
            hexatonicScale scaleRoot hexatonicScaleClassIntervals

        HeptatonicScaleClass heptatonicScaleClassIntervals ->
            heptatonicScale scaleRoot heptatonicScaleClassIntervals

        OctatonicScaleClass octatonicScaleClassIntervals ->
            octatonicScale scaleRoot octatonicScaleClassIntervals


root : Scale -> PitchClass
root theScale =
    case theScale of
        PentatonicScale scaleDegrees ->
            scaleDegrees.root

        HexatonicScale scaleDegrees ->
            scaleDegrees.root

        HeptatonicScale scaleDegrees ->
            scaleDegrees.root

        OctatonicScale scaleDegrees ->
            scaleDegrees.root


toList : Scale -> List PitchClass
toList theScale =
    case theScale of
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
            , scaleDegrees.seventhDegree
            , scaleDegrees.eighthDegree
            ]


pentatonicScale : PitchClass -> PentatonicScaleClassIntervals -> Scale
pentatonicScale scaleRoot intervals =
    let
        secondDegree =
            PitchClass.transposeUp intervals.firstInterval scaleRoot

        thirdDegree =
            PitchClass.transposeUp intervals.secondInterval secondDegree

        fourthDegree =
            PitchClass.transposeUp intervals.thirdInterval thirdDegree

        fifthDegree =
            PitchClass.transposeUp intervals.fourthInterval fourthDegree
    in
    PentatonicScale
        { root = scaleRoot
        , secondDegree = secondDegree
        , thirdDegree = thirdDegree
        , fourthDegree = fourthDegree
        , fifthDegree = fifthDegree
        }


hexatonicScale : PitchClass -> HexatonicScaleClassIntervals -> Scale
hexatonicScale scaleRoot intervals =
    let
        secondDegree =
            PitchClass.transposeUp intervals.firstInterval scaleRoot

        thirdDegree =
            PitchClass.transposeUp intervals.secondInterval secondDegree

        fourthDegree =
            PitchClass.transposeUp intervals.thirdInterval thirdDegree

        fifthDegree =
            PitchClass.transposeUp intervals.fourthInterval fourthDegree

        sixthDegree =
            PitchClass.transposeUp intervals.fifthInterval fifthDegree
    in
    HexatonicScale
        { root = scaleRoot
        , secondDegree = secondDegree
        , thirdDegree = thirdDegree
        , fourthDegree = fourthDegree
        , fifthDegree = fifthDegree
        , sixthDegree = sixthDegree
        }


heptatonicScale : PitchClass -> HeptatonicScaleClassIntervals -> Scale
heptatonicScale scaleRoot intervals =
    let
        secondDegree =
            PitchClass.transposeUp intervals.firstInterval scaleRoot

        thirdDegree =
            PitchClass.transposeUp intervals.secondInterval secondDegree

        fourthDegree =
            PitchClass.transposeUp intervals.thirdInterval thirdDegree

        fifthDegree =
            PitchClass.transposeUp intervals.fourthInterval fourthDegree

        sixthDegree =
            PitchClass.transposeUp intervals.fifthInterval fifthDegree

        seventhDegree =
            PitchClass.transposeUp intervals.sixthInterval sixthDegree
    in
    HeptatonicScale
        { root = scaleRoot
        , secondDegree = secondDegree
        , thirdDegree = thirdDegree
        , fourthDegree = fourthDegree
        , fifthDegree = fifthDegree
        , sixthDegree = sixthDegree
        , seventhDegree = seventhDegree
        }


octatonicScale : PitchClass -> OctatonicScaleClassIntervals -> Scale
octatonicScale scaleRoot intervals =
    let
        secondDegree =
            PitchClass.transposeUp intervals.firstInterval scaleRoot

        thirdDegree =
            PitchClass.transposeUp intervals.secondInterval secondDegree

        fourthDegree =
            PitchClass.transposeUp intervals.thirdInterval thirdDegree

        fifthDegree =
            PitchClass.transposeUp intervals.fourthInterval fourthDegree

        sixthDegree =
            PitchClass.transposeUp intervals.fifthInterval fifthDegree

        seventhDegree =
            PitchClass.transposeUp intervals.sixthInterval sixthDegree

        eighthDegree =
            PitchClass.transposeUp intervals.seventhInterval seventhDegree
    in
    OctatonicScale
        { root = scaleRoot
        , secondDegree = secondDegree
        , thirdDegree = thirdDegree
        , fourthDegree = fourthDegree
        , fifthDegree = fifthDegree
        , sixthDegree = sixthDegree
        , seventhDegree = seventhDegree
        , eighthDegree = eighthDegree
        }



-- ScaleClass


type ScaleClass
    = PentatonicScaleClass PentatonicScaleClassIntervals
    | HexatonicScaleClass HexatonicScaleClassIntervals
    | HeptatonicScaleClass HeptatonicScaleClassIntervals
    | OctatonicScaleClass OctatonicScaleClassIntervals


type alias PentatonicScaleClassIntervals =
    { firstInterval : Interval
    , secondInterval : Interval
    , thirdInterval : Interval
    , fourthInterval : Interval
    }


type alias HexatonicScaleClassIntervals =
    { firstInterval : Interval
    , secondInterval : Interval
    , thirdInterval : Interval
    , fourthInterval : Interval
    , fifthInterval : Interval
    }


type alias HeptatonicScaleClassIntervals =
    { firstInterval : Interval
    , secondInterval : Interval
    , thirdInterval : Interval
    , fourthInterval : Interval
    , fifthInterval : Interval
    , sixthInterval : Interval
    }


type alias OctatonicScaleClassIntervals =
    { firstInterval : Interval
    , secondInterval : Interval
    , thirdInterval : Interval
    , fourthInterval : Interval
    , fifthInterval : Interval
    , sixthInterval : Interval
    , seventhInterval : Interval
    }


major : ScaleClass
major =
    ionian


minor : ScaleClass
minor =
    aeolian


ionian : ScaleClass
ionian =
    HeptatonicScaleClass
        { firstInterval = Interval.majorSecond
        , secondInterval = Interval.majorSecond
        , thirdInterval = Interval.minorSecond
        , fourthInterval = Interval.majorSecond
        , fifthInterval = Interval.majorSecond
        , sixthInterval = Interval.majorSecond
        }


aeolian : ScaleClass
aeolian =
    HeptatonicScaleClass
        { firstInterval = Interval.majorSecond
        , secondInterval = Interval.minorSecond
        , thirdInterval = Interval.majorSecond
        , fourthInterval = Interval.majorSecond
        , fifthInterval = Interval.minorSecond
        , sixthInterval = Interval.majorSecond
        }


diminishedHalfWhole : ScaleClass
diminishedHalfWhole =
    OctatonicScaleClass
        { firstInterval = Interval.minorSecond
        , secondInterval = Interval.majorSecond
        , thirdInterval = Interval.minorSecond
        , fourthInterval = Interval.majorSecond
        , fifthInterval = Interval.minorSecond
        , sixthInterval = Interval.majorSecond
        , seventhInterval = Interval.minorSecond
        }


diminishedWholeHalf : ScaleClass
diminishedWholeHalf =
    OctatonicScaleClass
        { firstInterval = Interval.majorSecond
        , secondInterval = Interval.minorSecond
        , thirdInterval = Interval.majorSecond
        , fourthInterval = Interval.minorSecond
        , fifthInterval = Interval.majorSecond
        , sixthInterval = Interval.minorSecond
        , seventhInterval = Interval.majorSecond
        }


wholeTone : ScaleClass
wholeTone =
    HexatonicScaleClass
        { firstInterval = Interval.majorSecond
        , secondInterval = Interval.majorSecond
        , thirdInterval = Interval.majorSecond
        , fourthInterval = Interval.majorSecond
        , fifthInterval = Interval.majorSecond
        }


majorPentatonic : ScaleClass
majorPentatonic =
    PentatonicScaleClass
        { firstInterval = Interval.majorSecond
        , secondInterval = Interval.majorSecond
        , thirdInterval = Interval.minorThird
        , fourthInterval = Interval.majorSecond
        }


minorPentatonic : ScaleClass
minorPentatonic =
    PentatonicScaleClass
        { firstInterval = Interval.minorThird
        , secondInterval = Interval.majorSecond
        , thirdInterval = Interval.majorSecond
        , fourthInterval = Interval.minorThird
        }
