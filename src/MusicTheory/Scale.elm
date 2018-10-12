module MusicTheory.Scale exposing
    ( Scale
    , root
    , scale
    , toList
    )

import MusicTheory.Internals.ScaleClass as Internal
import MusicTheory.Interval as Interval exposing (Interval)
import MusicTheory.PitchClass as PitchClass exposing (PitchClass)
import MusicTheory.ScaleClass as ScaleClass exposing (ScaleClass)



-- Scale


type Scale
    = Pentatonic PentatonicScaleDegrees
    | Hexatonic HexatonicScaleDegrees
    | Heptatonic HeptatonicScaleDegrees
    | Octatonic OctatonicScaleDegrees


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
        Internal.Pentatonic pentatonicScaleClassIntervals ->
            pentatonicScale scaleRoot pentatonicScaleClassIntervals

        Internal.Hexatonic hexatonicScaleClassIntervals ->
            hexatonicScale scaleRoot hexatonicScaleClassIntervals

        Internal.Heptatonic heptatonicScaleClassIntervals ->
            heptatonicScale scaleRoot heptatonicScaleClassIntervals

        Internal.Octatonic octatonicScaleClassIntervals ->
            octatonicScale scaleRoot octatonicScaleClassIntervals


root : Scale -> PitchClass
root theScale =
    case theScale of
        Pentatonic scaleDegrees ->
            scaleDegrees.root

        Hexatonic scaleDegrees ->
            scaleDegrees.root

        Heptatonic scaleDegrees ->
            scaleDegrees.root

        Octatonic scaleDegrees ->
            scaleDegrees.root


toList : Scale -> List PitchClass
toList theScale =
    case theScale of
        Pentatonic scaleDegrees ->
            [ scaleDegrees.root
            , scaleDegrees.secondDegree
            , scaleDegrees.thirdDegree
            , scaleDegrees.fourthDegree
            , scaleDegrees.fifthDegree
            ]

        Hexatonic scaleDegrees ->
            [ scaleDegrees.root
            , scaleDegrees.secondDegree
            , scaleDegrees.thirdDegree
            , scaleDegrees.fourthDegree
            , scaleDegrees.fifthDegree
            , scaleDegrees.sixthDegree
            ]

        Heptatonic scaleDegrees ->
            [ scaleDegrees.root
            , scaleDegrees.secondDegree
            , scaleDegrees.thirdDegree
            , scaleDegrees.fourthDegree
            , scaleDegrees.fifthDegree
            , scaleDegrees.sixthDegree
            , scaleDegrees.seventhDegree
            ]

        Octatonic scaleDegrees ->
            [ scaleDegrees.root
            , scaleDegrees.secondDegree
            , scaleDegrees.thirdDegree
            , scaleDegrees.fourthDegree
            , scaleDegrees.fifthDegree
            , scaleDegrees.sixthDegree
            , scaleDegrees.seventhDegree
            , scaleDegrees.eighthDegree
            ]


pentatonicScale : PitchClass -> Internal.PentatonicScaleClassIntervals -> Scale
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
    Pentatonic
        { root = scaleRoot
        , secondDegree = secondDegree
        , thirdDegree = thirdDegree
        , fourthDegree = fourthDegree
        , fifthDegree = fifthDegree
        }


hexatonicScale : PitchClass -> Internal.HexatonicScaleClassIntervals -> Scale
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
    Hexatonic
        { root = scaleRoot
        , secondDegree = secondDegree
        , thirdDegree = thirdDegree
        , fourthDegree = fourthDegree
        , fifthDegree = fifthDegree
        , sixthDegree = sixthDegree
        }


heptatonicScale : PitchClass -> Internal.HeptatonicScaleClassIntervals -> Scale
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
    Heptatonic
        { root = scaleRoot
        , secondDegree = secondDegree
        , thirdDegree = thirdDegree
        , fourthDegree = fourthDegree
        , fifthDegree = fifthDegree
        , sixthDegree = sixthDegree
        , seventhDegree = seventhDegree
        }


octatonicScale : PitchClass -> Internal.OctatonicScaleClassIntervals -> Scale
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
    Octatonic
        { root = scaleRoot
        , secondDegree = secondDegree
        , thirdDegree = thirdDegree
        , fourthDegree = fourthDegree
        , fifthDegree = fifthDegree
        , sixthDegree = sixthDegree
        , seventhDegree = seventhDegree
        , eighthDegree = eighthDegree
        }
