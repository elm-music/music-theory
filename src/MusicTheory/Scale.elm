module MusicTheory.Scale exposing
    ( Scale
    , scale
    )

import MusicTheory.PitchClass as PitchClass exposing (PitchClass)
import MusicTheory.ScaleClass as ScaleClass exposing (ScaleClass(..))


type Scale
    = PentatonicScale PentatonicScaleDegrees
    | HexatonicScale HexatonicScaleDegrees
    | HeptatonicScale HeptatonicScaleDegrees
    | OctatonicScale OctatonicScaleDegrees


type alias PentatonicScaleDegrees =
    { firstDegree : PitchClass
    , secondDegree : PitchClass
    , thirdDegree : PitchClass
    , fourthDegree : PitchClass
    , fifthDegree : PitchClass
    }


type alias HexatonicScaleDegrees =
    { firstDegree : PitchClass
    , secondDegree : PitchClass
    , thirdDegree : PitchClass
    , fourthDegree : PitchClass
    , fifthDegree : PitchClass
    , sixthDegree : PitchClass
    }


type alias HeptatonicScaleDegrees =
    { firstDegree : PitchClass
    , secondDegree : PitchClass
    , thirdDegree : PitchClass
    , fourthDegree : PitchClass
    , fifthDegree : PitchClass
    , sixthDegree : PitchClass
    , seventhDegree : PitchClass
    }


type alias OctatonicScaleDegrees =
    { firstDegree : PitchClass
    , secondDegree : PitchClass
    , thirdDegree : PitchClass
    , fourthDegree : PitchClass
    , fifthDegree : PitchClass
    , sixthDegree : PitchClass
    , seventhDegree : PitchClass
    , eighthDegree : PitchClass
    }


scale : PitchClass -> ScaleClass -> Scale
scale root scaleClass =
    case scaleClass of
        PentatonicScaleClass pentatonicScaleClassIntervals ->
            pentatonicScale root pentatonicScaleClassIntervals

        HexatonicScaleClass hexatonicScaleClassIntervals ->
            hexatonicScale root hexatonicScaleClassIntervals

        HeptatonicScaleClass heptatonicScaleClassIntervals ->
            heptatonicScale root heptatonicScaleClassIntervals

        OctatonicScaleClass octatonicScaleClassIntervals ->
            octatonicScale root octatonicScaleClassIntervals


pentatonicScale : PitchClass -> ScaleClass.PentatonicScaleClassIntervals -> Scale
pentatonicScale root intervals =
    PentatonicScale
        { firstDegree = PitchClass.transposeUp intervals.firstDegree root
        , secondDegree = PitchClass.transposeUp intervals.secondDegree root
        , thirdDegree = PitchClass.transposeUp intervals.thirdDegree root
        , fourthDegree = PitchClass.transposeUp intervals.fourthDegree root
        , fifthDegree = PitchClass.transposeUp intervals.fifthDegree root
        }


hexatonicScale : PitchClass -> ScaleClass.HexatonicScaleClassIntervals -> Scale
hexatonicScale root intervals =
    HexatonicScale
        { firstDegree = PitchClass.transposeUp intervals.firstDegree root
        , secondDegree = PitchClass.transposeUp intervals.secondDegree root
        , thirdDegree = PitchClass.transposeUp intervals.thirdDegree root
        , fourthDegree = PitchClass.transposeUp intervals.fourthDegree root
        , fifthDegree = PitchClass.transposeUp intervals.fifthDegree root
        , sixthDegree = PitchClass.transposeUp intervals.sixthDegree root
        }


heptatonicScale : PitchClass -> ScaleClass.HeptatonicScaleClassIntervals -> Scale
heptatonicScale root intervals =
    HeptatonicScale
        { firstDegree = PitchClass.transposeUp intervals.firstDegree root
        , secondDegree = PitchClass.transposeUp intervals.secondDegree root
        , thirdDegree = PitchClass.transposeUp intervals.thirdDegree root
        , fourthDegree = PitchClass.transposeUp intervals.fourthDegree root
        , fifthDegree = PitchClass.transposeUp intervals.fifthDegree root
        , sixthDegree = PitchClass.transposeUp intervals.sixthDegree root
        , seventhDegree = PitchClass.transposeUp intervals.seventhDegree root
        }


octatonicScale : PitchClass -> ScaleClass.OctatonicScaleClassIntervals -> Scale
octatonicScale root intervals =
    OctatonicScale
        { firstDegree = PitchClass.transposeUp intervals.firstDegree root
        , secondDegree = PitchClass.transposeUp intervals.secondDegree root
        , thirdDegree = PitchClass.transposeUp intervals.thirdDegree root
        , fourthDegree = PitchClass.transposeUp intervals.fourthDegree root
        , fifthDegree = PitchClass.transposeUp intervals.fifthDegree root
        , sixthDegree = PitchClass.transposeUp intervals.sixthDegree root
        , seventhDegree = PitchClass.transposeUp intervals.seventhDegree root
        , eighthDegree = PitchClass.transposeUp intervals.eighthDegree root
        }
