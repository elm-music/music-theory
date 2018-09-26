module MusicTheory.ScaleClass exposing
    ( HeptatonicScaleClassIntervals
    , HexatonicScaleClassIntervals
    , OctatonicScaleClassIntervals
    , PentatonicScaleClassIntervals
    , ScaleClass
    , aeolian
    , diminishedHalfWhole
    , diminishedWholeHalf
    , ionian
    , major
    , majorPentatonic
    , minor
    , minorPentatonic
    , wholeTone
    )

import MusicTheory.Interval as Interval exposing (Interval)


type ScaleClass
    = PentatonicScaleClass PentatonicScaleClassIntervals
    | HexatonicScaleClass HexatonicScaleClassIntervals
    | HeptatonicScaleClass HeptatonicScaleClassIntervals
    | OctatonicScaleClass OctatonicScaleClassIntervals


type alias PentatonicScaleClassIntervals =
    { firstDegree : Interval
    , secondDegree : Interval
    , thirdDegree : Interval
    , fourthDegree : Interval
    , fifthDegree : Interval
    }


type alias HexatonicScaleClassIntervals =
    { firstDegree : Interval
    , secondDegree : Interval
    , thirdDegree : Interval
    , fourthDegree : Interval
    , fifthDegree : Interval
    , sixthDegree : Interval
    }


type alias HeptatonicScaleClassIntervals =
    { firstDegree : Interval
    , secondDegree : Interval
    , thirdDegree : Interval
    , fourthDegree : Interval
    , fifthDegree : Interval
    , sixthDegree : Interval
    , seventhDegree : Interval
    }


type alias OctatonicScaleClassIntervals =
    { firstDegree : Interval
    , secondDegree : Interval
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
    HeptatonicScaleClass
        { firstDegree = Interval.perfectUnison
        , secondDegree = Interval.majorSecond
        , thirdDegree = Interval.majorThird
        , fourthDegree = Interval.perfectFourth
        , fifthDegree = Interval.perfectFifth
        , sixthDegree = Interval.majorSixth
        , seventhDegree = Interval.majorSeventh
        }


aeolian : ScaleClass
aeolian =
    HeptatonicScaleClass
        { firstDegree = Interval.perfectUnison
        , secondDegree = Interval.majorSecond
        , thirdDegree = Interval.minorThird
        , fourthDegree = Interval.perfectFourth
        , fifthDegree = Interval.perfectFifth
        , sixthDegree = Interval.minorSixth
        , seventhDegree = Interval.minorSeventh
        }


diminishedHalfWhole : ScaleClass
diminishedHalfWhole =
    OctatonicScaleClass
        { firstDegree = Interval.perfectUnison
        , secondDegree = Interval.minorSecond
        , thirdDegree = Interval.augmentedSecond
        , fourthDegree = Interval.majorThird
        , fifthDegree = Interval.augmentedFourth
        , sixthDegree = Interval.perfectFifth
        , seventhDegree = Interval.majorSixth
        , eighthDegree = Interval.minorSeventh
        }


diminishedWholeHalf : ScaleClass
diminishedWholeHalf =
    OctatonicScaleClass
        { firstDegree = Interval.perfectUnison
        , secondDegree = Interval.majorSecond
        , thirdDegree = Interval.minorThird
        , fourthDegree = Interval.perfectFourth
        , fifthDegree = Interval.diminishedFifth
        , sixthDegree = Interval.minorSixth
        , seventhDegree = Interval.diminishedSeventh
        , eighthDegree = Interval.majorSeventh
        }


wholeTone : ScaleClass
wholeTone =
    HexatonicScaleClass
        { firstDegree = Interval.perfectUnison
        , secondDegree = Interval.majorSecond
        , thirdDegree = Interval.majorThird
        , fourthDegree = Interval.augmentedFourth
        , fifthDegree = Interval.augmentedFifth
        , sixthDegree = Interval.augmentedSixth
        }


majorPentatonic : ScaleClass
majorPentatonic =
    PentatonicScaleClass
        { firstDegree = Interval.perfectUnison
        , secondDegree = Interval.majorSecond
        , thirdDegree = Interval.majorThird
        , fourthDegree = Interval.perfectFifth
        , fifthDegree = Interval.majorSixth
        }


minorPentatonic : ScaleClass
minorPentatonic =
    PentatonicScaleClass
        { firstDegree = Interval.perfectUnison
        , secondDegree = Interval.minorThird
        , thirdDegree = Interval.perfectFourth
        , fourthDegree = Interval.perfectFifth
        , fifthDegree = Interval.minorSeventh
        }
