module MusicTheory.ScaleClass exposing
    ( ScaleClass
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

import MusicTheory.Internals.ScaleClass as Internal
import MusicTheory.Interval as Interval exposing (Interval)
import MusicTheory.PitchClass as PitchClass exposing (PitchClass)


type alias ScaleClass =
    Internal.ScaleClass


major : ScaleClass
major =
    ionian


minor : ScaleClass
minor =
    aeolian


ionian : ScaleClass
ionian =
    Internal.Heptatonic
        { firstInterval = Interval.majorSecond
        , secondInterval = Interval.majorSecond
        , thirdInterval = Interval.minorSecond
        , fourthInterval = Interval.majorSecond
        , fifthInterval = Interval.majorSecond
        , sixthInterval = Interval.majorSecond
        }


aeolian : ScaleClass
aeolian =
    Internal.Heptatonic
        { firstInterval = Interval.majorSecond
        , secondInterval = Interval.minorSecond
        , thirdInterval = Interval.majorSecond
        , fourthInterval = Interval.majorSecond
        , fifthInterval = Interval.minorSecond
        , sixthInterval = Interval.majorSecond
        }


diminishedHalfWhole : ScaleClass
diminishedHalfWhole =
    Internal.Octatonic
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
    Internal.Octatonic
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
    Internal.Hexatonic
        { firstInterval = Interval.majorSecond
        , secondInterval = Interval.majorSecond
        , thirdInterval = Interval.majorSecond
        , fourthInterval = Interval.majorSecond
        , fifthInterval = Interval.majorSecond
        }


majorPentatonic : ScaleClass
majorPentatonic =
    Internal.Pentatonic
        { firstInterval = Interval.majorSecond
        , secondInterval = Interval.majorSecond
        , thirdInterval = Interval.minorThird
        , fourthInterval = Interval.majorSecond
        }


minorPentatonic : ScaleClass
minorPentatonic =
    Internal.Pentatonic
        { firstInterval = Interval.minorThird
        , secondInterval = Interval.majorSecond
        , thirdInterval = Interval.majorSecond
        , fourthInterval = Interval.minorThird
        }
