module MusicTheory.Internals.ScaleClass exposing
    ( HeptatonicScaleClassIntervals
    , HexatonicScaleClassIntervals
    , OctatonicScaleClassIntervals
    , PentatonicScaleClassIntervals
    , ScaleClass(..)
    )

import MusicTheory.Interval as Interval exposing (Interval)
import MusicTheory.PitchClass as PitchClass exposing (PitchClass)


type ScaleClass
    = Pentatonic PentatonicScaleClassIntervals
    | Hexatonic HexatonicScaleClassIntervals
    | Heptatonic HeptatonicScaleClassIntervals
    | Octatonic OctatonicScaleClassIntervals


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
