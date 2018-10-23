module MusicTheory.Pitch exposing (Pitch(..))

import MusicTheory.Internal.PitchClass as PitchClass exposing (Offset, PitchClass)


type Pitch
    = Pitch PitchClass Offset
