module MusicTheory.Pitch exposing (Pitch(..))

import MusicTheory.Internal.PitchClass as PitchClass exposing (Offset, PitchClass)
import MusicTheory.Octave as Octave exposing (Octave)


type Pitch
    = Pitch PitchClass Octave


pitch : PitchClass -> Octave -> Pitch
pitch =
    Pitch
