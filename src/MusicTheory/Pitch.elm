module MusicTheory.Pitch exposing (Pitch, areEnharmonicEqual, fromPitchClass, map, semitones)

import MusicTheory.Internal.PitchClass as PitchClass exposing (Offset, PitchClass)
import MusicTheory.Octave as Octave exposing (Octave)


type Pitch
    = Pitch PitchClass Octave



-- todo: pitch ctor (pitch : Letter -> Accidental -> Octave -> Pitch)


fromPitchClass : Octave -> PitchClass -> Pitch
fromPitchClass o p =
    Pitch p o


{-| better name this mapPitchClass?
-}
map : (PitchClass -> PitchClass) -> Pitch -> Pitch
map f (Pitch pc o) =
    fromPitchClass o (f pc)


semitones : Pitch -> Int
semitones (Pitch pc o) =
    Octave.semitones o + PitchClass.semitonesNotOctaveBound pc


areEnharmonicEqual : Pitch -> Pitch -> Bool
areEnharmonicEqual lhs rhs =
    semitones lhs == semitones rhs
