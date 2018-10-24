module MusicTheory.Pitch exposing
    ( Pitch
    , areEnharmonicEqual
    , fromPitchClass
    , octave
    , pitchClass
    , semitones
    )

import MusicTheory.Internal.PitchClass as PitchClass exposing (Offset, PitchClass)
import MusicTheory.Octave as Octave exposing (Octave)


type Pitch
    = Pitch PitchClass Octave



-- todo: pitch ctor (pitch : Letter -> Accidental -> Octave -> Pitch)


pitchClass : Pitch -> PitchClass
pitchClass (Pitch pc _) =
    pc


octave : Pitch -> Octave
octave (Pitch _ o) =
    o


fromPitchClass : Octave -> PitchClass -> Pitch
fromPitchClass o p =
    Pitch p o


semitones : Pitch -> Int
semitones (Pitch pc o) =
    Octave.semitones o + PitchClass.semitonesNotOctaveBound pc


areEnharmonicEqual : Pitch -> Pitch -> Bool
areEnharmonicEqual lhs rhs =
    semitones lhs == semitones rhs
