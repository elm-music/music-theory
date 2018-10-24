module MusicTheory.Pitch exposing
    ( Pitch
    , areEnharmonicEqual
    , doubleFlat
    , doubleSharp
    , flat
    , fromPitchClass
    , natural
    , octave
    , pitch
    , pitchClass
    , semitones
    , sharp
    , toString
    , tripleFlat
    , tripleSharp
    )

import MusicTheory.Internal.Pitch as Pitch exposing (PitchError(..))
import MusicTheory.Internal.PitchClass as PitchClass exposing (Offset, PitchClass)
import MusicTheory.Letter exposing (Letter)
import MusicTheory.Octave as Octave exposing (Octave, OctaveError(..))
import MusicTheory.Pitch.Enharmonic as Enharmonic


type alias Pitch =
    Pitch.Pitch


pitch : Letter -> Offset -> Octave -> Pitch
pitch =
    Pitch.pitch


tripleFlat : Offset
tripleFlat =
    PitchClass.tripleFlat


doubleFlat : Offset
doubleFlat =
    PitchClass.doubleFlat


flat : Offset
flat =
    PitchClass.flat


natural : Offset
natural =
    PitchClass.natural


sharp : Offset
sharp =
    PitchClass.sharp


doubleSharp : Offset
doubleSharp =
    PitchClass.doubleSharp


tripleSharp : Offset
tripleSharp =
    PitchClass.tripleSharp


pitchClass : Pitch -> PitchClass
pitchClass =
    Pitch.pitchClass


octave : Pitch -> Octave
octave =
    Pitch.octave


fromPitchClass : Octave -> PitchClass -> Pitch
fromPitchClass =
    Pitch.fromPitchClass


semitones : Pitch -> Int
semitones =
    Pitch.semitones


areEnharmonicEqual : Pitch -> Pitch -> Bool
areEnharmonicEqual =
    Pitch.areEnharmonicEqual


toString : Pitch -> String
toString pc =
    case pc |> Enharmonic.simple of
        Ok enharmonic ->
            (pitchClass enharmonic |> PitchClass.toString) ++ String.fromInt (octave enharmonic |> Octave.number)

        Err (InvalidEnharmonicEquivalent thePitchClass (AboveValidRange o)) ->
            PitchClass.toString thePitchClass ++ String.fromInt o

        Err (InvalidEnharmonicEquivalent thePitchClass (BelowValidRange o)) ->
            PitchClass.toString thePitchClass ++ String.fromInt o
