module MusicTheory.Internal.Pitch exposing
    ( Pitch
    , PitchError(..)
    , all
    , areEnharmonicEqual
    , doubleFlat
    , doubleSharp
    , errorToString
    , flat
    , fromPitchClass
    , natural
    , octave
    , pitch
    , pitchClass
    , semitones
    , sharp
    , tripleFlat
    , tripleSharp
    )

import MusicTheory.Internal.PitchClass as PitchClass exposing (Offset, PitchClass)
import MusicTheory.Letter as Letter exposing (Letter)
import MusicTheory.Octave as Octave exposing (Octave, OctaveError(..))


type PitchError
    = InvalidEnharmonicEquivalent PitchClass OctaveError


errorToString : PitchError -> String
errorToString error =
    case error of
        InvalidEnharmonicEquivalent pc octaveError ->
            let
                letter =
                    PitchClass.letter pc

                offset =
                    PitchClass.offset pc

                accidental =
                    if offset == 0 then
                        " natural"

                    else if offset < 0 then
                        " with " ++ String.fromInt (abs offset) ++ " flats"

                    else
                        " with " ++ String.fromInt (abs offset) ++ " sharps"
            in
            "Pitch class " ++ Letter.toString letter ++ accidental ++ " has an invalid octave. " ++ Octave.errorToString octaveError


type Pitch
    = Pitch PitchClass Octave


pitch : Letter -> Offset -> Octave -> Pitch
pitch l os o =
    fromPitchClass o (PitchClass.pitchClass l os)


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


all : List Pitch
all =
    Octave.all
        |> List.concatMap (\o -> PitchClass.all |> List.map (\pc -> Pitch pc o))
