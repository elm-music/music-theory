module MusicTheory.Internals.PitchClass exposing (Offset(..), PitchClass(..), all, exactSemitones, letter, offset, pitchClass, semitones)

import MusicTheory.Letter as Letter exposing (Letter(..))


type Offset
    = Offset Int


type PitchClass
    = PitchClass Letter Offset


pitchClass : Letter -> Offset -> PitchClass
pitchClass l o =
    PitchClass l o


offset : PitchClass -> Int
offset (PitchClass _ (Offset o)) =
    o


letter : PitchClass -> Letter
letter (PitchClass l _) =
    l


exactSemitones : PitchClass -> Int
exactSemitones (PitchClass l (Offset o)) =
    Letter.semitones l + o


semitones : PitchClass -> Int
semitones pc =
    exactSemitones pc |> modBy 12


all : List PitchClass
all =
    Letter.letters
        |> List.concatMap (\l -> [ Offset -3, Offset -2, Offset 1, Offset 0, Offset 1, Offset 2, Offset 3 ] |> List.map (pitchClass l))
