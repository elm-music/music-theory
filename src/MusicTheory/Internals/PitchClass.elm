module MusicTheory.Internals.PitchClass exposing (Offset(..), PitchClass(..), letter, offset)

import MusicTheory.Accidental exposing (Accidental(..))
import MusicTheory.Letter exposing (Letter(..))


type Offset
    = Offset Int


type PitchClass
    = PitchClass Letter Offset


offset : PitchClass -> Int
offset (PitchClass _ (Offset o)) =
    o


letter : PitchClass -> Letter
letter (PitchClass l _) =
    l
