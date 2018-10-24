module MusicTheory.Pitch.Enharmonic exposing (asNaturalOrElseFlat, asNaturalOrElseSharp, simple)

import MusicTheory.Internal.Pitch as Pitch exposing (Pitch, PitchError(..))
import MusicTheory.Internal.PitchClass exposing (PitchClass)
import MusicTheory.Octave as Octave
import MusicTheory.PitchClass.Enharmonic as Enharmonic


fromPitchClassAndOctaveOffset : Pitch -> ( PitchClass, Int ) -> Result PitchError Pitch
fromPitchClassAndOctaveOffset pitch ( pc, octaveOffset ) =
    Octave.add octaveOffset (Pitch.octave pitch)
        |> Result.map (\o -> Pitch.fromPitchClass o pc)
        |> Result.mapError (InvalidEnharmonicEquivalent pc)


simple : Pitch -> Result PitchError Pitch
simple pitch =
    Enharmonic.simpleWithOctaveOffset (Pitch.pitchClass pitch)
        |> fromPitchClassAndOctaveOffset pitch


asNaturalOrElseSharp : Pitch -> Result PitchError Pitch
asNaturalOrElseSharp pitch =
    Enharmonic.asNaturalOrElseSharpWithOctaveOffset (Pitch.pitchClass pitch)
        |> fromPitchClassAndOctaveOffset pitch


asNaturalOrElseFlat : Pitch -> Result PitchError Pitch
asNaturalOrElseFlat pitch =
    Enharmonic.asNaturalOrElseFlatWithOctaveOffset (Pitch.pitchClass pitch)
        |> fromPitchClassAndOctaveOffset pitch
