module MusicTheory.Pitch.Enharmonic exposing (simple)

import MusicTheory.Internal.Pitch as Pitch exposing (Pitch, PitchError(..))
import MusicTheory.Octave as Octave
import MusicTheory.PitchClass.Enharmonic as Enharmonic


simple : Pitch -> Result PitchError Pitch
simple pitch =
    case Enharmonic.simpleWithOctaveOffset (Pitch.pitchClass pitch) of
        ( pc, octaveOffset ) ->
            Octave.add octaveOffset (Pitch.octave pitch)
                |> Result.map (\o -> Pitch.fromPitchClass o pc)
                |> Result.mapError (InvalidEnharmonicEquivalent pc)
