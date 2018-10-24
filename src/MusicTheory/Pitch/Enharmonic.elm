module MusicTheory.Pitch.Enharmonic exposing (simple)

import MusicTheory.Octave as Octave
import MusicTheory.Pitch as Pitch exposing (Pitch)
import MusicTheory.PitchClass.Enharmonic as Enharmonic


simple : Pitch -> Maybe Pitch
simple pitch =
    case Enharmonic.simpleWithOctaveOffset (Pitch.pitchClass pitch) of
        ( pc, octaveOffset ) ->
            Octave.add octaveOffset (Pitch.octave pitch)
                |> Maybe.map (\o -> Pitch.fromPitchClass o pc)
