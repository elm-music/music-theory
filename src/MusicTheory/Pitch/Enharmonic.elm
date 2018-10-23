module MusicTheory.Pitch.Enharmonic exposing (simple)

import MusicTheory.Pitch as Pitch exposing (Pitch)
import MusicTheory.PitchClass.Enharmonic as PitchClassEnharmonic


simple : Pitch -> Pitch
simple pitch =
    pitch |> Pitch.map PitchClassEnharmonic.simple
