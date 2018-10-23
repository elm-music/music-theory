module MusicTheory.PitchClass.Enharmonic exposing (asNaturalOrElseFlat, asNaturalOrElseSharp, equivalents, simple)

import MusicTheory.Internal.PitchClass as PitchClassInternal exposing (Offset(..), PitchClass(..))
import MusicTheory.Internal.PitchClass.Enharmonic as EnharmonicInternal exposing (NaturalOrSingleAccidental(..))


simple : PitchClass -> PitchClass
simple pc =
    if PitchClassInternal.offset pc == 0 then
        pc

    else if PitchClassInternal.offset pc < 0 then
        asNaturalOrElseFlat pc

    else
        asNaturalOrElseSharp pc


asNaturalOrElseFlat : PitchClass -> PitchClass
asNaturalOrElseFlat pc =
    case pc |> PitchClassInternal.semitones |> EnharmonicInternal.semitonesToNaturalOrAccidental of
        Nat letter ->
            PitchClassInternal.pitchClass letter (Offset 0)

        SharpFlat _ letter ->
            PitchClassInternal.pitchClass letter (Offset -1)


asNaturalOrElseSharp : PitchClass -> PitchClass
asNaturalOrElseSharp pc =
    case pc |> PitchClassInternal.semitones |> EnharmonicInternal.semitonesToNaturalOrAccidental of
        Nat letter ->
            PitchClassInternal.pitchClass letter (Offset 0)

        SharpFlat letter _ ->
            PitchClassInternal.pitchClass letter (Offset 1)


equivalents : PitchClass -> List PitchClass
equivalents pc =
    PitchClassInternal.all |> List.filter (PitchClassInternal.semitones >> (==) (PitchClassInternal.semitones pc))
