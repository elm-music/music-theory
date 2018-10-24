module MusicTheory.PitchClass.Enharmonic exposing (asNaturalOrElseFlat, asNaturalOrElseSharp, equivalents, simple, simpleWithOctaveOffset)

import MusicTheory.Internal.PitchClass as PitchClass exposing (Offset, PitchClass(..))
import MusicTheory.Internal.PitchClass.Enharmonic as Enharmonic exposing (NaturalOrSingleAccidental(..))


simpleWithOctaveOffset : PitchClass -> ( PitchClass, Int )
simpleWithOctaveOffset pc =
    let
        semitonesNotOctaveBound =
            PitchClass.semitonesNotOctaveBound pc

        octaveOffset =
            if semitonesNotOctaveBound >= 0 then
                semitonesNotOctaveBound // 12

            else
                (semitonesNotOctaveBound - 12) // 12
    in
    ( simple pc, octaveOffset )


simple : PitchClass -> PitchClass
simple pc =
    if PitchClass.offset pc == 0 then
        pc

    else if PitchClass.offset pc < 0 then
        asNaturalOrElseFlat pc

    else
        asNaturalOrElseSharp pc


asNaturalOrElseFlat : PitchClass -> PitchClass
asNaturalOrElseFlat pc =
    case pc |> PitchClass.semitones |> Enharmonic.semitonesToNaturalOrAccidental of
        Nat letter ->
            PitchClass.pitchClass letter PitchClass.natural

        SharpFlat _ letter ->
            PitchClass.pitchClass letter PitchClass.flat


asNaturalOrElseSharp : PitchClass -> PitchClass
asNaturalOrElseSharp pc =
    case pc |> PitchClass.semitones |> Enharmonic.semitonesToNaturalOrAccidental of
        Nat letter ->
            PitchClass.pitchClass letter PitchClass.natural

        SharpFlat letter _ ->
            PitchClass.pitchClass letter PitchClass.sharp


equivalents : PitchClass -> List PitchClass
equivalents pc =
    PitchClass.all |> List.filter (PitchClass.semitones >> (==) (PitchClass.semitones pc))
