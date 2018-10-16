module MusicTheory.PitchClass.Spelling exposing
    ( Accidental(..)
    , PitchClassSpelling
    , naturalOrElseFlat
    , naturalOrElseSharp
    , simple
    , toPitchClass
    , toString
    )

import MusicTheory.Internals.PitchClass as Internal
import MusicTheory.Letter as Letter exposing (Letter(..))
import MusicTheory.PitchClass as PitchClass exposing (PitchClass)


{-| An accidental describes by how many semitones a letter is raised or lowered.
-}
type Accidental
    = Flat
    | Natural
    | Sharp


type alias PitchClassSpelling =
    { letter : Letter
    , accidental : Accidental
    }


simple : PitchClass -> PitchClassSpelling
simple pitchClass =
    if Internal.offset pitchClass == 0 then
        { letter = Internal.letter pitchClass, accidental = Natural }

    else if Internal.offset pitchClass < 0 then
        naturalOrElseFlat pitchClass

    else
        naturalOrElseSharp pitchClass


{-| String representation of a letter and an accidental.

    toString ( D, Sharp ) == "D♯"

-}
toString : PitchClassSpelling -> String
toString { letter, accidental } =
    Letter.toString letter ++ accidentalToString accidental


{-| Create a pitch class from a tuple of a letter and an accidental.

    toPitchClass ( G, Flat ) -- creates the pitch class G♭

-}
toPitchClass : PitchClassSpelling -> PitchClass
toPitchClass { letter, accidental } =
    PitchClass.pitchClass letter (accidentalToOffset accidental)


{-| Returns the enharmonic equivalent pitch class expressed as a note from the diatonic C major scale that is natural or lowered once.

    naturalOrElseFlat (pitchClass F DoubleSharp) == ( G, Natural )

    naturalOrElseFlat (pitchClass C TripleSharp) == ( E, Flat )

-}
naturalOrElseFlat : PitchClass -> PitchClassSpelling
naturalOrElseFlat pitchClass =
    case pitchClass |> PitchClass.semitones |> semitonesToNaturalOrAccidental of
        Nat letter ->
            { letter = letter, accidental = Natural }

        SharpFlat _ letter ->
            { letter = letter, accidental = Flat }


{-| Returns the enharmonic equivalent pitch class expressed as a note from the diatonic C major scale that is natural or raised once

    naturalOrElseSharp (pitchClass F DoubleSharp) == ( G, Natural )

    naturalOrElseSharp (pitchClass C TripleSharp) == ( D, Sharp )

-}
naturalOrElseSharp : PitchClass -> PitchClassSpelling
naturalOrElseSharp pitchClass =
    case pitchClass |> PitchClass.semitones |> semitonesToNaturalOrAccidental of
        Nat letter ->
            { letter = letter, accidental = Natural }

        SharpFlat letter _ ->
            { letter = letter, accidental = Sharp }



-- INTERNALS


accidentalToOffset : Accidental -> Internal.Offset
accidentalToOffset accidental =
    case accidental of
        Flat ->
            Internal.Offset -1

        Natural ->
            Internal.Offset 0

        Sharp ->
            Internal.Offset 1


accidentalToString : Accidental -> String
accidentalToString accidental =
    case accidental of
        Flat ->
            "♭"

        Natural ->
            ""

        Sharp ->
            "♯"


offsetToAccidental : Internal.Offset -> Maybe Accidental
offsetToAccidental (Internal.Offset offset) =
    if offset == -1 then
        Just Flat

    else if offset == 0 then
        Just Natural

    else if offset == 1 then
        Just Sharp

    else
        Nothing


type NaturalOrAccidental
    = Nat Letter
    | SharpFlat Letter Letter


semitonesToNaturalOrAccidental : Int -> NaturalOrAccidental
semitonesToNaturalOrAccidental offset =
    if offset == -3 then
        Nat A

    else if offset == -2 then
        SharpFlat A B

    else if offset == -1 then
        Nat B

    else if offset == 0 then
        Nat C

    else if offset == 1 then
        SharpFlat C D

    else if offset == 2 then
        Nat D

    else if offset == 3 then
        SharpFlat D E

    else if offset == 4 then
        Nat E

    else if offset == 5 then
        Nat F

    else if offset == 6 then
        SharpFlat F G

    else if offset == 7 then
        Nat G

    else if offset == 8 then
        SharpFlat G A

    else if offset == 9 then
        Nat A

    else if offset == 10 then
        SharpFlat A B

    else if offset == 11 then
        Nat B

    else if offset == 12 then
        Nat C

    else if offset == 13 then
        SharpFlat C D

    else if offset == 14 then
        Nat D

    else if offset > 14 then
        semitonesToNaturalOrAccidental (offset - 12)

    else
        semitonesToNaturalOrAccidental (offset + 12)
