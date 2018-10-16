module MusicTheory.PitchClass.Spelling exposing
    ( Accidental(..)
    , PitchClassSpelling
    , simple
    , toPitchClass
    , toString
    )

import MusicTheory.Internals.PitchClass as Internal
import MusicTheory.Letter exposing (Letter(..))
import MusicTheory.PitchClass as PitchClass exposing (PitchClass)


{-| An accidental describes by how many semitones a letter is raised or lowered.
-}
type Accidental
    = TripleFlat
    | DoubleFlat
    | Flat
    | Natural
    | Sharp
    | DoubleSharp
    | TripleSharp


type alias PitchClassSpelling =
    { letter : Letter
    , accidental : Accidental
    }


simple : PitchClass -> PitchClassSpelling
simple pitchClass =
    if Internal.offset pitchClass == 0 then
        { letter = Internal.letter pitchClass, accidental = Natural }

    else if Internal.offset pitchClass < 0 then
        naturalOrSingleFlat pitchClass

    else
        naturalOrSingleSharp pitchClass


{-| String representation of a letter and an accidental.

    toString ( D, Sharp ) == "D♯"

-}
toString : PitchClassSpelling -> String
toString { letter, accidental } =
    letterToString letter ++ accidentalToString accidental


{-| Create a pitch class from a tuple of a letter and an accidental.

    toPitchClass ( G, Flat ) -- creates the pitch class G♭

-}
toPitchClass : PitchClassSpelling -> PitchClass
toPitchClass { letter, accidental } =
    PitchClass.pitchClass letter (accidentalToOffset accidental)



-- INTERNALS


{-| Returns the enharmonic equivalent pitch class expressed as a note from the diatonic C major scale that is natural or lowered once.

    naturalOrSingleFlat (pitchClass F DoubleSharp) == ( G, Natural )

    naturalOrSingleFlat (pitchClass C TripleSharp) == ( E, Flat )

-}
naturalOrSingleFlat : PitchClass -> PitchClassSpelling
naturalOrSingleFlat pitchClass =
    case pitchClass |> PitchClass.semitones |> semitonesToNaturalOrAccidental of
        Nat letter ->
            { letter = letter, accidental = Natural }

        SharpFlat _ letter ->
            { letter = letter, accidental = Flat }


{-| Returns the enharmonic equivalent pitch class expressed as a note from the diatonic C major scale that is natural or raised once

    naturalOrSingleSharp (pitchClass F DoubleSharp) == ( G, Natural )

    naturalOrSingleSharp (pitchClass C TripleSharp) == ( D, Sharp )

-}
naturalOrSingleSharp : PitchClass -> PitchClassSpelling
naturalOrSingleSharp pitchClass =
    case pitchClass |> PitchClass.semitones |> semitonesToNaturalOrAccidental of
        Nat letter ->
            { letter = letter, accidental = Natural }

        SharpFlat letter _ ->
            { letter = letter, accidental = Sharp }


accidentalToOffset : Accidental -> Internal.Offset
accidentalToOffset accidental =
    case accidental of
        TripleFlat ->
            Internal.Offset -3

        DoubleFlat ->
            Internal.Offset -2

        Flat ->
            Internal.Offset -1

        Natural ->
            Internal.Offset 0

        Sharp ->
            Internal.Offset 1

        DoubleSharp ->
            Internal.Offset 2

        TripleSharp ->
            Internal.Offset 3


letterToString : Letter -> String
letterToString letter =
    case letter of
        C ->
            "C"

        D ->
            "D"

        E ->
            "E"

        F ->
            "F"

        G ->
            "G"

        A ->
            "A"

        B ->
            "B"


accidentalToString : Accidental -> String
accidentalToString accidental =
    case accidental of
        TripleFlat ->
            "♭𝄫"

        DoubleFlat ->
            "𝄫"

        Flat ->
            "♭"

        Natural ->
            ""

        Sharp ->
            "♯"

        DoubleSharp ->
            "𝄪"

        TripleSharp ->
            "♯𝄪"


offsetToAccidental : Internal.Offset -> Maybe Accidental
offsetToAccidental (Internal.Offset offset) =
    if offset == -3 then
        Just TripleFlat

    else if offset == -2 then
        Just DoubleFlat

    else if offset == -1 then
        Just Flat

    else if offset == 0 then
        Just Natural

    else if offset == 1 then
        Just Sharp

    else if offset == 2 then
        Just DoubleSharp

    else if offset == 3 then
        Just TripleSharp

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
