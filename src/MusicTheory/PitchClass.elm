module MusicTheory.PitchClass exposing
    ( PitchClass
    , all
    , areEnharmonicEqual
    , doubleFlat, doubleSharp, flat, fromTuple, natural, pitchClass, semitones, sharp, transposeBySemitones, transposeDown, transposeUp, tripleFlat, tripleSharp
    )

{-| A pitch class is a set of all pitches that are a whole number of octaves apart. A pitch class is represented as a letter together with an accidental.

The internals of `PitchClass` are opaque. By using accessor functions the caller can get specific enharmonic equivalent pitch classes depending on their needs.


# Definition

@docs PitchClass


# Constructors

@docs pitchClass fromTuple


# Accessors

@docs all


# Transform

@docs transposeUp transposeDown transposeBySemitones semitones


# Comparison

@docs areEnharmonicEqual

-}

import MusicTheory.Accidental exposing (Accidental(..))
import MusicTheory.Internals.PitchClass as Internal
import MusicTheory.Interval as Interval exposing (Interval, IntervalNumber(..), IntervalQuality(..))
import MusicTheory.Letter exposing (Letter(..))



-- DEFINITION


{-| Internal representation of an unlimited raised or lowered letter. E.g. four flats are represented as as `Offset -4`.
-}
type alias Offset =
    Internal.Offset


{-| Opaque type that represents a pitch class.
-}
type alias PitchClass =
    Internal.PitchClass



-- CONSTRUCTORS


tripleFlat : Offset
tripleFlat =
    Internal.Offset -3


doubleFlat : Offset
doubleFlat =
    Internal.Offset -2


flat : Offset
flat =
    Internal.Offset -1


natural : Offset
natural =
    Internal.Offset 0


sharp : Offset
sharp =
    Internal.Offset 1


doubleSharp : Offset
doubleSharp =
    Internal.Offset 2


tripleSharp : Offset
tripleSharp =
    Internal.Offset 3


{-| Create a pitch class from a letter and an accidental.

    pitchClass C Sharp -- creates the pitch class C♯

-}
pitchClass : Letter -> Offset -> PitchClass
pitchClass letter offset =
    Internal.PitchClass letter offset


{-| Create a pitch class from a tuple of a letter and an accidental.

    fromTuple ( G, Flat ) -- creates the pitch class G♭

-}
fromTuple : ( Letter, Accidental ) -> PitchClass
fromTuple ( letter, accidental ) =
    pitchClass letter (accidentalToOffset accidental)



-- ACCESSORS


{-| A list of all pitch classes that can be represented in terms of `Letter` and an `Accidental`.
-}
all : List PitchClass
all =
    letters
        |> List.concatMap (\l -> [ tripleFlat, doubleFlat, flat, natural, sharp, doubleSharp, tripleSharp ] |> List.map (pitchClass l))



-- TRANSFORM


{-| Number of semitones between C and a given pitch class.

    semitones (pitchClass E Natural) == 4

-}
semitones : PitchClass -> Int
semitones pc =
    exactSemitones pc |> modBy 12



-- TRANSPOSE


{-| Moves a pitch class up by a given interval while taking the correct number off staff positions between root and target pitch class into account.

    (pitchClass C Sharp |> transposeUp Interval.majorSecond |> exact) == Just (D, Sharp)

    The result might not be representable in terms of the correct letter and a valid accidental. In this case a suitable enharmonic representation can be retrieved by applying `asNaturalOrLoweredOnce` or `asNaturalOrRaisedOnce`.

    (pitchClass C TripleFlat |> transposeUp Interval.minorSecond |> exact) == Nothing

    (pitchClass C TripleFlat |> transposeUp Interval.minorSecond |> asNaturalOrLoweredOnce) == (B, Flat)

-}
transposeUp : Interval -> PitchClass -> PitchClass
transposeUp interval pc =
    let
        ( targetLetter, letterToLetterDistance ) =
            targetLetterWithSemitoneDistance (letterIndex (Internal.letter pc)) (intervalNumberIndex (Interval.number interval)) ( Internal.letter pc, 0 )
    in
    Internal.PitchClass targetLetter (Internal.Offset (Interval.semitones interval - letterToLetterDistance + Internal.offset pc))


{-| Moves a pitch class down by a given interval while taking the correct number off staff positions between root and target pitch class into account.

    (pitchClass B Natural |> transposeDown Interval.minorSecond) == pitchClass A Sharp

-}
transposeDown : Interval -> PitchClass -> PitchClass
transposeDown interval pc =
    interval
        |> Interval.complementary
        |> (\i -> transposeUp i pc)


{-| Moves a pitch class by a given number of semitones. The result will be ambiguous because while transposing by semitones alone it cannot be determined which enharmonic equivalent pitch class to choose as the result. An enharmonic equivalent representation can be retrieved by applying `asNaturalOrLoweredOnce` or `asNaturalOrRaisedOnce`.

    (pitchClass C Natural |> transposeBySemitones 10 |> asNaturalOrLoweredOnce) == ( B, Flat )

    (pitchClass C Natural |> transposeBySemitones 10 |> asNaturalOrRaisedOnce) == ( A, Sharp )

-}
transposeBySemitones : Int -> PitchClass -> PitchClass
transposeBySemitones n (Internal.PitchClass letter (Internal.Offset offset)) =
    Internal.PitchClass letter (Internal.Offset (offset + n))



-- COMPARISON


{-| Returns true if two pitch classes are enharmonic equivalent.
-}
areEnharmonicEqual : PitchClass -> PitchClass -> Bool
areEnharmonicEqual lhs rhs =
    semitones lhs == semitones rhs



-- INTERNALS


exactSemitones : PitchClass -> Int
exactSemitones (Internal.PitchClass letter (Internal.Offset offset)) =
    letterSemitones letter + offset


accidentalToOffset : Accidental -> Offset
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


letters : List Letter
letters =
    [ C, D, E, F, G, A, B, C ]


letterSemitones : Letter -> Int
letterSemitones letter =
    case letter of
        C ->
            0

        D ->
            2

        E ->
            4

        F ->
            5

        G ->
            7

        A ->
            9

        B ->
            11


letterIndex : Letter -> Int
letterIndex letter =
    case letter of
        C ->
            0

        D ->
            1

        E ->
            2

        F ->
            3

        G ->
            4

        A ->
            5

        B ->
            6


intervalNumberIndex : IntervalNumber -> Int
intervalNumberIndex intervalNumber =
    case intervalNumber of
        Unison ->
            0

        Second ->
            1

        Third ->
            2

        Fourth ->
            3

        Fifth ->
            4

        Sixth ->
            5

        Seventh ->
            6

        Octave ->
            7


letterAndSemitoneStepsByIndex : Int -> ( Letter, Int )
letterAndSemitoneStepsByIndex n =
    case n of
        0 ->
            ( C, 1 )

        1 ->
            ( D, 2 )

        2 ->
            ( E, 2 )

        3 ->
            ( F, 1 )

        4 ->
            ( G, 2 )

        5 ->
            ( A, 2 )

        6 ->
            ( B, 2 )

        other ->
            if other < 0 then
                letterAndSemitoneStepsByIndex (other + 7)

            else
                letterAndSemitoneStepsByIndex (other - 7)


targetLetterWithSemitoneDistance : Int -> Int -> ( Letter, Int ) -> ( Letter, Int )
targetLetterWithSemitoneDistance currentIndex steps ( currentLetter, totalSemitones ) =
    if steps <= 0 then
        ( currentLetter, totalSemitones )

    else
        let
            ( currentTargetLetter, stepSemitones ) =
                letterAndSemitoneStepsByIndex (currentIndex + 1)
        in
        targetLetterWithSemitoneDistance (currentIndex + 1) (steps - 1) ( currentTargetLetter, totalSemitones + stepSemitones )
