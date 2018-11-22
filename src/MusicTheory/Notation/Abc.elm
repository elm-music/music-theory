module MusicTheory.Notation.Abc exposing (fromNotation)

import Libs.Ratio as Ratio exposing (Rational)
import List.Extra
import Maybe.Extra
import MusicTheory.Duration as Duration exposing (Duration)
import MusicTheory.Key as Key exposing (Key)
import MusicTheory.Letter as Letter
import MusicTheory.Notation as Notation exposing (..)
import MusicTheory.Octave as Octave
import MusicTheory.Pitch.Spelling exposing (PitchSpelling)
import MusicTheory.PitchClass.Spelling as Spelling exposing (Accidental(..))
import MusicTheory.TimeSignature as TimeSignature exposing (TimeSignature)
import MusicTheory.Tuplet as Tuplet


defaultUnitNoteLength : Duration
defaultUnitNoteLength =
    Duration.eighthNote



{- Example multiple voices single staff
   X: 1
   M: C
   %%score (1 2)
   V: 1 clef=treble
   V: 1 clef=treble
   K: F
   %
   [V: 1] A,CA,C B,CB,C-|Cz ([^D,2^F,2][E,G,][D,A,][E,2B,2]|
   [V: 2] F,4 [F,4G,4] |[F,A,] x x2 C,4 |
   %
   [V: 1] [F,A,]CA,C C,CB,C|
   [V: 2] x4 x4 |
-}


fromNotation : Notation PitchSpelling -> String
fromNotation notation =
    let
        score =
            notation.staffs
                |> List.indexedMap
                    (\staffNum staff ->
                        "(" ++ (staff.voices |> List.indexedMap (\voiceNum _ -> staffNum + voiceNum |> String.fromInt) |> String.join " ") ++ ")"
                    )
                |> String.join " "
                |> String.append "%%score "

        header =
            [ notation.title |> Maybe.map ((++) "T: ")
            , notation.composer |> Maybe.map ((++) "C: ")
            , notation.key |> fromKey |> Just
            , notation.timeSignature |> fromTimeSignature |> Just
            , notation.tempo |> Maybe.map (\( d, n ) -> fromTempo d n)
            ]
                |> Maybe.Extra.values
                |> String.join "\n"

        voices =
            notation.staffs
                |> List.indexedMap
                    (\staffNum staff ->
                        staff.voices |> List.indexedMap (\voiceNum voice -> "[V:" ++ (staffNum + voiceNum |> String.fromInt) ++ "]" ++ fromVoice defaultContext defaultUnitNoteLength voice)
                    )
                |> List.concat
    in
    [ header, score ]
        ++ voices
        |> String.join "\n"


type alias Context =
    { beamed : Bool }


defaultContext : Context
defaultContext =
    { beamed = False }


fromVoice : Context -> Duration -> Voice PitchSpelling -> String
fromVoice context unitNoteLength voice =
    let
        noteSeparator =
            if context.beamed then
                ""

            else
                " "
    in
    case voice of
        Line elements ->
            elements |> List.map (fromVoiceElement unitNoteLength) |> String.join noteSeparator

        Append v1 v2 ->
            fromVoice context unitNoteLength v1 ++ noteSeparator ++ fromVoice context unitNoteLength v2

        Attribute (Bar attrs) v ->
            fromVoice context unitNoteLength v ++ barLine attrs

        Attribute Beam v ->
            fromVoice { context | beamed = True } unitNoteLength v

        Attribute (Tuplet t) v ->
            case Tuplet.split t of
                ( x, y ) ->
                    "(" ++ String.fromInt x ++ ":" ++ String.fromInt y ++ ":" ++ String.fromInt (Notation.length v) ++ fromVoice context unitNoteLength v


barLine : List BarAttribute -> String
barLine attrs =
    if attrs |> List.member DoubleLine then
        " ||"

    else
        " |"


fromVoiceElement : Duration -> VoiceElement PitchSpelling -> String
fromVoiceElement unitNoteLength voice =
    case voice of
        Note note ->
            fromNote unitNoteLength note

        Rest rest ->
            fromRest unitNoteLength rest

        Chord chord ->
            fromChord unitNoteLength chord


fromChord : Duration -> Chord PitchSpelling -> String
fromChord unitNoteLength (ChordElement attributes duration elements) =
    ("["
        ++ String.join (fromDuration unitNoteLength duration) (List.map fromChordNote elements)
        ++ "]"
    )
        |> applyAttributes attributes


fromChordNote : ChordNote PitchSpelling -> String
fromChordNote (ChordNote attrs p) =
    fromPitch True p
        |> applyAttributes attrs


fromRest : Duration -> Rest -> String
fromRest unitNoteLength (RestElement attrs duration) =
    restType attrs
        ++ fromDuration unitNoteLength duration
        |> applyRestAttributes attrs


restType : List RestAttribute -> String
restType attrs =
    case attrs of
        [] ->
            "z"

        Invisible :: _ ->
            "y"


applyRestAttributes : List RestAttribute -> String -> String
applyRestAttributes _ v =
    v


fromNote : Duration -> Note PitchSpelling -> String
fromNote unitNoteLength (NoteElement attrs duration element) =
    fromPitch True element
        ++ fromDuration unitNoteLength duration
        |> applyAttributes attrs


applyAttributes : List NoteAttribute -> String -> String
applyAttributes attrs v =
    case attrs of
        [] ->
            v

        Tie :: attrsTail ->
            applyAttributes attrsTail v ++ "-"

        _ :: _ ->
            v


fromPitch : Bool -> PitchSpelling -> String
fromPitch showAccidental { letter, accidental, octave } =
    let
        acc =
            if showAccidental then
                case accidental of
                    Flat ->
                        "_"

                    Natural ->
                        "="

                    Sharp ->
                        "^"

            else
                ""
    in
    if Octave.number octave < 5 then
        acc ++ (letter |> Letter.toString) ++ (List.repeat (4 - Octave.number octave) ',' |> String.fromList)

    else
        acc ++ (letter |> Letter.toString |> String.toLower) ++ (List.repeat (Octave.number octave - 5) '\'' |> String.fromList)


rationalToString : Rational -> String
rationalToString =
    Ratio.split
        >> (\( x, y ) -> String.fromInt x ++ "/" ++ String.fromInt y)


fromDuration : Duration -> Duration -> String
fromDuration unitNoteLength duration =
    Ratio.divide (Duration.toRational duration) (Duration.toRational unitNoteLength)
        |> rationalToString


fromDurations : Duration -> Duration -> List String
fromDurations unitNoteLength duration =
    Duration.toList duration
        |> List.map (fromDuration unitNoteLength)


fromTimeSignature : TimeSignature -> String
fromTimeSignature timeSignature =
    "M: " ++ String.fromInt (TimeSignature.numberOfBeatsInt timeSignature) ++ "/" ++ String.fromInt (TimeSignature.beatValueInt timeSignature)


fromKey : Key -> String
fromKey key =
    let
        tonic =
            Key.tonic key |> Spelling.simple

        accidental =
            case tonic.accidental of
                Flat ->
                    "b"

                Natural ->
                    ""

                Sharp ->
                    "#"
    in
    "K: " ++ Letter.toString tonic.letter ++ accidental


fromTempo : Duration -> Float -> String
fromTempo duration bpm =
    "Q: " ++ rationalToString (Duration.toRational duration) ++ "=" ++ (bpm |> round |> String.fromInt)
