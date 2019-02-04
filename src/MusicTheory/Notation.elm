module MusicTheory.Notation exposing
    ( BarAttribute(..)
    , Bpm
    , Chord(..)
    , ChordNote(..)
    , Clef(..)
    , Notation
    , Note(..)
    , NoteAttribute(..)
    , Rest(..)
    , RestAttribute(..)
    , Staff
    , Voice(..)
    , VoiceAttribute(..)
    , VoiceElement(..)
    , bar
    , bass
    , beam
    , chord
    , chordNote
    , doubleLine
    , fromVoice
    , length
    , line
    , map
    , modify
    , note
    , rest
    , staff
    , staffMap
    , tie
    , treble
    , tuplet
    , voiceDuration
    , voiceElementDuration
    , voiceMap
    )

import Libs.Ratio as Ratio exposing (Rational)
import MusicTheory.Duration as Duration exposing (Duration)
import MusicTheory.Key as Key exposing (Key)
import MusicTheory.Letter exposing (Letter(..))
import MusicTheory.Notation.Decoration as Decoration exposing (..)
import MusicTheory.Octave exposing (Octave)
import MusicTheory.Pitch as Pitch exposing (Pitch)
import MusicTheory.Pitch.Spelling exposing (PitchSpelling)
import MusicTheory.PitchClass as PitchClass
import MusicTheory.PitchClass.Spelling exposing (Accidental(..))
import MusicTheory.TimeSignature as TimeSignature exposing (TimeSignature)
import MusicTheory.Tuplet as Tuplet exposing (Tuplet)


type alias Bpm =
    Float


type NoteAttribute
    = Tie
    | NoteDynamic Dynamic
    | Articulation Articulation
    | Ornament Ornament
    | NoteHead NoteHead
    | ChordSymbol ChordSymbol
    | Lyrics Lyrics


type Note a
    = NoteElement (List NoteAttribute) Duration a


type RestAttribute
    = Invisible


type Rest
    = RestElement (List RestAttribute) Duration


type ChordNote a
    = ChordNote (List NoteAttribute) a


type Chord a
    = ChordElement (List NoteAttribute) Duration (List (ChordNote a))


type Clef
    = Treble
    | Bass


type BarAttribute
    = DoubleLine


type VoiceAttribute
    = Tuplet Tuplet
    | Beam
    | Bar (List BarAttribute)


type VoiceElement a
    = Note (Note a)
    | Rest Rest
    | Chord (Chord a)


type Voice a
    = Line (List (VoiceElement a))
    | Append (Voice a) (Voice a)
    | Attribute VoiceAttribute (Voice a)


type alias Staff a =
    { clef : Clef
    , voices : List (Voice a)
    }


type alias Notation a =
    { title : Maybe String
    , composer : Maybe String
    , key : Key
    , timeSignature : TimeSignature
    , tempo : Maybe ( Duration, Bpm )
    , staffs : List (Staff a)
    }



-- CONSTRUCTORS


note : List NoteAttribute -> a -> Duration -> Voice a
note attrs a duration =
    NoteElement attrs duration a
        |> Note
        |> List.singleton
        |> Line


rest : List RestAttribute -> Duration -> Voice a
rest attrs duration =
    RestElement attrs duration
        |> Rest
        |> List.singleton
        |> Line


line : List (Voice a) -> Voice a
line voices =
    let
        fold v1 v2 =
            case ( v1, v2 ) of
                ( Line es1, Line es2 ) ->
                    Line (es1 ++ es2)

                _ ->
                    Append v1 v2
    in
    List.foldr fold (Line []) voices


chordNote : List NoteAttribute -> a -> ChordNote a
chordNote attrs a =
    ChordNote attrs a


chord : List NoteAttribute -> List (ChordNote a) -> Duration -> Voice a
chord attrs chordNotes duration =
    ChordElement attrs duration chordNotes
        |> Chord
        |> List.singleton
        |> Line


modify : VoiceAttribute -> Voice a -> Voice a
modify c voice =
    Attribute c voice


tuplet : Tuplet -> List (Voice a) -> Voice a
tuplet t =
    line >> modify (Tuplet t)


beam : List (Voice a) -> Voice a
beam =
    line >> Attribute Beam


bar : List BarAttribute -> List (Voice a) -> Voice a
bar attrs =
    line >> modify (Bar attrs)


staff : Clef -> List (Voice a) -> Staff a
staff cl voices =
    { clef = cl, voices = voices }


treble : Clef
treble =
    Treble


bass : Clef
bass =
    Bass


fromVoice : Voice a -> Notation a
fromVoice voice =
    { title = Nothing
    , composer = Nothing
    , key = Key.cMajor
    , timeSignature = TimeSignature.four TimeSignature.quarters
    , tempo = Nothing
    , staffs = [ staff treble [ voice ] ]
    }



-- ATTRIBUTES


tie : NoteAttribute
tie =
    Tie


doubleLine : BarAttribute
doubleLine =
    DoubleLine



-- FUNCTIONS


map : (a -> b) -> Notation a -> Notation b
map f n =
    { title = n.title
    , composer = n.composer
    , key = n.key
    , timeSignature = n.timeSignature
    , tempo = n.tempo
    , staffs = n.staffs |> List.map (staffMap f)
    }


staffMap : (a -> b) -> Staff a -> Staff b
staffMap f s =
    { clef = s.clef, voices = s.voices |> List.map (voiceMap f) }


voiceMap : (a -> b) -> Voice a -> Voice b
voiceMap f voice =
    case voice of
        Line elements ->
            Line (elements |> List.map (elMap f))

        Append v1 v2 ->
            Append (voiceMap f v1) (voiceMap f v2)

        Attribute c v ->
            Attribute c (voiceMap f v)


chordNoteMap : (a -> b) -> ChordNote a -> ChordNote b
chordNoteMap f (ChordNote attrs a) =
    ChordNote attrs (f a)


elMap : (a -> b) -> VoiceElement a -> VoiceElement b
elMap f el =
    case el of
        Note (NoteElement attrs duration a) ->
            Note (NoteElement attrs duration (f a))

        Rest r ->
            Rest r

        Chord (ChordElement attrs duration chordNotes) ->
            Chord (ChordElement attrs duration (chordNotes |> List.map (chordNoteMap f)))


restElementDuration : Rest -> Rational
restElementDuration (RestElement _ duration) =
    Duration.toRational duration


voiceElementDuration : VoiceElement a -> Rational
voiceElementDuration voiceElement =
    case voiceElement of
        Note (NoteElement _ duration _) ->
            Duration.toRational duration

        Rest (RestElement _ duration) ->
            Duration.toRational duration

        Chord (ChordElement _ duration _) ->
            Duration.toRational duration


voiceDuration : Voice a -> Rational
voiceDuration voice =
    case voice of
        Line elements ->
            elements |> List.foldl (\a b -> Ratio.add (voiceElementDuration a) b) (Ratio.over 0 1)

        Append v1 v2 ->
            Ratio.add (voiceDuration v1) (voiceDuration v2)

        Attribute (Tuplet t) v ->
            Ratio.divide (voiceDuration v) (Tuplet.toRational t)

        Attribute _ v ->
            voiceDuration v


length : Voice a -> Int
length voice =
    case voice of
        Line elements ->
            elements |> List.length

        Append v1 v2 ->
            length v1 + length v2

        Attribute _ v ->
            length v
