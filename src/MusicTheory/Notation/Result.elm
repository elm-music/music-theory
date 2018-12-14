module MusicTheory.Notation.Result exposing (combine, sequence, traverse)

import List.Extra
import MusicTheory.Notation exposing (..)


chordNoteTraverse : (a -> Result e b) -> ChordNote a -> Result e (ChordNote b)
chordNoteTraverse f (ChordNote attrs a) =
    Result.map (ChordNote attrs) (f a)


listTraverse : (a -> Result e b) -> List a -> Result e (List b)
listTraverse f =
    List.foldr (f >> Result.map2 (::)) (Ok [])


voiceElementTraverse : (a -> Result e b) -> VoiceElement a -> Result e (VoiceElement b)
voiceElementTraverse f voiceElement =
    case voiceElement of
        Note (NoteElement attrs duration a) ->
            Result.map (NoteElement attrs duration >> Note) (f a)

        Rest r ->
            Ok (Rest r)

        Chord (ChordElement attrs duration chordNotes) ->
            chordNotes |> listTraverse (chordNoteTraverse f) |> Result.map (ChordElement attrs duration >> Chord)


voiceTraverse : (a -> Result e b) -> Voice a -> Result e (Voice b)
voiceTraverse f voice =
    case voice of
        Line elements ->
            Result.map Line (elements |> listTraverse (voiceElementTraverse f))

        Append v1 v2 ->
            Result.map2 Append (voiceTraverse f v1) (voiceTraverse f v2)

        Attribute c v ->
            Result.map (Attribute c) (voiceTraverse f v)


staffTraverse : (a -> Result e b) -> Staff a -> Result e (Staff b)
staffTraverse f s =
    Result.map (Staff s.clef) (listTraverse (voiceTraverse f) s.voices)


traverse : (a -> Result e b) -> Notation a -> Result e (Notation b)
traverse f n =
    Result.map (Notation n.title n.composer n.key n.timeSignature n.tempo) (listTraverse (staffTraverse f) n.staffs)


sequence : Notation (Result e a) -> Result e (Notation a)
sequence =
    traverse identity


combine : Notation (Result e a) -> Result e (Notation a)
combine =
    sequence
