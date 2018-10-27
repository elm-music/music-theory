module MusicTheory.NoteOrTuplet exposing
    ( NoteOrTuplet
    , duplet
    , eighth
    , half
    , mapAll
    , mapFifth
    , mapFirst
    , mapFourth
    , mapSecond
    , mapThird
    , note
    , oneHundredTwentyEighth
    , quadruplet
    , quintuplet
    , setDivision
    , setDotted
    , setDoubleDotted
    , setPitches
    , setRest
    , setSeparate
    , setTied
    , setTripleDotted
    , sixteenth
    , sixtyFourth
    , thirtySecond
    , triplet
    , whole
    )

import MusicTheory.Pitch exposing (Pitch)


type NoteOrTuplet
    = Single DurationAndPitches
    | Duplet DurationAndPitches DurationAndPitches
    | Triplet DurationAndPitches DurationAndPitches DurationAndPitches
    | Quadruplet DurationAndPitches DurationAndPitches DurationAndPitches DurationAndPitches
    | Quintuplet DurationAndPitches DurationAndPitches DurationAndPitches DurationAndPitches DurationAndPitches


type alias DurationAndPitches =
    { duration : Duration
    , pitches : List Pitch
    }


type Division
    = Whole
    | Half
    | Quarter
    | Eighth
    | Sixteenth
    | ThirtySecond
    | SixtyFourth
    | OneHundredTwentyEighth


type Duration
    = Normal Division TiedOrSeparate
    | Dotted Division TiedOrSeparate
    | DoubleDotted Division TiedOrSeparate
    | TripleDotted Division TiedOrSeparate


type TiedOrSeparate
    = Tied
    | Separate


note : Division -> List Pitch -> NoteOrTuplet
note division pitchList =
    Single
        { duration = Normal division Separate
        , pitches = pitchList
        }


duplet : Division -> List Pitch -> List Pitch -> NoteOrTuplet
duplet division pitchList1 pitchList2 =
    Duplet
        { duration = Normal division Separate
        , pitches = pitchList1
        }
        { duration = Normal division Separate
        , pitches = pitchList2
        }


triplet : Division -> List Pitch -> List Pitch -> List Pitch -> NoteOrTuplet
triplet division pitchList1 pitchList2 pitchList3 =
    Triplet
        { duration = Normal division Separate
        , pitches = pitchList1
        }
        { duration = Normal division Separate
        , pitches = pitchList2
        }
        { duration = Normal division Separate
        , pitches = pitchList3
        }


quadruplet : Division -> List Pitch -> List Pitch -> List Pitch -> List Pitch -> NoteOrTuplet
quadruplet division pitchList1 pitchList2 pitchList3 pitchList4 =
    Quadruplet
        { duration = Normal division Separate
        , pitches = pitchList1
        }
        { duration = Normal division Separate
        , pitches = pitchList2
        }
        { duration = Normal division Separate
        , pitches = pitchList3
        }
        { duration = Normal division Separate
        , pitches = pitchList4
        }


quintuplet : Division -> List Pitch -> List Pitch -> List Pitch -> List Pitch -> List Pitch -> NoteOrTuplet
quintuplet division pitchList1 pitchList2 pitchList3 pitchList4 pitchList5 =
    Quintuplet
        { duration = Normal division Separate
        , pitches = pitchList1
        }
        { duration = Normal division Separate
        , pitches = pitchList2
        }
        { duration = Normal division Separate
        , pitches = pitchList3
        }
        { duration = Normal division Separate
        , pitches = pitchList4
        }
        { duration = Normal division Separate
        , pitches = pitchList5
        }


whole : Division
whole =
    Whole


half : Division
half =
    Half


quarter : Division
quarter =
    Quarter


eighth : Division
eighth =
    Eighth


sixteenth : Division
sixteenth =
    Sixteenth


thirtySecond : Division
thirtySecond =
    ThirtySecond


sixtyFourth : Division
sixtyFourth =
    SixtyFourth


oneHundredTwentyEighth : Division
oneHundredTwentyEighth =
    OneHundredTwentyEighth


mapAll : (DurationAndPitches -> DurationAndPitches) -> NoteOrTuplet -> NoteOrTuplet
mapAll fn noteOrTuplet =
    case noteOrTuplet of
        Single durationAndPitches ->
            Single (fn durationAndPitches)

        Duplet durationAndPitches durationAndPitches2 ->
            Duplet (fn durationAndPitches) (fn durationAndPitches2)

        Triplet durationAndPitches durationAndPitches2 durationAndPitches3 ->
            Triplet (fn durationAndPitches) (fn durationAndPitches2) (fn durationAndPitches3)

        Quadruplet durationAndPitches durationAndPitches2 durationAndPitches3 durationAndPitches4 ->
            Quadruplet (fn durationAndPitches) (fn durationAndPitches2) (fn durationAndPitches3) (fn durationAndPitches4)

        Quintuplet durationAndPitches durationAndPitches2 durationAndPitches3 durationAndPitches4 durationAndPitches5 ->
            Quintuplet (fn durationAndPitches) (fn durationAndPitches2) (fn durationAndPitches3) (fn durationAndPitches4) (fn durationAndPitches5)


mapFirst : (DurationAndPitches -> DurationAndPitches) -> NoteOrTuplet -> NoteOrTuplet
mapFirst fn noteOrTuplet =
    case noteOrTuplet of
        Single durationAndPitches ->
            Single (fn durationAndPitches)

        Duplet durationAndPitches durationAndPitches2 ->
            Duplet (fn durationAndPitches) durationAndPitches2

        Triplet durationAndPitches durationAndPitches2 durationAndPitches3 ->
            Triplet (fn durationAndPitches) durationAndPitches2 durationAndPitches3

        Quadruplet durationAndPitches durationAndPitches2 durationAndPitches3 durationAndPitches4 ->
            Quadruplet (fn durationAndPitches) durationAndPitches2 durationAndPitches3 durationAndPitches4

        Quintuplet durationAndPitches durationAndPitches2 durationAndPitches3 durationAndPitches4 durationAndPitches5 ->
            Quintuplet (fn durationAndPitches) durationAndPitches2 durationAndPitches3 durationAndPitches4 durationAndPitches5


mapSecond : (DurationAndPitches -> DurationAndPitches) -> NoteOrTuplet -> NoteOrTuplet
mapSecond fn noteOrTuplet =
    case noteOrTuplet of
        Single durationAndPitches ->
            Single durationAndPitches

        Duplet durationAndPitches durationAndPitches2 ->
            Duplet durationAndPitches (fn durationAndPitches2)

        Triplet durationAndPitches durationAndPitches2 durationAndPitches3 ->
            Triplet durationAndPitches (fn durationAndPitches2) durationAndPitches3

        Quadruplet durationAndPitches durationAndPitches2 durationAndPitches3 durationAndPitches4 ->
            Quadruplet durationAndPitches (fn durationAndPitches2) durationAndPitches3 durationAndPitches4

        Quintuplet durationAndPitches durationAndPitches2 durationAndPitches3 durationAndPitches4 durationAndPitches5 ->
            Quintuplet durationAndPitches (fn durationAndPitches2) durationAndPitches3 durationAndPitches4 durationAndPitches5


mapThird : (DurationAndPitches -> DurationAndPitches) -> NoteOrTuplet -> NoteOrTuplet
mapThird fn noteOrTuplet =
    case noteOrTuplet of
        Single durationAndPitches ->
            Single durationAndPitches

        Duplet durationAndPitches durationAndPitches2 ->
            Duplet durationAndPitches durationAndPitches2

        Triplet durationAndPitches durationAndPitches2 durationAndPitches3 ->
            Triplet durationAndPitches durationAndPitches2 (fn durationAndPitches3)

        Quadruplet durationAndPitches durationAndPitches2 durationAndPitches3 durationAndPitches4 ->
            Quadruplet durationAndPitches durationAndPitches2 (fn durationAndPitches3) durationAndPitches4

        Quintuplet durationAndPitches durationAndPitches2 durationAndPitches3 durationAndPitches4 durationAndPitches5 ->
            Quintuplet durationAndPitches durationAndPitches2 (fn durationAndPitches3) durationAndPitches4 durationAndPitches5


mapFourth : (DurationAndPitches -> DurationAndPitches) -> NoteOrTuplet -> NoteOrTuplet
mapFourth fn noteOrTuplet =
    case noteOrTuplet of
        Single durationAndPitches ->
            Single durationAndPitches

        Duplet durationAndPitches durationAndPitches2 ->
            Duplet durationAndPitches durationAndPitches2

        Triplet durationAndPitches durationAndPitches2 durationAndPitches3 ->
            Triplet durationAndPitches durationAndPitches2 durationAndPitches3

        Quadruplet durationAndPitches durationAndPitches2 durationAndPitches3 durationAndPitches4 ->
            Quadruplet durationAndPitches durationAndPitches2 durationAndPitches3 (fn durationAndPitches4)

        Quintuplet durationAndPitches durationAndPitches2 durationAndPitches3 durationAndPitches4 durationAndPitches5 ->
            Quintuplet durationAndPitches durationAndPitches2 durationAndPitches3 (fn durationAndPitches4) durationAndPitches5


mapFifth : (DurationAndPitches -> DurationAndPitches) -> NoteOrTuplet -> NoteOrTuplet
mapFifth fn noteOrTuplet =
    case noteOrTuplet of
        Single durationAndPitches ->
            Single durationAndPitches

        Duplet durationAndPitches durationAndPitches2 ->
            Duplet durationAndPitches durationAndPitches2

        Triplet durationAndPitches durationAndPitches2 durationAndPitches3 ->
            Triplet durationAndPitches durationAndPitches2 durationAndPitches3

        Quadruplet durationAndPitches durationAndPitches2 durationAndPitches3 durationAndPitches4 ->
            Quadruplet durationAndPitches durationAndPitches2 durationAndPitches3 durationAndPitches4

        Quintuplet durationAndPitches durationAndPitches2 durationAndPitches3 durationAndPitches4 durationAndPitches5 ->
            Quintuplet durationAndPitches durationAndPitches2 durationAndPitches3 durationAndPitches4 (fn durationAndPitches5)


setDivision : Division -> DurationAndPitches -> DurationAndPitches
setDivision newDivision { duration, pitches } =
    let
        updatedDuration =
            case duration of
                Normal _ tiedOrSeparate ->
                    DoubleDotted newDivision tiedOrSeparate

                Dotted _ tiedOrSeparate ->
                    DoubleDotted newDivision tiedOrSeparate

                DoubleDotted _ tiedOrSeparate ->
                    DoubleDotted newDivision tiedOrSeparate

                TripleDotted _ tiedOrSeparate ->
                    DoubleDotted newDivision tiedOrSeparate
    in
    { duration = updatedDuration
    , pitches = pitches
    }


setRest : DurationAndPitches -> DurationAndPitches
setRest { duration, pitches } =
    { duration = duration
    , pitches = []
    }


setPitches : List Pitch -> DurationAndPitches -> DurationAndPitches
setPitches newPitches { duration, pitches } =
    { duration = duration
    , pitches = newPitches
    }


setDotted : DurationAndPitches -> DurationAndPitches
setDotted { duration, pitches } =
    let
        updatedDuration =
            case duration of
                Normal division tiedOrSeparate ->
                    Dotted division tiedOrSeparate

                Dotted division tiedOrSeparate ->
                    Dotted division tiedOrSeparate

                DoubleDotted division tiedOrSeparate ->
                    Dotted division tiedOrSeparate

                TripleDotted division tiedOrSeparate ->
                    Dotted division tiedOrSeparate
    in
    { duration = updatedDuration
    , pitches = pitches
    }


setDoubleDotted : DurationAndPitches -> DurationAndPitches
setDoubleDotted { duration, pitches } =
    let
        updatedDuration =
            case duration of
                Normal division tiedOrSeparate ->
                    DoubleDotted division tiedOrSeparate

                Dotted division tiedOrSeparate ->
                    DoubleDotted division tiedOrSeparate

                DoubleDotted division tiedOrSeparate ->
                    DoubleDotted division tiedOrSeparate

                TripleDotted division tiedOrSeparate ->
                    DoubleDotted division tiedOrSeparate
    in
    { duration = updatedDuration
    , pitches = pitches
    }


setTripleDotted : DurationAndPitches -> DurationAndPitches
setTripleDotted { duration, pitches } =
    let
        updatedDuration =
            case duration of
                Normal division tiedOrSeparate ->
                    TripleDotted division tiedOrSeparate

                Dotted division tiedOrSeparate ->
                    TripleDotted division tiedOrSeparate

                DoubleDotted division tiedOrSeparate ->
                    TripleDotted division tiedOrSeparate

                TripleDotted division tiedOrSeparate ->
                    TripleDotted division tiedOrSeparate
    in
    { duration = updatedDuration
    , pitches = pitches
    }


setTied : DurationAndPitches -> DurationAndPitches
setTied { duration, pitches } =
    let
        updatedDuration =
            case duration of
                Normal division _ ->
                    Normal division Tied

                Dotted division _ ->
                    Dotted division Tied

                DoubleDotted division _ ->
                    DoubleDotted division Tied

                TripleDotted division _ ->
                    TripleDotted division Tied
    in
    { duration = updatedDuration
    , pitches = pitches
    }


setSeparate : DurationAndPitches -> DurationAndPitches
setSeparate { duration, pitches } =
    let
        updatedDuration =
            case duration of
                Normal division _ ->
                    Normal division Separate

                Dotted division _ ->
                    Dotted division Separate

                DoubleDotted division _ ->
                    DoubleDotted division Separate

                TripleDotted division _ ->
                    TripleDotted division Separate
    in
    { duration = updatedDuration
    , pitches = pitches
    }
