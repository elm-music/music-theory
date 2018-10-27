module MusicTheory.Music exposing
    ( Control(..)
    , Division(..)
    , Duration(..)
    , Music(..)
    , Primitive(..)
    , PrimitiveGroup(..)
    , TiedOrUntied(..)
    , dotted
    , doubleDotted
    , eighth
    , half
    , oneHundredTwentyEighth
    , quarter
    , sixteenth
    , sixtyFourth
    , thirtySecond
    , toList
    , tripleDotted
    , untied
    , whole
    , withTie
    )


type PrimitiveGroup a
    = Single (Primitive a)
    | Duplet (Primitive a) (Primitive a)
    | Triplet (Primitive a) (Primitive a) (Primitive a)
    | Quadruplet (Primitive a) (Primitive a) (Primitive a) (Primitive a)
    | Quintuplet (Primitive a) (Primitive a) (Primitive a) (Primitive a) (Primitive a)


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
    = Normal Division TiedOrUntied
    | Dotted Division TiedOrUntied
    | DoubleDotted Division TiedOrUntied
    | TripleDotted Division TiedOrUntied


type TiedOrUntied
    = Tied
    | Untied


type Primitive a
    = Note Duration a
    | Rest Duration


type Control
    = Control


type Music a
    = Primitives (PrimitiveGroup a)
    | Seq (Music a) (Music a)
    | Par (Music a) (Music a)
    | Modify Control (Music a)


toList : Music a -> List a
toList music =
    case music of
        Primitives (Single (Note _ a)) ->
            [ a ]

        Primitives (Duplet (Note _ a1) (Note _ a2)) ->
            [ a1, a2 ]

        Primitives (Triplet (Note _ a1) (Note _ a2) (Note _ a3)) ->
            [ a1, a2, a3 ]

        Primitives (Quadruplet (Note _ a1) (Note _ a2) (Note _ a3) (Note _ a4)) ->
            [ a1, a2, a3, a4 ]

        Primitives (Quintuplet (Note _ a1) (Note _ a2) (Note _ a3) (Note _ a4) (Note _ a5)) ->
            [ a1, a2, a3, a4, a5 ]

        Primitives _ ->
            []

        Seq m1 m2 ->
            toList m1 ++ toList m2

        Par m1 m2 ->
            toList m1 ++ toList m2

        Modify _ m ->
            toList m


whole : Duration
whole =
    Normal Whole Untied


half : Duration
half =
    Normal Half Untied


quarter : Duration
quarter =
    Normal Quarter Untied


eighth : Duration
eighth =
    Normal Eighth Untied


sixteenth : Duration
sixteenth =
    Normal Sixteenth Untied


thirtySecond : Duration
thirtySecond =
    Normal ThirtySecond Untied


sixtyFourth : Duration
sixtyFourth =
    Normal SixtyFourth Untied


oneHundredTwentyEighth : Duration
oneHundredTwentyEighth =
    Normal OneHundredTwentyEighth Untied


withTie : Duration -> Duration
withTie duration =
    case duration of
        Normal div _ ->
            Normal div Tied

        Dotted div _ ->
            Dotted div Tied

        DoubleDotted div _ ->
            DoubleDotted div Tied

        TripleDotted div _ ->
            TripleDotted div Tied


untied : Duration -> Duration
untied duration =
    case duration of
        Normal div _ ->
            Normal div Untied

        Dotted div _ ->
            Dotted div Untied

        DoubleDotted div _ ->
            DoubleDotted div Untied

        TripleDotted div _ ->
            TripleDotted div Untied


dotted : Duration -> Duration
dotted duration =
    case duration of
        Normal d t ->
            Dotted d t

        Dotted d t ->
            Dotted d t

        DoubleDotted d t ->
            Dotted d t

        TripleDotted d t ->
            Dotted d t


doubleDotted : Duration -> Duration
doubleDotted duration =
    case duration of
        Normal d t ->
            DoubleDotted d t

        Dotted d t ->
            DoubleDotted d t

        DoubleDotted d t ->
            DoubleDotted d t

        TripleDotted d t ->
            DoubleDotted d t


tripleDotted : Duration -> Duration
tripleDotted duration =
    case duration of
        Normal d t ->
            TripleDotted d t

        Dotted d t ->
            TripleDotted d t

        DoubleDotted d t ->
            TripleDotted d t

        TripleDotted d t ->
            TripleDotted d t
