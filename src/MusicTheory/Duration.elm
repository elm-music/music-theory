module MusicTheory.Duration exposing
    ( Division
    , Duration
    , TiedOrUntied
    , dotted
    , doubleDotted
    , eighthNote
    , halfNote
    , oneHundredTwentyEighthNote
    , quarterNote
    , sixteenthNote
    , sixtyFourthNote
    , thirtySecondNote
    , tied
    , tripleDotted
    , untied
    , wholeNote
    , zero
    )


type Division
    = Whole
    | Half
    | Quarter
    | Eighth
    | Sixteenth
    | ThirtySecond
    | SixtyFourth
    | OneHundredTwentyEighth
    | Zero


type Duration
    = Normal Division TiedOrUntied
    | Dotted Division TiedOrUntied
    | DoubleDotted Division TiedOrUntied
    | TripleDotted Division TiedOrUntied


type TiedOrUntied
    = Tied
    | Untied


wholeNote : Duration
wholeNote =
    Normal Whole Untied


halfNote : Duration
halfNote =
    Normal Half Untied


quarterNote : Duration
quarterNote =
    Normal Quarter Untied


eighthNote : Duration
eighthNote =
    Normal Eighth Untied


sixteenthNote : Duration
sixteenthNote =
    Normal Sixteenth Untied


thirtySecondNote : Duration
thirtySecondNote =
    Normal ThirtySecond Untied


sixtyFourthNote : Duration
sixtyFourthNote =
    Normal SixtyFourth Untied


oneHundredTwentyEighthNote : Duration
oneHundredTwentyEighthNote =
    Normal OneHundredTwentyEighth Untied


zero : Duration
zero =
    Normal Zero Untied


tied : Duration -> Duration
tied duration =
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
