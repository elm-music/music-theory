module MusicTheory.Duration exposing
    ( Division(..)
    , Duration(..)
    , Grouping(..)
    , TiedOrSeparate(..)
    )


type Grouping
    = Single Duration
    | Duplet Duration Duration
    | Triplet Duration Duration Duration
    | Quadruplet Duration Duration Duration Duration
    | Quintuplet Duration Duration Duration Duration Duration


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
