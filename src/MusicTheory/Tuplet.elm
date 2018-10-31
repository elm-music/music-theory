module MusicTheory.Tuplet exposing (Tuplet, Value(..), duplet, quadruplet, quintuplet, septuplet, sextuplet, triplet)


type Value
    = Half
    | Quarter
    | Eighth
    | Sixteenth
    | ThirtySecond
    | SixtyFourth
    | OneHundredTwentyEighth


type Tuplet
    = Duplet Value
    | Triplet Value
    | Quadruplet Value
    | Quintuplet Value
    | Sextuplet Value
    | Septuplet Value
    | Octuplet Value
    | Nonuplet Value
    | Decuplet Value
    | Undecuplet Value
    | Duodecuplet Value
    | Tredecuplet Value
    | Quattuordecuplet Value
    | Quindecuplet Value
    | Sexdecuplet Value
    | Septendecuplet Value
    | Octodecuplet Value
    | Novemdecuplet Value
    | Vigintuplet Value


duplet : Value -> Tuplet
duplet =
    Duplet


triplet : Value -> Tuplet
triplet =
    Triplet


quadruplet : Value -> Tuplet
quadruplet =
    Quadruplet


quintuplet : Value -> Tuplet
quintuplet =
    Quintuplet


sextuplet : Value -> Tuplet
sextuplet =
    Sextuplet


septuplet : Value -> Tuplet
septuplet =
    Septuplet
