module MusicTheory.Octave exposing
    ( Octave
    , all
    , eight
    , five
    , four
    , number
    , one
    , semitones
    , seven
    , six
    , three
    , two
    , zero
    )


type Octave
    = Zero
    | One
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight


semitones : Octave -> Int
semitones octave =
    number octave * 12


number : Octave -> Int
number octave =
    case octave of
        Zero ->
            0

        One ->
            1

        Two ->
            2

        Three ->
            3

        Four ->
            4

        Five ->
            5

        Six ->
            6

        Seven ->
            7

        Eight ->
            8


all : List Octave
all =
    [ Zero, One, Two, Three, Four, Five, Six, Seven, Eight ]


zero : Octave
zero =
    Zero


one : Octave
one =
    One


two : Octave
two =
    Two


three : Octave
three =
    Three


four : Octave
four =
    Four


five : Octave
five =
    Five


six : Octave
six =
    Six


seven : Octave
seven =
    Seven


eight : Octave
eight =
    Eight
