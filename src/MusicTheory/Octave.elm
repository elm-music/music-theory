module MusicTheory.Octave exposing
    ( Octave
    , add
    , all
    , eight
    , five
    , four
    , number
    , octave
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


octave : Int -> Maybe Octave
octave n =
    case n of
        0 ->
            Just Zero

        1 ->
            Just One

        2 ->
            Just Two

        3 ->
            Just Three

        4 ->
            Just Four

        5 ->
            Just Five

        6 ->
            Just Six

        7 ->
            Just Seven

        8 ->
            Just Eight

        _ ->
            Nothing


add : Int -> Octave -> Maybe Octave
add n o =
    octave (number o + n)


semitones : Octave -> Int
semitones o =
    number o * 12


number : Octave -> Int
number o =
    case o of
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
