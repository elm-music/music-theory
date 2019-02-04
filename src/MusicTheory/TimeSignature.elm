module MusicTheory.TimeSignature exposing
    ( BeatValue(..)
    , NumberOfBeats(..)
    , TimeSignature
    , additive
    , beatValue
    , beatValueInt
    , eight
    , eighteen
    , eighths
    , eleven
    , fifteen
    , five
    , four
    , fourteen
    , halfs
    , nine
    , nineteen
    , numberOfBeats
    , numberOfBeatsInt
    , numberOfBeatsToInt
    , one
    , quarters
    , seven
    , seventeen
    , six
    , sixteen
    , sixteenths
    , ten
    , thirteen
    , thirtySeconds
    , three
    , timeSignature
    , twelve
    , twenty
    , two
    , whole
    )


type BeatValue
    = Whole
    | Half
    | Quarter
    | Eighth
    | Sixteenth
    | ThirtySecond


type NumberOfBeats
    = One
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ten
    | Eleven
    | Twelve
    | Thirteen
    | Fourteen
    | Fifteen
    | Sixteen
    | Seventeen
    | Eighteen
    | Nineteen
    | Twenty


type TimeSignature
    = Normal NumberOfBeats BeatValue
    | Additive NumberOfBeats (List NumberOfBeats) BeatValue


timeSignature : NumberOfBeats -> BeatValue -> TimeSignature
timeSignature =
    Normal


additive : NumberOfBeats -> List NumberOfBeats -> BeatValue -> TimeSignature
additive =
    Additive


beatValue : TimeSignature -> BeatValue
beatValue ts =
    case ts of
        Normal _ bv ->
            bv

        Additive _ _ bv ->
            bv


beatValueToInt : BeatValue -> Int
beatValueToInt bv =
    case bv of
        Whole ->
            1

        Half ->
            2

        Quarter ->
            4

        Eighth ->
            8

        Sixteenth ->
            16

        ThirtySecond ->
            32


beatValueInt : TimeSignature -> Int
beatValueInt ts =
    case ts of
        Normal _ bv ->
            beatValueToInt bv

        Additive _ _ bv ->
            beatValueToInt bv


numberOfBeats : TimeSignature -> ( NumberOfBeats, List NumberOfBeats )
numberOfBeats ts =
    case ts of
        Normal nob _ ->
            ( nob, [] )

        Additive nob nobs _ ->
            ( nob, nobs )


numberOfBeatsToInt : NumberOfBeats -> Int
numberOfBeatsToInt nob =
    case nob of
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

        Nine ->
            9

        Ten ->
            10

        Eleven ->
            11

        Twelve ->
            12

        Thirteen ->
            13

        Fourteen ->
            14

        Fifteen ->
            15

        Sixteen ->
            16

        Seventeen ->
            17

        Eighteen ->
            18

        Nineteen ->
            19

        Twenty ->
            20


numberOfBeatsInt : TimeSignature -> Int
numberOfBeatsInt ts =
    ts
        |> numberOfBeats
        |> Tuple.mapBoth numberOfBeatsToInt (List.map numberOfBeatsToInt >> List.sum)
        |> (\( x, y ) -> x + y)


whole : BeatValue
whole =
    Whole


halfs : BeatValue
halfs =
    Half


quarters : BeatValue
quarters =
    Quarter


eighths : BeatValue
eighths =
    Eighth


sixteenths : BeatValue
sixteenths =
    Sixteenth


thirtySeconds : BeatValue
thirtySeconds =
    ThirtySecond



----


one : BeatValue -> TimeSignature
one val =
    timeSignature One val


two : BeatValue -> TimeSignature
two val =
    timeSignature Two val


three : BeatValue -> TimeSignature
three val =
    timeSignature Three val


four : BeatValue -> TimeSignature
four val =
    timeSignature Four val


five : BeatValue -> TimeSignature
five val =
    timeSignature Five val


six : BeatValue -> TimeSignature
six val =
    timeSignature Six val


seven : BeatValue -> TimeSignature
seven val =
    timeSignature Seven val


eight : BeatValue -> TimeSignature
eight val =
    timeSignature Eight val


nine : BeatValue -> TimeSignature
nine val =
    timeSignature Nine val


ten : BeatValue -> TimeSignature
ten val =
    timeSignature Ten val


eleven : BeatValue -> TimeSignature
eleven val =
    timeSignature Eleven val


twelve : BeatValue -> TimeSignature
twelve val =
    timeSignature Twelve val


thirteen : BeatValue -> TimeSignature
thirteen val =
    timeSignature Thirteen val


fourteen : BeatValue -> TimeSignature
fourteen val =
    timeSignature Fourteen val


fifteen : BeatValue -> TimeSignature
fifteen val =
    timeSignature Fifteen val


sixteen : BeatValue -> TimeSignature
sixteen val =
    timeSignature Sixteen val


seventeen : BeatValue -> TimeSignature
seventeen val =
    timeSignature Seventeen val


eighteen : BeatValue -> TimeSignature
eighteen val =
    timeSignature Eighteen val


nineteen : BeatValue -> TimeSignature
nineteen val =
    timeSignature Nineteen val


twenty : BeatValue -> TimeSignature
twenty val =
    timeSignature Twenty val
