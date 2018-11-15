module MusicTheory.TimeSignature exposing
    ( BeatValue(..)
    , NumberOfBeats(..)
    , TimeSignature
    , additive
    , beatValue
    , beatValueInt
    , numberOfBeats
    , numberOfBeatsInt
    , numberOfBeatsToInt
    , timeSignature
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
