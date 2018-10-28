module MusicTheory.Music exposing
    ( Control(..)
    , Music(..)
    , Primitive(..)
    , aFlat
    , aNatural
    , aSharp
    , addDuration
    , bFlat
    , bNatural
    , bSharp
    , cFlat
    , cNatural
    , cSharp
    , chord
    , dFlat
    , dNatural
    , dSharp
    , duplet
    , eFlat
    , eNatural
    , eSharp
    , fFlat
    , fNatural
    , fSharp
    , gFlat
    , gNatural
    , gSharp
    , line
    , map
    , modify
    , note
    , pMap
    , pToList
    , par
    , quadruplet
    , quintuplet
    , rest
    , seq
    , times
    , toList
    , triplet
    )

import MusicTheory.Duration as Duration exposing (Duration)
import MusicTheory.Letter exposing (Letter(..))
import MusicTheory.Octave exposing (Octave)
import MusicTheory.Pitch as Pitch exposing (Pitch)



-- TYPES


type Primitive a
    = Note Duration a
    | Rest Duration


type Control
    = Duplet
    | Triplet
    | Quadruplet
    | Quintuplet


type Music a
    = Prim (Primitive a)
    | Seq (Music a) (Music a)
    | Par (Music a) (Music a)
    | Modify Control (Music a)



-- CONSTRUCTORS


note : Duration -> a -> Music a
note duration a =
    Prim <| Note duration a


rest : Duration -> Music a
rest duration =
    Prim <| Rest duration


seq : Music a -> Music a -> Music a
seq =
    Seq


par : Music a -> Music a -> Music a
par =
    Par


modify : Control -> Music a -> Music a
modify =
    Modify


duplet : Music a -> Music a -> Music a
duplet m1 m2 =
    modify Duplet <| seq m1 m2


triplet : Music a -> Music a -> Music a -> Music a
triplet m1 m2 m3 =
    modify Triplet <| line [ m1, m2, m3 ]


quadruplet : Music a -> Music a -> Music a -> Music a -> Music a
quadruplet m1 m2 m3 m4 =
    modify Quadruplet <| line [ m1, m2, m3, m4 ]


quintuplet : Music a -> Music a -> Music a -> Music a -> Music a -> Music a
quintuplet m1 m2 m3 m4 m5 =
    modify Quintuplet <| line [ m1, m2, m3, m4, m5 ]



-- NOTE CONSTRUCTORS


cFlat : Octave -> Duration -> Music Pitch
cFlat o duration =
    note duration (Pitch.pitch C Pitch.flat o)


cNatural : Octave -> Duration -> Music Pitch
cNatural o duration =
    note duration (Pitch.pitch C Pitch.natural o)


cSharp : Octave -> Duration -> Music Pitch
cSharp o duration =
    note duration (Pitch.pitch C Pitch.sharp o)


dFlat : Octave -> Duration -> Music Pitch
dFlat o duration =
    note duration (Pitch.pitch D Pitch.flat o)


dNatural : Octave -> Duration -> Music Pitch
dNatural o duration =
    note duration (Pitch.pitch D Pitch.natural o)


dSharp : Octave -> Duration -> Music Pitch
dSharp o duration =
    note duration (Pitch.pitch D Pitch.sharp o)


eFlat : Octave -> Duration -> Music Pitch
eFlat o duration =
    note duration (Pitch.pitch E Pitch.flat o)


eNatural : Octave -> Duration -> Music Pitch
eNatural o duration =
    note duration (Pitch.pitch E Pitch.natural o)


eSharp : Octave -> Duration -> Music Pitch
eSharp o duration =
    note duration (Pitch.pitch E Pitch.sharp o)


fFlat : Octave -> Duration -> Music Pitch
fFlat o duration =
    note duration (Pitch.pitch F Pitch.flat o)


fNatural : Octave -> Duration -> Music Pitch
fNatural o duration =
    note duration (Pitch.pitch F Pitch.natural o)


fSharp : Octave -> Duration -> Music Pitch
fSharp o duration =
    note duration (Pitch.pitch F Pitch.sharp o)


gFlat : Octave -> Duration -> Music Pitch
gFlat o duration =
    note duration (Pitch.pitch G Pitch.flat o)


gNatural : Octave -> Duration -> Music Pitch
gNatural o duration =
    note duration (Pitch.pitch G Pitch.natural o)


gSharp : Octave -> Duration -> Music Pitch
gSharp o duration =
    note duration (Pitch.pitch G Pitch.sharp o)


aFlat : Octave -> Duration -> Music Pitch
aFlat o duration =
    note duration (Pitch.pitch A Pitch.flat o)


aNatural : Octave -> Duration -> Music Pitch
aNatural o duration =
    note duration (Pitch.pitch A Pitch.natural o)


aSharp : Octave -> Duration -> Music Pitch
aSharp o duration =
    note duration (Pitch.pitch A Pitch.sharp o)


bFlat : Octave -> Duration -> Music Pitch
bFlat o duration =
    note duration (Pitch.pitch B Pitch.flat o)


bNatural : Octave -> Duration -> Music Pitch
bNatural o duration =
    note duration (Pitch.pitch B Pitch.natural o)


bSharp : Octave -> Duration -> Music Pitch
bSharp o duration =
    note duration (Pitch.pitch B Pitch.sharp o)



-- FUNCTIONS


line : List (Music a) -> Music a
line ms =
    case ms of
        [] ->
            rest Duration.zero

        [ m ] ->
            m

        h :: t ->
            List.foldl (\m1 m2 -> seq m2 m1) h t


chord : List (Music a) -> Music a
chord ms =
    case ms of
        [] ->
            rest Duration.zero

        [ m ] ->
            m

        h :: t ->
            List.foldl (\m1 m2 -> par m2 m1) h t


pToList : Primitive a -> List a
pToList primitive =
    case primitive of
        Note _ a ->
            [ a ]

        Rest _ ->
            []


pMap : (a -> b) -> Primitive a -> Primitive b
pMap f primitive =
    case primitive of
        Note duration a ->
            Note duration (f a)

        Rest duration ->
            Rest duration


map : (a -> b) -> Music a -> Music b
map f music =
    case music of
        Prim group ->
            Prim (pMap f group)

        Seq m1 m2 ->
            Seq (map f m1) (map f m2)

        Par m1 m2 ->
            Par (map f m1) (map f m2)

        Modify control m ->
            Modify control (map f m)


toList : Music a -> List a
toList music =
    case music of
        Prim prim ->
            pToList prim

        Seq m1 m2 ->
            toList m1 ++ toList m2

        Par m1 m2 ->
            toList m1 ++ toList m2

        Modify _ m ->
            toList m


times : Int -> Music a -> Music a
times n m =
    if n <= 0 then
        rest Duration.zero

    else
        m |> seq (times (n - 1) m)


addDuration : Duration -> List (Duration -> Music a) -> Music a
addDuration duration =
    List.map (\n -> n duration) >> line
