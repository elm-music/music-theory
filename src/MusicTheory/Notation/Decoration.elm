module MusicTheory.Notation.Decoration exposing (Articulation(..), ChordSymbol(..), Dynamic(..), DynamicChange(..), Loudness(..), Lyrics(..), NoteHead(..), Ornament(..))


type DynamicChange
    = Crescendo
    | Decrescendo
    | Diminuendo


type Loudness
    = Pppp
    | Ppp
    | Pp
    | P
    | Mp
    | Mf
    | F
    | Ff
    | Fff
    | Ffff
    | Sfz


type Dynamic
    = Loudness Loudness
    | Change DynamicChange


type Articulation
    = Staccato
    | Accent
    | Tenuto
    | Marcato


type Ornament
    = Trill
    | Mordent


type NoteHead
    = DiamondHead
    | SquareHead
    | XHead
    | TriangleHead
    | TremoloHead
    | SlashHead
    | ArtHarmonic
    | NoHead


type Lyrics
    = Lyrics String


type ChordSymbol
    = ChordSymbol String
