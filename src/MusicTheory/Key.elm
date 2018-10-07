module Key exposing
    ( Key
    , Tonality(..)
    , major
    , minor
    , scaleDegrees
    )

import MusicTheory.PitchClass as PitchClass exposing (PitchClass)
import MusicTheory.Scale as Scale
    exposing
        ( DiatonicScale
        , HeptatonicScaleDegrees
        , diatonicScale
        )


type Key
    = Key Tonality DiatonicScale


type Tonality
    = Major
    | Minor


major : PitchClass -> Key
major root =
    Key Major (diatonicScale root Scale.major)


minor : PitchClass -> Key
minor root =
    Key Minor (diatonicScale root Scale.minor)


scaleDegrees : Key -> HeptatonicScaleDegrees
scaleDegrees (Key _ diatonicScale) =
    Scale.diatonicScaleDegrees diatonicScale


tonic : Key -> PitchClass
tonic key =
    scaleDegrees key
        |> .root
