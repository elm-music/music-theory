module MusicTheory.Key exposing
    ( Key
    , Tonality(..)
    , major
    , minor
    , scaleDegrees
    )

import MusicTheory.PitchClass as PitchClass exposing (PitchClass)
import MusicTheory.Scale as Scale


type Key
    = Key Tonality DiatonicScaleDegrees


type Tonality
    = Major
    | Minor


type alias DiatonicScaleDegrees =
    Scale.HeptatonicScaleDegrees


major : PitchClass -> Key
major root =
    Key Major
        (Scale.diatonicScale root Scale.major
            |> Scale.diatonicScaleDegrees
        )


minor : PitchClass -> Key
minor root =
    Key Minor
        (Scale.diatonicScale root Scale.minor
            |> Scale.diatonicScaleDegrees
        )


scaleDegrees : Key -> DiatonicScaleDegrees
scaleDegrees (Key _ diatonicScaleDegrees) =
    diatonicScaleDegrees


tonic : Key -> PitchClass
tonic key =
    scaleDegrees key
        |> .root
