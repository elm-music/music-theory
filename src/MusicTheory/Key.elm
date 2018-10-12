module MusicTheory.Key exposing
    ( Key
    , MajorOrMinor(..)
    , major
    , minor
    , scale
    )

import MusicTheory.PitchClass exposing (Accidental(..), Letter(..), PitchClass, pitchClass)
import MusicTheory.Scale as Scale exposing (Scale)


type Key
    = Key MajorOrMinor Scale


type MajorOrMinor
    = Major
    | Minor


major : PitchClass -> Key
major root =
    Key Major <| Scale.scale root Scale.major


minor : PitchClass -> Key
minor root =
    Key Minor <| Scale.scale root Scale.minor


scale : Key -> Scale
scale (Key _ keyScale) =
    keyScale


tonic : Key -> PitchClass
tonic key =
    Scale.root <| scale key
