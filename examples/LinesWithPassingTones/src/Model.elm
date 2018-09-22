module Model exposing (Model, Msg(..), init, update)

import MusicTheory.Key
import MusicTheory.Pitch exposing (Octave, Pitch)
import MusicTheory.PitchClass exposing (Accidental(..), Letter(..))
import MusicTheory.Scale
    exposing
        ( degree
        , downTo
        , loweredDegree
        , raisedDegree
        , upTo
        )


type alias Model =
    { line : List Pitch }


type Msg
    = NoOp



{- Ideally, the signature of Scale.generateLine is:

   generateLine : Pattern -> Degree -> Octave -> Maybe Key -> List Pitch

   and the call to it below in myLineWithPassingTones returns:

       [ Pitch (PitchClass G Natural) 4
       , Pitch (PitchClass G Sharp) 4
       , Pitch (PitchClass A Natural) 4
       , Pitch (PitchClass C Natural) 5
       , Pitch (PitchClass A Natural) 4
       , Pitch (PitchClass A Flat) 4
       , Pitch (PitchClass G Natural) 4
       , Pitch (PitchClass E Natural) 4
       ]

   Where this list of pitches is correctly spelled in terms of the key of
   C major, uses sharps for raised degrees and flats for lowered degrees,
   begins on the correct scale degree and octave, and moves in the directions
   specified by the Pattern.
-}


myLineWithPassingTones : List Pitch
myLineWithPassingTones =
    let
        startingOctave =
            octave 4

        startingDegree =
            degree 5

        scale =
            Scale.major <| pitch C Natural

        key =
            Key.major <| C Natural
    in
    Scale.generateLine myPattern startingDegree startingOctave (Just key)


myPattern : Scale.Pattern
myPattern =
    [ upTo (raisedDegree 5)
    , upTo (degree 6)
    , upTo (degree 1)
    , downTo (degree 6)
    , downTo (loweredDegree 6)
    , downTo (degree 5)
    , downTo (degree 3)
    ]


init : Model
init =
    { line = myLineWithPassingTones
    }


update : Msg -> Model -> Model
update msg model =
    model
