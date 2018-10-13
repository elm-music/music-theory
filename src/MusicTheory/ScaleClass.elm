module MusicTheory.ScaleClass exposing
    ( ScaleClass
    , aeolian
    , altered
    , alteredBb7
    , arabian
    , augmented
    , augmentedIonian
    , augmentedLydian
    , balinese
    , blues
    , byzantine
    , chinese
    , diminishedHalftoneWholetone
    , diminishedLydian
    , diminishedWholetoneHalftone
    , dorian
    , dorianB2
    , dorianSharp4
    , doubleHarmonic
    , egyptian
    , eightToneSpanish
    , enigmatic
    , harmonicMinor
    , hindu
    , hirajoshi
    , hungarianMajor
    , hungarianMinorGipsy
    , ichikosucho
    , ionian
    , kumoi
    , leadingWholeTone
    , locrian
    , locrian2
    , locrian6
    , lydian
    , lydianB7
    , lydianSharp9
    , major
    , majorPentatonic
    , majorPhrygian
    , melodicMinor
    , minor
    , minorLydian
    , minorPentatonic
    , mixolydian
    , mixolydianB6
    , mohammedan
    , mongolian
    , naturalMinor
    , neopolitan
    , neopolitanMajor
    , neopolitanMinor
    , overtone
    , pelog
    , persian
    , phrygian
    , prometheus
    , prometheusNeopolitan
    , purviTheta
    , sixToneSymmetrical
    , todiTheta
    , wholeTone
    )

import MusicTheory.Internals.ScaleClass as Internal
import MusicTheory.Interval as Interval exposing (Interval)
import MusicTheory.PitchClass as PitchClass exposing (PitchClass)


type alias ScaleClass =
    Internal.ScaleClass


major : ScaleClass
major =
    ionian


minor : ScaleClass
minor =
    aeolian


melodicMinor : ScaleClass
melodicMinor =
    Internal.Heptatonic
        { first = Interval.majorSecond
        , second = Interval.minorSecond
        , third = Interval.majorSecond
        , fourth = Interval.majorSecond
        , fifth = Interval.majorSecond
        , sixth = Interval.majorSecond
        }


ionian : ScaleClass
ionian =
    Internal.Heptatonic
        { first = Interval.majorSecond
        , second = Interval.majorSecond
        , third = Interval.minorSecond
        , fourth = Interval.majorSecond
        , fifth = Interval.majorSecond
        , sixth = Interval.majorSecond
        }


dorian : ScaleClass
dorian =
    Internal.Heptatonic
        { first = Interval.majorSecond
        , second = Interval.minorSecond
        , third = Interval.majorSecond
        , fourth = Interval.majorSecond
        , fifth = Interval.majorSecond
        , sixth = Interval.minorSecond
        }


naturalMinor : ScaleClass
naturalMinor =
    Internal.Heptatonic
        { first = Interval.majorSecond
        , second = Interval.minorSecond
        , third = Interval.majorSecond
        , fourth = Interval.majorSecond
        , fifth = Interval.minorSecond
        , sixth = Interval.majorSecond
        }


harmonicMinor : ScaleClass
harmonicMinor =
    Internal.Heptatonic
        { first = Interval.majorSecond
        , second = Interval.minorSecond
        , third = Interval.majorSecond
        , fourth = Interval.majorSecond
        , fifth = Interval.minorSecond
        , sixth = Interval.minorThird
        }


lydian : ScaleClass
lydian =
    Internal.Heptatonic
        { first = Interval.majorSecond
        , second = Interval.majorSecond
        , third = Interval.majorSecond
        , fourth = Interval.minorSecond
        , fifth = Interval.majorSecond
        , sixth = Interval.majorSecond
        }


aeolian : ScaleClass
aeolian =
    Internal.Heptatonic
        { first = Interval.majorSecond
        , second = Interval.minorSecond
        , third = Interval.majorSecond
        , fourth = Interval.majorSecond
        , fifth = Interval.minorSecond
        , sixth = Interval.majorSecond
        }


phrygian : ScaleClass
phrygian =
    Internal.Heptatonic
        { first = Interval.minorSecond
        , second = Interval.majorSecond
        , third = Interval.majorSecond
        , fourth = Interval.majorSecond
        , fifth = Interval.minorSecond
        , sixth = Interval.majorSecond
        }


locrian : ScaleClass
locrian =
    Internal.Heptatonic
        { first = Interval.minorSecond
        , second = Interval.majorSecond
        , third = Interval.majorSecond
        , fourth = Interval.minorSecond
        , fifth = Interval.majorSecond
        , sixth = Interval.majorSecond
        }


blues : ScaleClass
blues =
    Internal.Hexatonic
        { first = Interval.minorThird
        , second = Interval.majorSecond
        , third = Interval.minorSecond
        , fourth = Interval.minorSecond
        , fifth = Interval.minorThird
        }


mixolydian : ScaleClass
mixolydian =
    Internal.Heptatonic
        { first = Interval.majorSecond
        , second = Interval.majorSecond
        , third = Interval.minorSecond
        , fourth = Interval.majorSecond
        , fifth = Interval.majorSecond
        , sixth = Interval.minorSecond
        }


diminishedHalftoneWholetone : ScaleClass
diminishedHalftoneWholetone =
    Internal.Octatonic
        { first = Interval.minorSecond
        , second = Interval.majorSecond
        , third = Interval.minorSecond
        , fourth = Interval.majorSecond
        , fifth = Interval.minorSecond
        , sixth = Interval.majorSecond
        , seventh = Interval.minorSecond
        }


minorPentatonic : ScaleClass
minorPentatonic =
    Internal.Pentatonic
        { first = Interval.minorThird
        , second = Interval.majorSecond
        , third = Interval.majorSecond
        , fourth = Interval.minorThird
        }


augmented : ScaleClass
augmented =
    Internal.Hexatonic
        { first = Interval.minorThird
        , second = Interval.minorSecond
        , third = Interval.minorThird
        , fourth = Interval.minorSecond
        , fifth = Interval.minorThird
        }


diminishedWholetoneHalftone : ScaleClass
diminishedWholetoneHalftone =
    Internal.Octatonic
        { first = Interval.majorSecond
        , second = Interval.minorSecond
        , third = Interval.majorSecond
        , fourth = Interval.minorSecond
        , fifth = Interval.majorSecond
        , sixth = Interval.minorSecond
        , seventh = Interval.majorSecond
        }


majorPentatonic : ScaleClass
majorPentatonic =
    Internal.Pentatonic
        { first = Interval.majorSecond
        , second = Interval.majorSecond
        , third = Interval.minorThird
        , fourth = Interval.majorSecond
        }


wholeTone : ScaleClass
wholeTone =
    Internal.Hexatonic
        { first = Interval.majorSecond
        , second = Interval.majorSecond
        , third = Interval.majorSecond
        , fourth = Interval.majorSecond
        , fifth = Interval.majorSecond
        }


leadingWholeTone : ScaleClass
leadingWholeTone =
    Internal.Heptatonic
        { first = Interval.majorSecond
        , second = Interval.majorSecond
        , third = Interval.majorSecond
        , fourth = Interval.majorSecond
        , fifth = Interval.majorSecond
        , sixth = Interval.minorSecond
        }


doubleHarmonic : ScaleClass
doubleHarmonic =
    Internal.Heptatonic
        { first = Interval.minorSecond
        , second = Interval.minorThird
        , third = Interval.minorSecond
        , fourth = Interval.majorSecond
        , fifth = Interval.minorSecond
        , sixth = Interval.minorThird
        }


overtone : ScaleClass
overtone =
    Internal.Heptatonic
        { first = Interval.majorSecond
        , second = Interval.majorSecond
        , third = Interval.majorSecond
        , fourth = Interval.minorSecond
        , fifth = Interval.majorSecond
        , sixth = Interval.minorSecond
        }


sixToneSymmetrical : ScaleClass
sixToneSymmetrical =
    Internal.Hexatonic
        { first = Interval.minorSecond
        , second = Interval.minorThird
        , third = Interval.minorSecond
        , fourth = Interval.minorThird
        , fifth = Interval.minorSecond
        }


altered : ScaleClass
altered =
    Internal.Heptatonic
        { first = Interval.minorSecond
        , second = Interval.majorSecond
        , third = Interval.minorSecond
        , fourth = Interval.majorSecond
        , fifth = Interval.majorSecond
        , sixth = Interval.majorSecond
        }


alteredBb7 : ScaleClass
alteredBb7 =
    Internal.Heptatonic
        { first = Interval.minorSecond
        , second = Interval.majorSecond
        , third = Interval.minorSecond
        , fourth = Interval.majorSecond
        , fifth = Interval.majorSecond
        , sixth = Interval.minorSecond
        }


dorianB2 : ScaleClass
dorianB2 =
    Internal.Heptatonic
        { first = Interval.minorSecond
        , second = Interval.majorSecond
        , third = Interval.majorSecond
        , fourth = Interval.minorSecond
        , fifth = Interval.majorSecond
        , sixth = Interval.minorSecond
        }


augmentedLydian : ScaleClass
augmentedLydian =
    Internal.Heptatonic
        { first = Interval.majorSecond
        , second = Interval.majorSecond
        , third = Interval.majorSecond
        , fourth = Interval.majorSecond
        , fifth = Interval.minorSecond
        , sixth = Interval.majorSecond
        }


enigmatic : ScaleClass
enigmatic =
    Internal.Heptatonic
        { first = Interval.minorSecond
        , second = Interval.minorThird
        , third = Interval.majorSecond
        , fourth = Interval.majorSecond
        , fifth = Interval.majorSecond
        , sixth = Interval.minorSecond
        }


lydianB7 : ScaleClass
lydianB7 =
    Internal.Heptatonic
        { first = Interval.majorSecond
        , second = Interval.majorSecond
        , third = Interval.majorSecond
        , fourth = Interval.minorSecond
        , fifth = Interval.majorSecond
        , sixth = Interval.minorSecond
        }


mixolydianB6 : ScaleClass
mixolydianB6 =
    Internal.Heptatonic
        { first = Interval.majorSecond
        , second = Interval.majorSecond
        , third = Interval.minorSecond
        , fourth = Interval.majorSecond
        , fifth = Interval.minorSecond
        , sixth = Interval.majorSecond
        }


locrian2 : ScaleClass
locrian2 =
    Internal.Heptatonic
        { first = Interval.majorSecond
        , second = Interval.minorSecond
        , third = Interval.majorSecond
        , fourth = Interval.minorSecond
        , fifth = Interval.majorSecond
        , sixth = Interval.majorSecond
        }


dorianSharp4 : ScaleClass
dorianSharp4 =
    Internal.Heptatonic
        { first = Interval.majorSecond
        , second = Interval.minorSecond
        , third = Interval.majorSecond
        , fourth = Interval.majorSecond
        , fifth = Interval.majorSecond
        , sixth = Interval.minorSecond
        }


augmentedIonian : ScaleClass
augmentedIonian =
    Internal.Heptatonic
        { first = Interval.majorSecond
        , second = Interval.majorSecond
        , third = Interval.minorSecond
        , fourth = Interval.minorThird
        , fifth = Interval.minorSecond
        , sixth = Interval.majorSecond
        }


locrian6 : ScaleClass
locrian6 =
    Internal.Heptatonic
        { first = Interval.minorSecond
        , second = Interval.majorSecond
        , third = Interval.majorSecond
        , fourth = Interval.minorSecond
        , fifth = Interval.minorThird
        , sixth = Interval.minorSecond
        }


majorPhrygian : ScaleClass
majorPhrygian =
    Internal.Heptatonic
        { first = Interval.minorSecond
        , second = Interval.minorThird
        , third = Interval.minorSecond
        , fourth = Interval.majorSecond
        , fifth = Interval.minorSecond
        , sixth = Interval.majorSecond
        }


lydianSharp9 : ScaleClass
lydianSharp9 =
    Internal.Heptatonic
        { first = Interval.majorSecond
        , second = Interval.majorSecond
        , third = Interval.majorSecond
        , fourth = Interval.minorSecond
        , fifth = Interval.majorSecond
        , sixth = Interval.majorSecond
        }


diminishedLydian : ScaleClass
diminishedLydian =
    Internal.Heptatonic
        { first = Interval.majorSecond
        , second = Interval.minorSecond
        , third = Interval.minorThird
        , fourth = Interval.minorSecond
        , fifth = Interval.majorSecond
        , sixth = Interval.majorSecond
        }


arabian : ScaleClass
arabian =
    Internal.Heptatonic
        { first = Interval.majorSecond
        , second = Interval.majorSecond
        , third = Interval.minorSecond
        , fourth = Interval.minorSecond
        , fifth = Interval.majorSecond
        , sixth = Interval.majorSecond
        }


minorLydian : ScaleClass
minorLydian =
    Internal.Heptatonic
        { first = Interval.majorSecond
        , second = Interval.majorSecond
        , third = Interval.majorSecond
        , fourth = Interval.minorSecond
        , fifth = Interval.minorSecond
        , sixth = Interval.majorSecond
        }


balinese : ScaleClass
balinese =
    Internal.Pentatonic
        { first = Interval.minorSecond
        , second = Interval.majorSecond
        , third = Interval.minorThird
        , fourth = Interval.majorSecond
        }


byzantine : ScaleClass
byzantine =
    Internal.Heptatonic
        { first = Interval.minorSecond
        , second = Interval.minorThird
        , third = Interval.minorSecond
        , fourth = Interval.majorSecond
        , fifth = Interval.minorSecond
        , sixth = Interval.minorThird
        }


chinese : ScaleClass
chinese =
    Internal.Pentatonic
        { first = Interval.majorThird
        , second = Interval.majorSecond
        , third = Interval.minorSecond
        , fourth = Interval.majorThird
        }


mongolian : ScaleClass
mongolian =
    Internal.Pentatonic
        { first = Interval.majorSecond
        , second = Interval.majorSecond
        , third = Interval.minorThird
        , fourth = Interval.majorSecond
        }


hindu : ScaleClass
hindu =
    Internal.Heptatonic
        { first = Interval.majorSecond
        , second = Interval.majorSecond
        , third = Interval.minorSecond
        , fourth = Interval.majorSecond
        , fifth = Interval.minorSecond
        , sixth = Interval.majorSecond
        }


egyptian : ScaleClass
egyptian =
    Internal.Pentatonic
        { first = Interval.majorSecond
        , second = Interval.minorThird
        , third = Interval.majorSecond
        , fourth = Interval.minorThird
        }


hirajoshi : ScaleClass
hirajoshi =
    Internal.Pentatonic
        { first = Interval.majorSecond
        , second = Interval.minorSecond
        , third = Interval.majorThird
        , fourth = Interval.minorSecond
        }


eightToneSpanish : ScaleClass
eightToneSpanish =
    Internal.Octatonic
        { first = Interval.minorSecond
        , second = Interval.majorSecond
        , third = Interval.minorSecond
        , fourth = Interval.minorSecond
        , fifth = Interval.minorSecond
        , sixth = Interval.majorSecond
        , seventh = Interval.majorSecond
        }


hungarianMajor : ScaleClass
hungarianMajor =
    Internal.Heptatonic
        { first = Interval.minorThird
        , second = Interval.minorSecond
        , third = Interval.majorSecond
        , fourth = Interval.minorSecond
        , fifth = Interval.majorSecond
        , sixth = Interval.minorSecond
        }


hungarianMinorGipsy : ScaleClass
hungarianMinorGipsy =
    Internal.Heptatonic
        { first = Interval.majorSecond
        , second = Interval.minorSecond
        , third = Interval.minorThird
        , fourth = Interval.minorSecond
        , fifth = Interval.minorSecond
        , sixth = Interval.minorThird
        }


ichikosucho : ScaleClass
ichikosucho =
    Internal.Octatonic
        { first = Interval.majorSecond
        , second = Interval.majorSecond
        , third = Interval.minorSecond
        , fourth = Interval.minorSecond
        , fifth = Interval.minorSecond
        , sixth = Interval.majorSecond
        , seventh = Interval.majorSecond
        }


mohammedan : ScaleClass
mohammedan =
    Internal.Heptatonic
        { first = Interval.majorSecond
        , second = Interval.minorSecond
        , third = Interval.majorSecond
        , fourth = Interval.majorSecond
        , fifth = Interval.minorSecond
        , sixth = Interval.minorThird
        }


neopolitan : ScaleClass
neopolitan =
    Internal.Heptatonic
        { first = Interval.minorSecond
        , second = Interval.majorSecond
        , third = Interval.majorSecond
        , fourth = Interval.majorSecond
        , fifth = Interval.minorSecond
        , sixth = Interval.minorThird
        }


kumoi : ScaleClass
kumoi =
    Internal.Pentatonic
        { first = Interval.majorSecond
        , second = Interval.minorSecond
        , third = Interval.majorThird
        , fourth = Interval.majorSecond
        }


neopolitanMajor : ScaleClass
neopolitanMajor =
    Internal.Heptatonic
        { first = Interval.minorSecond
        , second = Interval.majorSecond
        , third = Interval.majorSecond
        , fourth = Interval.majorSecond
        , fifth = Interval.majorSecond
        , sixth = Interval.majorSecond
        }


neopolitanMinor : ScaleClass
neopolitanMinor =
    Internal.Heptatonic
        { first = Interval.minorSecond
        , second = Interval.majorSecond
        , third = Interval.majorSecond
        , fourth = Interval.majorSecond
        , fifth = Interval.minorSecond
        , sixth = Interval.majorSecond
        }


todiTheta : ScaleClass
todiTheta =
    Internal.Heptatonic
        { first = Interval.minorSecond
        , second = Interval.majorSecond
        , third = Interval.minorThird
        , fourth = Interval.minorSecond
        , fifth = Interval.minorSecond
        , sixth = Interval.minorThird
        }


pelog : ScaleClass
pelog =
    Internal.Pentatonic
        { first = Interval.minorSecond
        , second = Interval.majorSecond
        , third = Interval.majorThird
        , fourth = Interval.minorSecond
        }


prometheus : ScaleClass
prometheus =
    Internal.Hexatonic
        { first = Interval.majorSecond
        , second = Interval.majorSecond
        , third = Interval.majorSecond
        , fourth = Interval.minorThird
        , fifth = Interval.minorSecond
        }


prometheusNeopolitan : ScaleClass
prometheusNeopolitan =
    Internal.Hexatonic
        { first = Interval.minorSecond
        , second = Interval.minorThird
        , third = Interval.majorSecond
        , fourth = Interval.minorThird
        , fifth = Interval.minorSecond
        }


persian : ScaleClass
persian =
    Internal.Heptatonic
        { first = Interval.minorSecond
        , second = Interval.minorThird
        , third = Interval.minorSecond
        , fourth = Interval.minorSecond
        , fifth = Interval.majorSecond
        , sixth = Interval.minorThird
        }


purviTheta : ScaleClass
purviTheta =
    Internal.Heptatonic
        { first = Interval.minorSecond
        , second = Interval.minorThird
        , third = Interval.majorSecond
        , fourth = Interval.minorSecond
        , fifth = Interval.minorSecond
        , sixth = Interval.minorThird
        }
