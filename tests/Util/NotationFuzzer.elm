module Util.NotationFuzzer exposing (notationFuzzer)

import Fuzz exposing (Fuzzer)
import MusicTheory.Duration as Duration exposing (Duration)
import MusicTheory.Notation exposing (..)
import Util.DurationFuzzer as DurationFuzzer
import Util.Fuzzer
import Util.KeyFuzzer
import Util.TimeSignatureFuzzer
import Util.TupletFuzzer as TupletFuzzer


noteAttributeFuzzer : Fuzzer (List NoteAttribute)
noteAttributeFuzzer =
    Util.Fuzzer.fromList [ Tie ] |> Fuzz.list


restAttributeFuzzer : Fuzzer (List RestAttribute)
restAttributeFuzzer =
    Util.Fuzzer.fromList [ Invisible ] |> Fuzz.list


noteFuzzer : Fuzzer a -> Fuzzer (Voice a)
noteFuzzer aFuzzer =
    Fuzz.map3 note noteAttributeFuzzer aFuzzer DurationFuzzer.duration


restFuzzer : Fuzzer (Voice a)
restFuzzer =
    Fuzz.map2 rest restAttributeFuzzer DurationFuzzer.duration


chordNoteFuzzer : Fuzzer a -> Fuzzer (ChordNote a)
chordNoteFuzzer aFuzzer =
    Fuzz.map2 chordNote noteAttributeFuzzer aFuzzer


chordFuzzer : Fuzzer a -> Fuzzer (Voice a)
chordFuzzer aFuzzer =
    Fuzz.map3 chord noteAttributeFuzzer (Fuzz.list (chordNoteFuzzer aFuzzer)) DurationFuzzer.duration


barAttributeFuzzer : Fuzzer (List BarAttribute)
barAttributeFuzzer =
    Util.Fuzzer.fromList [ DoubleLine ] |> Fuzz.list


voiceFuzzer : Fuzzer a -> Fuzzer (Voice a)
voiceFuzzer aFuzzer =
    let
        sequence =
            Fuzz.list (Fuzz.oneOf [ noteFuzzer aFuzzer, restFuzzer, chordFuzzer aFuzzer ])
    in
    Fuzz.oneOf
        [ Fuzz.map2 bar barAttributeFuzzer sequence
        , Fuzz.map beam sequence
        , Fuzz.map2 tuplet TupletFuzzer.tuplet sequence
        ]


staffFuzzer : Fuzzer a -> Fuzzer (Staff a)
staffFuzzer aFuzzer =
    Fuzz.map2 Staff (Fuzz.oneOf [ Fuzz.constant Treble, Fuzz.constant Bass ]) (Fuzz.list (voiceFuzzer aFuzzer))


notationFuzzer : Fuzzer a -> Fuzzer (Notation a)
notationFuzzer aFuzzer =
    Fuzz.map Notation (Fuzz.maybe Fuzz.string)
        |> Fuzz.andMap (Fuzz.maybe Fuzz.string)
        |> Fuzz.andMap Util.KeyFuzzer.key
        |> Fuzz.andMap Util.TimeSignatureFuzzer.timeSignature
        |> Fuzz.andMap (Fuzz.maybe (Fuzz.map2 Tuple.pair DurationFuzzer.duration Fuzz.float))
        |> Fuzz.andMap (Fuzz.list (staffFuzzer aFuzzer))
