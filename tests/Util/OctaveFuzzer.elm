module Util.OctaveFuzzer exposing (octave)

import Fuzz exposing (Fuzzer)
import List.Extra
import MusicTheory.Octave as Octave exposing (Octave)
import Util.Fuzzer


octave : Fuzzer Octave
octave =
    Util.Fuzzer.fromList Octave.all
