module Util.IntervalFuzzer exposing (interval)

import Fuzz exposing (Fuzzer)
import List.Extra
import MusicTheory.Interval as Interval


interval : Fuzzer Interval.Interval
interval =
    Fuzz.intRange 0 ((Interval.all |> List.length) - 1)
        |> Fuzz.map
            (\n ->
                case Interval.all |> List.Extra.getAt n of
                    Just x ->
                        x

                    Nothing ->
                        Debug.todo "impossible"
            )
