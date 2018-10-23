module Util.IntervalFuzzer exposing (intervalFuzzer)

import Fuzz exposing (Fuzzer)
import List.Extra
import MusicTheory.Interval as Interval


intervalFuzzer : Fuzzer Interval.Interval
intervalFuzzer =
    Fuzz.intRange 0 ((Interval.all |> List.length) - 1)
        |> Fuzz.map
            (\n ->
                case Interval.all |> List.Extra.getAt n of
                    Just interval ->
                        interval

                    Nothing ->
                        Debug.todo "impossible"
            )
