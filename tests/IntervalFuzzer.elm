module IntervalFuzzer exposing (intervalFuzzer)

import Fuzz exposing (Fuzzer)
import MusicTheory.Interval as Interval
import List.Extra

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
