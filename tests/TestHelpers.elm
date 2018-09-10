module TestHelpers exposing (intervalFuzzer)

import Fuzz exposing (Fuzzer)
import List.Extra exposing ((!!))
import Tonal.Interval as Interval


intervalFuzzer : Fuzzer Interval.Interval
intervalFuzzer =
    Fuzz.intRange 0 ((Interval.all |> List.length) - 1)
        |> Fuzz.map
            (\n ->
                case Interval.all !! n of
                    Just interval ->
                        interval

                    Nothing ->
                        Debug.crash "impossible"
            )
