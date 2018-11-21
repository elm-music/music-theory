module Util.Fuzzer exposing (fromList)

import Fuzz exposing (Fuzzer)
import List.Extra


fromList : List a -> Fuzzer a
fromList xs =
    xs |> List.map Fuzz.constant |> Fuzz.oneOf
