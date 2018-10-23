module Util.PitchClassFuzzer exposing (pitchClassFuzzer)

import Fuzz exposing (Fuzzer)
import MusicTheory.Letter exposing (Letter(..))
import MusicTheory.PitchClass exposing (..)


numberToLetter n =
    case n of
        0 ->
            C

        1 ->
            D

        2 ->
            E

        3 ->
            F

        4 ->
            G

        5 ->
            A

        6 ->
            B

        other ->
            if other > 0 then
                numberToLetter (other - 7)

            else
                numberToLetter (other + 7)


numberToAccidental n =
    if n == -3 then
        tripleFlat

    else if n == -2 then
        doubleFlat

    else if n == -1 then
        flat

    else if n == 0 then
        natural

    else if n == 1 then
        sharp

    else if n == 2 then
        doubleSharp

    else if n == 3 then
        tripleSharp

    else if n < -3 then
        numberToAccidental (n + 7)

    else
        numberToAccidental (n - 7)


pitchClassFuzzer =
    Fuzz.map2
        pitchClass
        (Fuzz.intRange 0 6 |> Fuzz.map numberToLetter)
        (Fuzz.intRange -3 3 |> Fuzz.map numberToAccidental)
