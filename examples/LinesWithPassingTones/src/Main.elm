module Main exposing (main)

import Browser
import Html
import Model exposing (Model, Msg)
import View


main : Program () Model Msg
main =
    Browser.sandbox
        { init = Model.init
        , view = View.view
        , update = Model.update
        }
