module View exposing (view)

import Html exposing (Html, div, text)
import Model exposing (Model, Msg)


view : Model -> Html Msg
view model =
    div []
        [ text "Hello world!" ]
