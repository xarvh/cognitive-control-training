port module Main exposing (..)

import Html exposing (div, text)
import Html.App
import Wells
import Pasat



--
-- MODEL
--


Model = {
  lol : String
}


--
-- PORTS
--
port downloadPort : ( String, String, String ) -> Cmd msg



--
-- UPDATE
--


update : Action -> Model -> ( Model, Cmd Action )
update action oldModel =
  oldModel



state0 =
  ( "", Cmd.none )



--
-- VIEW
--


view : Model -> Html.Html Action
view model =
  text "lol"


--
-- MAIN
--


main =
  Html.App.program
    { init = state0
    , view = view
    , update = update
    , subscriptions = \_ -> Sub.none
    }
