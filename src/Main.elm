port module Main exposing (..)

import Task
import Time
import Html exposing (div, text)
import Html.App
import Html.Events exposing (onClick)
import Html.Attributes exposing (disabled)
import AboutView
import Wells
import Pasat
import PasatView



--
-- MODEL
--


type Page
  = About
  | Pasat
  | Wells


type alias Model =
  { page : Page
  , pasat : Pasat.Model
  , wells : Wells.Model
  }


type Action
  = Noop
  | TransitionTo Page
  | WellsAction Wells.Action
  | PasatAction Pasat.Action



--
-- PORTS
--
port downloadPort : ( String, String, String ) -> Cmd msg



--
-- UPDATE
--


noCmd m =
  ( m, Cmd.none )


update : Action -> Model -> ( Model, Cmd Action )
update action oldModel =

  case action of
    TransitionTo page ->
      noCmd { oldModel | page = page }

    WellsAction wellsAction ->
      let
        ( wellsModel, wellsCmd ) =
          Wells.update wellsAction oldModel.wells
      in
        ( { oldModel | wells = wellsModel }, Cmd.map WellsAction wellsCmd )

    PasatAction pasatAction ->
      let
        ( pasatModel, pasatCmd ) =
          Pasat.update downloadPort ( 0, pasatAction ) oldModel.pasat
      in
        ( { oldModel | pasat = pasatModel }, Cmd.map PasatAction pasatCmd )

    Noop ->
      noCmd oldModel



state0 =
  let
    ( pasatModel0, pasatCmd0 ) =
      Pasat.state0

    ( wellsModel0, wellsCmd0 ) =
      Wells.state0

    model =
      Model Wells pasatModel0 wellsModel0
  in
    ( model, Cmd.batch [Cmd.map WellsAction wellsCmd0, Cmd.map PasatAction pasatCmd0] )



--
-- VIEW
--


view : Model -> Html.Html Action
view model =
  let
    page =
      case model.page of
        About ->
          AboutView.view

        Wells ->
          Html.App.map WellsAction <| Wells.view model.wells

        Pasat ->
          Html.App.map PasatAction <| PasatView.view model.pasat

    pageSelector page =
      div [ onClick <| TransitionTo page, disabled <| model.page == page ] [ text <| toString page ]
  in
    div
      []
      [ div
          []
          <| List.map pageSelector [ About, Wells, Pasat ]
      , page
      ]



--
-- MAIN
--


-- update' message state =
--   let
--     (model, task) = update message state
--     cmd = Task.perform (\_ -> Noop) (\_ -> Noop) task
--   in
--     (model, cmd)



main =
  Html.App.program
    { init = state0
    , view = view
    , update = update
    , subscriptions = \_ -> Sub.none
    }
