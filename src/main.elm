module Main (..) where

import Signal
import Task
import Time
import Html exposing (div, text)
import Html.Events exposing (onClick)
import Html.Attributes exposing (disabled)
import AboutView
import Wells
import Pasat
import PasatView


type alias SimpleTask =
  Task.Task () ()



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
  = TransitionTo Page
  | WellsAction Wells.Action
  | PasatAction Pasat.Action



--
-- PORTS & MAILBOXES
--


actionsMailbox : Signal.Mailbox Action
actionsMailbox =
  Signal.mailbox <| TransitionTo About



-- Download port


downloadPortMailbox : Signal.Mailbox ( String, String, String )
downloadPortMailbox =
  Signal.mailbox ( "", "", "" )


port downloadPort : Signal.Signal ( String, String, String )
port downloadPort =
  downloadPortMailbox.signal



--
-- FACTORIES
--


taskFactories actionConstructor =
  { triggerAction = (Signal.send actionsMailbox.address) << actionConstructor
  , download = Signal.send downloadPortMailbox.address
  }



--
-- UPDATE
--


noTask m =
  ( m, Task.succeed () )


update ( timestamp, action ) ( oldModel, tasks ) =
  case action of
    TransitionTo page ->
      noTask { oldModel | page = page }

    WellsAction wellsAction ->
      let
        factories =
          { triggerAction = (taskFactories WellsAction).triggerAction }

        ( wellsModel, task ) =
          Wells.update factories wellsAction oldModel.wells
      in
        ( { oldModel | wells = wellsModel }, task )

    PasatAction pasatAction ->
      let
        factories =
          taskFactories PasatAction

        ( pasatModel, task ) =
          Pasat.update factories ( timestamp, pasatAction ) oldModel.pasat
      in
        ( { oldModel | pasat = pasatModel }, task )


state0 =
  let
    ( pasatModel0, pasatTask0 ) =
      Pasat.state0 <| taskFactories PasatAction

    ( wellsModel0, wellsTask0 ) =
      Wells.state0 <| { triggerAction = (taskFactories WellsAction).triggerAction }

    model =
      Model Wells pasatModel0 wellsModel0
  in
    ( model, wellsTask0 `Task.andThen` \_ -> pasatTask0 )



--
-- VIEW
--


view : Signal.Address Action -> Model -> Html.Html
view address model =
  let
    page =
      case model.page of
        About ->
          AboutView.view

        Wells ->
          Wells.view (Signal.forwardTo actionsMailbox.address WellsAction) model.wells

        Pasat ->
          PasatView.view (Signal.forwardTo actionsMailbox.address PasatAction) model.pasat

    pageSelector page =
      div [ onClick address <| TransitionTo page, disabled <| model.page == page ] [ text <| toString page ]
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


modelAndTasksSignal =
  Signal.foldp update state0 (Time.timestamp actionsMailbox.signal)


main =
  Signal.map ((view actionsMailbox.address) << fst) modelAndTasksSignal


port tasks : Signal (Task.Task String ())
port tasks =
  Signal.map snd modelAndTasksSignal
