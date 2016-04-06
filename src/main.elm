import Signal
import Task
import Time
import Html

import Pasat
import PasatView

type alias SimpleTask = Task.Task () ()


--
-- MODEL
--
type Page = About | Pasat | Walls

type alias Model =
    { page : Page
    , pasat : Pasat.Model
    }

type Action
    = TransitionTo Page
    | PasatAction Pasat.Action


--
-- PORTS & MAILBOXES
--
actionsMailbox : Signal.Mailbox Action
actionsMailbox = Signal.mailbox <| TransitionTo About

-- Download port
downloadPortMailbox : Signal.Mailbox (String, String, String)
downloadPortMailbox = Signal.mailbox ("", "", "")
port downloadPort : Signal.Signal (String, String, String)
port downloadPort = downloadPortMailbox.signal

-- Sound ports
--
-- TODO: slap this into a library?
--
type alias LoadSoundsRecipient = String
loadSoundsRecipientPasat = "Pasat"

loadSoundsPortMailbox : Signal.Mailbox (String, List String)
loadSoundsPortMailbox = Signal.mailbox ("", [])
port loadSoundsPort : Signal.Signal (String, List String)
port loadSoundsPort = loadSoundsPortMailbox.signal
port loadSoundsProgressPort : Signal.Signal (String, Float)
loadSoundsProgressActionSignal =
   let
      map (pageName, progress) =
          if pageName == loadSoundsRecipientPasat
          then PasatAction <| Pasat.SoundLoaded progress
          else TransitionTo About
   in
      Signal.map map loadSoundsProgressPort

playSoundPortMailbox : Signal.Mailbox String
playSoundPortMailbox = Signal.mailbox ""
port playSoundPort : Signal.Signal String
port playSoundPort = playSoundPortMailbox.signal

playSound : String -> Task.Task x ()
playSound name = Signal.send playSoundPortMailbox.address <| name






--
-- FACTORIES
--
taskFactories pageName actionConstructor =
    { playSound = Signal.send playSoundPortMailbox.address
    , loadSounds = Signal.send loadSoundsPortMailbox.address << (,) pageName
    , triggerAction = (Signal.send actionsMailbox.address) << actionConstructor
    , download = Signal.send downloadPortMailbox.address
    }


--
-- UPDATE
--
noTask m = (m, Task.succeed ())

update (timestamp, action) (oldModel, tasks) =
    case action of
        TransitionTo page ->
            noTask oldModel

        PasatAction pasatAction ->
            let
                factories = taskFactories loadSoundsRecipientPasat PasatAction
                (pasatModel, task) = Pasat.update factories (timestamp, pasatAction) oldModel.pasat
            in
               ({ oldModel | pasat = pasatModel }, task)


state0 =
    let
        (pasatModel0, pasatTask0) = Pasat.state0 <| taskFactories loadSoundsRecipientPasat PasatAction
        model = Model Pasat pasatModel0
    in
       (model, pasatTask0)


--
-- VIEW
--
view : Signal.Address Action -> Model -> Html.Html
view address model =
    case model.page of
        About ->
            Html.text "About"
        Walls ->
            Html.text "Walls"
        Pasat ->
            PasatView.view (Signal.forwardTo actionsMailbox.address PasatAction) model.pasat


--
-- MAIN
--
signal = Signal.merge actionsMailbox.signal loadSoundsProgressActionSignal

modelAndTasksSignal = Signal.foldp update state0 (Time.timestamp signal)

main =
    Signal.map ((view actionsMailbox.address) << fst) modelAndTasksSignal

port tasks : Signal (Task.Task () ())
port tasks =
    Signal.map snd modelAndTasksSignal
