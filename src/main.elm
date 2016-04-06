import Signal
import Task
import Time

import Pasat
import PasatView


--
-- PORTS
--

-- Download port
downloadPortMailbox : Signal.Mailbox (String, String, String)
downloadPortMailbox = Signal.mailbox ("", "", "")
port downloadPort : Signal.Signal (String, String, String)
port downloadPort = downloadPortMailbox.signal

-- Sound ports
loadSoundsPortMailbox : Signal.Mailbox (List String)
loadSoundsPortMailbox = Signal.mailbox []
port loadSoundsPort : Signal.Signal (List String)
port loadSoundsPort = loadSoundsPortMailbox.signal

port loadSoundsProgressPort : Signal.Signal Float

playSoundPortMailbox : Signal.Mailbox String
playSoundPortMailbox = Signal.mailbox ""
port playSoundPort : Signal.Signal String
port playSoundPort = playSoundPortMailbox.signal

playSound : String -> Task.Task x ()
playSound name = Signal.send playSoundPortMailbox.address <| name


--
-- MAIN
--
actionsMailbox : Signal.Mailbox Pasat.Action
actionsMailbox = Signal.mailbox <| Pasat.SelectVoice ""

taskFactories =
    { playSound = Signal.send playSoundPortMailbox.address
    , loadSounds = Signal.send loadSoundsPortMailbox.address
    , triggerAction = Signal.send actionsMailbox.address
    , download = Signal.send downloadPortMailbox.address
    }


signal = Signal.merge actionsMailbox.signal (Signal.map Pasat.SoundLoaded loadSoundsProgressPort)



update timestampedAction (model, tasks) =
    Pasat.update taskFactories timestampedAction model

modelAndTasksSignal =
    Signal.foldp update (Pasat.state0 taskFactories) (Time.timestamp signal)

main =
    Signal.map ((PasatView.view actionsMailbox.address) << fst) modelAndTasksSignal

port tasks : Signal (Task.Task () ())
port tasks =
    Signal.map snd modelAndTasksSignal
