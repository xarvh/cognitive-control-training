import Signal
import Time
import Task

import Psat
import PsatView

import Debug exposing (log)


--
-- Answers Key
--
sumOfLastTwoDigits : Psat.Key Int Int
sumOfLastTwoDigits list = case list of
    a :: b :: _ -> Just (a + b)
    _ -> Nothing


-------------------------------------------------------------------
taskDelayedTrigger : Time.Time -> a -> Task.Task x a
taskDelayedTrigger delay successValue =
    Task.andThen (Task.sleep delay) (\_ -> Task.succeed successValue)




--
-- PORTS
--

-- Sound port
playSoundPortMailbox : Signal.Mailbox Int
playSoundPortMailbox =
  Signal.mailbox 0

port playSoundPort : Signal.Signal Int
port playSoundPort =
  playSoundPortMailbox.signal


-------------------------------------------------------------------

model0 = Psat.model sumOfLastTwoDigits [1..9] 3000 5

actionsMailbox : Signal.Mailbox (Psat.Action Int Int)
actionsMailbox =
    Signal.mailbox Psat.ManualStop


update : (Time.Time, Psat.Action Int Int) -> (Psat.Model Int Int, List (Psat.Trigger Int Int)) -> (Psat.Model Int Int, List (Psat.Trigger Int Int))
update timestampedAction (model, triggers) =
    Psat.update timestampedAction model

modelAndTriggersSignal =
    Signal.foldp update (model0, []) (Time.timestamp actionsMailbox.signal)



triggerToTask : Signal.Address (Psat.Action Int Int) -> Psat.Trigger Int Int -> Task.Task x ()
triggerToTask address trigger =
    case trigger of
        Psat.TriggerDelayedAction delay action ->
            Task.andThen (Task.sleep delay) (\_ -> Signal.send address action)

        Psat.TriggerSound maybePq ->
            case maybePq of
                Just pq -> Signal.send playSoundPortMailbox.address pq
                Nothing -> Task.succeed ()



triggersToTask : Signal.Address (Psat.Action Int Int) -> List (Psat.Trigger Int Int) -> Task.Task x ()
triggersToTask address triggers =
    let
        tasks = List.map (triggerToTask address) triggers

        squash taskA taskB =
            Task.spawn taskA `Task.andThen` (\_ -> taskB)

    in
        List.foldl squash (Task.succeed ()) tasks




main =
    Signal.map (PsatView.view actionsMailbox.address << fst) modelAndTriggersSignal


port tasks : Signal (Task.Task x ())
port tasks =
    Signal.map (triggersToTask actionsMailbox.address << snd) modelAndTriggersSignal
