import Signal exposing (Address)
import Html exposing (..)
import Html.Attributes exposing (style, class, value, disabled)
import Html.Events exposing (onClick, on, targetValue)

import Time
import Task

import Psat

import Debug exposing (log)


--
-- Answers Key
--
sumOfLastTwoDigits : Psat.Key Int Int
sumOfLastTwoDigits list = case list of
    a :: b :: _ -> Just (a + b)
    _ -> Nothing


--
-- View
--
buttonContainerHeight = 280
buttonRadius = 36

makeButton : Signal.Address (Psat.Action Int Int) -> Int -> Float -> Float -> Html
makeButton address answer radius angle =
    div
        [ onClick address <| Psat.UserAnswers answer
        , class "number-button"
        , style
            [ ("top", toString (buttonContainerHeight/2 - 1.1 * radius * buttonRadius * cos (turns angle)) ++ "px")
            , ("left", toString (buttonContainerHeight/2 + 1.1 * radius * buttonRadius * sin (turns angle)) ++ "px")
            ]
        ]
        [ text <| toString answer]


-- Lay out the more frequent answers closer to the centre
makeButtons : Signal.Address (Psat.Action Int Int) -> List Int -> List Html
makeButtons address answers =
    let
        bt = makeButton address
    in
        -- centre
        [   bt 10 0 0

        -- inner circle (from top, clockwise)
        ,   bt 11 1 <| 1/12
        ,   bt 12 1 <| 3/12
        ,   bt 13 1 <| 5/12
        ,   bt 9 1 <| 7/12
        ,   bt 8 1 <| 9/12
        ,   bt 7 1 <| 11/12

        -- outer circle (from top, clockwise)
        ,   bt 14 2 <| 1/12
        ,   bt 15 2 <| 2/12
        ,   bt 16 2 <| 3/12
        ,   bt 17 2 <| 4/12
        ,   bt 18 2 <| 5/12

        ,   bt 6 2 <| 7/12
        ,   bt 5 2 <| 8/12
        ,   bt 4 2 <| 9/12
        ,   bt 3 2 <| 10/12
        ,   bt 2 2 <| 11/12
        ]


brbr = br [] []

view : Signal.Address (Psat.Action Int Int) -> Psat.Model Int Int -> Html
view address model =
    div
        [ style [( "text-align", "center")]]
        [ div
            [ style [("display", "inline-block")] ]
            [ div [ class "number-buttons-container" ] <| makeButtons address [2..18]
            , button [ onClick address (if model.isRunning then Psat.ManualStop else Psat.Start) ] [ text (if model.isRunning then "Stop" else "Start") ]
            , br [] []
            , br [] []
            ]
        , brbr
        , text <| "Right " ++ toString model.rightCount
        , brbr
        , text <| "Wrong " ++ toString model.wrongCount
        , brbr
        , text <| "Missed " ++ toString model.missedCount
        , brbr
        , text "Duration: "
        , input
            [ value <| toString model.duration
            , disabled model.isRunning
            , on "input" targetValue (\newDuration -> Signal.message address <| Psat.UpdateDuration newDuration)
            ]
            []
        , brbr
        , text "Inter Stimulus Interval:"
        , input
            [ value <| toString model.isi
            , disabled model.isRunning
            , on "input" targetValue (\newIsi -> Signal.message address <| Psat.UpdateIsi newIsi)
            ]
            []
        , brbr
        , brbr
        , button [] [ text "Download full session log" ]
        , button [] [ text "Download aggregate data" ]
        ]


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


update : Psat.Action Int Int -> (Psat.Model Int Int, List (Psat.Trigger Int Int)) -> (Psat.Model Int Int, List (Psat.Trigger Int Int))
update action (model, triggers) =
    Psat.update action model

modelAndTriggersSignal =
    Signal.foldp update (model0, []) actionsMailbox.signal



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
    Signal.map (view actionsMailbox.address << fst) modelAndTriggersSignal


port tasks : Signal (Task.Task x ())
port tasks =
    Signal.map (triggersToTask actionsMailbox.address << snd) modelAndTriggersSignal

