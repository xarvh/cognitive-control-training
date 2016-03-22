import Signal exposing (Address)
import Html exposing (..)
import Html.Attributes exposing (style, class, value, disabled)
import Html.Events exposing (onClick, on, targetValue)

import Task
import Effects

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
-- TODO: provide a method to create the appropriate Task/Effect
-- have update be `update : Action -> Model -> (Model, Maybe Task Action)`

{-
-- Outgoing port
portMailboxRequestPq : Signal.Mailbox (List pq)
portMailboxRequestPq =
  Signal.mailbox []

port requestPq : Signal.Signal (List pq)
port requestPq =
  portMailboxRequestPq.signal
  -}









-------------------------------------------------------------------

model0 = Psat.model sumOfLastTwoDigits [1..9] 1000 5


actionsMailbox : Signal.Mailbox (Maybe action)
actionsMailbox =
    Signal.mailbox Nothing

-- updateStep : action -> (model, Effects.Effects action) -> (model, Effects.Effects action)
updateStep action (oldModel, accumulatedEffects) =
    let
        (newModel, additionalEffects) = Psat.update action oldModel
    in
        (newModel, Effects.batch [accumulatedEffects, additionalEffects])

-- update : List action -> (model, Effects.Effects action) -> (model, Effects.Effects action)
update actions (model, _) =
    List.foldl updateStep (model, Effects.none) actions


-- effectsAndModel : Signal (model, Effects action)
(modelSignal, triggersSignal) =
    Signal.foldp update (model0, []) messages.signal




main =
    Signal.map (view actionsMailbox.address) model

port tasks : Signal (Task.Task Effects.Never ())
port tasks =
    Signal.map (Effects.toTask actionsMailbox.address ) 

