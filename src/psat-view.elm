module PsatView where


import Signal
import Html exposing (..)
import Html.Attributes exposing (style, class, value, disabled)
import Html.Events exposing (onClick, on, targetValue)

import Psat


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

countOutcomes model outcome =
    toString <| List.length <| List.filter ((==) outcome) model.outcomes

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
        , text <| "Right " ++ countOutcomes model Psat.Right
        , brbr
        , text <| "Wrong " ++ countOutcomes model Psat.Wrong
        , brbr
        , text <| "Missed " ++ countOutcomes model Psat.Missed
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
