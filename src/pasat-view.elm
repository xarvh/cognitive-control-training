module PasatView where


import Signal
import Html exposing (..)
import Html.Attributes exposing (style, class, value, disabled)
import Html.Events exposing (onClick, on, targetValue)

import Pasat
import PacedSerialTask


buttonContainerHeight = 280
buttonRadius = 36

makeButton : Signal.Address Pasat.Action -> Int -> Float -> Float -> Html
makeButton address answer radius angle =
    div
        [ onClick address <| Pasat.NestedPstAction <| PacedSerialTask.UserAnswers answer
        , class "number-button"
        , style
            [ ("top", toString (buttonContainerHeight/2 - 1.1 * radius * buttonRadius * cos (turns angle)) ++ "px")
            , ("left", toString (buttonContainerHeight/2 + 1.1 * radius * buttonRadius * sin (turns angle)) ++ "px")
            ]
        ]
        [ text <| toString answer]


-- Lay out the more frequent answers closer to the centre
makeButtons : Signal.Address Pasat.Action -> List Html
makeButtons address =
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

countOutcomes pstModel outcome =
    toString <| List.length <| List.filter ((==) outcome) pstModel.sessionOutcomes


view : Signal.Address Pasat.Action -> Pasat.Model -> Html
view address model =
    div
        [ style [( "text-align", "center")]]
        [ div
            [ style [("display", "inline-block")] ]
            [ div [ class "number-buttons-container" ] <| makeButtons address

            , let
                  (action, label) = if model.pst.isRunning then (PacedSerialTask.ManualStop, "Stop") else (PacedSerialTask.Start, "Start")
              in
                  button [ onClick address (Pasat.NestedPstAction action) ] [ text label ]

            , br [] []
            , br [] []
            ]
        , brbr
        , text <| "Right " ++ countOutcomes model.pst PacedSerialTask.Right
        , brbr
        , text <| "Wrong " ++ countOutcomes model.pst PacedSerialTask.Wrong
        , brbr
        , text <| "Missed " ++ countOutcomes model.pst PacedSerialTask.Missed
        , brbr
        , text "Duration: "
        , input
            [ value <| toString model.pst.duration
            , disabled model.pst.isRunning
            , on "input" targetValue (\newDuration -> Signal.message address <| Pasat.NestedPstAction <| PacedSerialTask.UpdateDuration newDuration)
            ]
            []
        , brbr
        , text "Inter Stimulus Interval:"
        , input
            [ value <| toString model.pst.isi
            , disabled model.pst.isRunning
            , on "input" targetValue (\newIsi -> Signal.message address <| Pasat.NestedPstAction <| PacedSerialTask.UpdateIsi newIsi)
            ]
            []
        , brbr
        , brbr
        , button [ onClick address Pasat.DownloadLog ] [ text "Download full sessions log" ]
        , button [ onClick address Pasat.DownloadAggregateData ] [ text "Download aggregate data" ]
        ]
