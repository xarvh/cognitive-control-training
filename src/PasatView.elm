module PasatView exposing (..)

import Html exposing (..)
import Html.Attributes exposing (style, class, value, disabled, selected)
import Html.Events exposing (onClick, on, targetValue)
import Json.Decode
import Pasat
import PacedSerialTask


buttonContainerHeight =
  280


buttonRadius =
  36



-- TODO move this out of here


voices =
  [ "english/ossi"
  , "italiano/fra"
  ]


languageOptions currentVoice =
  let
    voiceToOption voice =
      option
        [ selected (voice == currentVoice)
        , value voice
        ]
        [ text voice ]
  in
    List.map voiceToOption voices


makeButton : Int -> Float -> Float -> Html Pasat.Action
makeButton answer radius angle =
  div
    [ onClick <| Pasat.NestedPstAction <| PacedSerialTask.UserAnswers answer
    , class "number-button"
    , style
        [ ( "top", toString (buttonContainerHeight / 2 - buttonRadius / 2 - 1.1 * radius * buttonRadius * cos (turns angle)) ++ "px" )
        , ( "left", toString (buttonContainerHeight / 2 - buttonRadius / 2 + 1.1 * radius * buttonRadius * sin (turns angle)) ++ "px" )
        ]
    ]
    [ text <| toString answer ]



-- Lay out the more frequent answers closer to the centre


makeButtons : List (Html Pasat.Action)
makeButtons =
  let
    bt = makeButton
  in
    -- centre
    [ bt 10 0 0
      -- inner circle (from top, clockwise)
    , bt 11 1 <| 1 / 12
    , bt 12 1 <| 3 / 12
    , bt 13 1 <| 5 / 12
    , bt 9 1 <| 7 / 12
    , bt 8 1 <| 9 / 12
    , bt 7 1 <| 11 / 12
      -- outer circle (from top, clockwise)
    , bt 14 2 <| 1 / 12
    , bt 15 2 <| 2 / 12
    , bt 16 2 <| 3 / 12
    , bt 17 2 <| 4 / 12
    , bt 18 2 <| 5 / 12
    , bt 6 2 <| 7 / 12
    , bt 5 2 <| 8 / 12
    , bt 4 2 <| 9 / 12
    , bt 3 2 <| 10 / 12
    , bt 2 2 <| 11 / 12
    ]


countOutcomes pstModel outcome =
  toString <| List.length <| List.filter ((==) outcome) pstModel.sessionOutcomes


view : Pasat.Model -> Html Pasat.Action
view model =
  let
    instructions =
      section
        [ class "pasat-instruction" ]
        [ text "\n                When you press Start, you will hear a voice calling out numbers:\n                press the button corresponding to the SUM of the LAST TWO numbers you HEARD.\n                For example, if you hear \"two, four, seven\", you will press button 6 (because 2 + 4 = 6)\n                and then 11 (because 4 + 7 = 11)"
        ]

    language =
      div
        [ class "row" ]
        [ div
            [ class "col col-label" ]
            [ text "Voice language" ]
        , div
            [ class "col col-value" ]
            [ select
                [ disabled model.pst.isRunning
                , on "change" (Json.Decode.map Pasat.SelectVoice targetValue)
                ]
                (languageOptions model.voice)
            ]
        ]

    numericInput : String -> Float -> (String -> PacedSerialTask.Action Int Int) -> String -> Html Pasat.Action
    numericInput label v action units =
      div
        [ class "row" ]
        [ div
            [ class "col col-label" ]
            [ text label ]
        , div
            [ class "col col-value" ]
            [ input
                [ value <| toString v
                , disabled model.pst.isRunning
                , on "input" (Json.Decode.map (Pasat.NestedPstAction << action) targetValue)
                ]
                []
            , text units
            ]
        ]

    duration =
      numericInput "Duration" (toFloat model.pst.duration) PacedSerialTask.UpdateDuration "minutes"

    isi =
      numericInput "Inter Stimulus Interval" (toFloat model.pst.isi / 1000) PacedSerialTask.UpdateIsi "seconds"

    controls =
      div
        [ class "pasat-controls"
        , style [ ( "text-align", "center" ) ]
        ]
        [ div
            [ style [ ( "display", "inline-block" ) ] ]
            [ div
                [ class "number-buttons-container" ]
                (makeButtons)
            , let
                ( action, label ) =
                  if model.pst.isRunning then
                    ( PacedSerialTask.ManualStop, "Stop" )
                  else
                    ( PacedSerialTask.Start, "Start" )
              in
                button [ onClick (Pasat.NestedPstAction action) ] [ text label ]
            ]
        ]

    scorecard =
      let
        score outcome =
          div
            [ class "row" ]
            [ div
                [ class "col col-label" ]
                [ text <| toString outcome ]
            , div
                [ class "col col-value" ]
                [ text <| countOutcomes model.pst outcome ]
            ]
      in
        div
          [ class "pasat-scorecard" ]
          <| List.map score [ PacedSerialTask.Right, PacedSerialTask.Wrong, PacedSerialTask.Missed ]

    download =
      section
        [ class "pasat-download" ]
        [ button [ onClick Pasat.DownloadLog ] [ text "Download full outcomes log" ]
        , button [ onClick Pasat.DownloadAggregateData ] [ text "Download aggregate outcomes" ]
        ]

    -- groups
    options =
      section
        [ class "pasat-options" ]
        [ language, duration, isi ]

    results =
      section
        [ class "pasat-results" ]
        [ if model.pst.sessionId /= 0 then
            scorecard
          else
            none
        , if model.pst.sessionId /= 0 && not model.pst.isRunning then
            download
          else
            none
        ]

    none =
      div [] []
  in
    div
      []
      [ controls
      , results
      , if model.pst.isRunning then
          none
        else
          instructions
      , if model.pst.isRunning then
          none
        else
          options
      ]
