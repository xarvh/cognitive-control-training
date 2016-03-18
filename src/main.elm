import StartApp
import Signal exposing (Address)
import Html exposing (..)
import Html.Attributes exposing (style, class, value, disabled)
import Html.Events exposing (onClick, on, targetValue)

import Task
import Effects

import Psat




import Debug exposing (log)



-- Key
sumOfLastTwoDigits : Psat.Key
sumOfLastTwoDigits list = case list of
    a :: b :: _ -> Just (a + b)
    _ -> Nothing







-- View
makeButtons : Signal.Address Psat.Action -> List Int -> List Html
makeButtons address answers =
    let
        styleByAngle angle =
            style
                [ ("top", toString (280 / 2.5 * (1 - cos angle)) ++ "px")
                , ("left", toString (280 / 2.5 * (1 + sin angle)) ++ "px")
                ]


        makeButton : Int -> Int -> Html
        makeButton index answer =
            div
                [ onClick address <| Psat.UserAnswers answer
                , class "number-button"
                , styleByAngle <| turns <| (toFloat (index + 1)) / (toFloat (List.length answers + 1))
                ]
                [ text <| toString answer]
    in
       List.indexedMap makeButton answers


brbr = br [] []

view : Signal.Address Psat.Action -> Psat.Model -> Html
view address model =
    div
        [ style [( "text-align", "center")]]
        [ div
            [ style [("display", "inline-block")] ]
            [ div [ class "number-buttons-container" ] <| makeButtons address [2..18]
            , button [ onClick address (if model.isRunning then Psat.Stop model.sessionId else Psat.Start) ] [ text (if model.isRunning then "Stop" else "Start") ]
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






app =
  StartApp.start
    { init = Psat.init sumOfLastTwoDigits [1..9] 1000
    , update = Psat.update
    , view = view
    , inputs = [Psat.newPqSignal]
    }


main = app.html

port tasks : Signal (Task.Task Effects.Never ())
port tasks =
  app.tasks
