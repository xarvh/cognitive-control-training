import StartApp
import Signal exposing (Address)
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)

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
view : Signal.Address Psat.Action -> Psat.Model -> Html
view address model =
    div
        [ style [( "text-align", "center")]]
        [ div
            [ style [("display", "inline-block")] ]
            [ div
                []
                [text "buttons here" ]
            , button [ onClick address (if model.isRunning then Psat.Stop else Psat.Start) ] [ text (if model.isRunning then "Stop" else "Start") ]
            , br [] []
            , br [] []
            ]
        , text <| "m" ++ toString (model.missedCount + model.wrongCount + model.rightCount)
        ]

{-
            <div id='hideable'>
                Duration: <input id='duration' value=5 /> minutes<br>
                Inter-stimulus interval: <input id='isi' value=4000 /> milliseconds<br>
                (this is the the time before the next digit is called).<br>
                <br>

                Right: <span id='right'></span><br>

                Missed: <span id='miss'></span><br>

                Wrong: <span id='wrong'></span><br>

                <br>
                <br>

                <button id='download-log' >Download full session log</button><br>
                <button id='download-aggregate' >Download aggregate data</button><br>
-}






app =
  StartApp.start
    { init = Psat.init sumOfLastTwoDigits 1000
    , update = Psat.update
    , view = view
    , inputs = [Psat.newPqSignal]
    }


main = app.html

port tasks : Signal (Task.Task Effects.Never ())
port tasks =
  app.tasks
