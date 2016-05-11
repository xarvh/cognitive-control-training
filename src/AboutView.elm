module AboutView exposing (..)

import Html exposing (..)
import Html.Attributes exposing (href)


view =
  div
    []
    [ h1 [] [ text "Cognitive Tasks" ]
    , p [] [ text "The tasks available from this page are used for cognitive exercises." ]
    , p [] [ text "A relevant bibliography will be hopefully added soon." ]
    , p [] [ text "For any technical issues, please <a href='https://github.com/xarvh/'>contact me from my github page</a>." ]
    , p [] [ text "These pages are undergoing heavy development, expect things to change without notice." ]
    , p [] [ text "" ]
    , p [] [ text "Please note that no data will be sent to any server, so if you leave a Task's page without downloading the results, the data will be lost." ]
    , p [] [ text "" ]
    , p [] [ text "" ]
      --     <a href='tasks/numbers/index.html'>Numbers Task</a>
    ]
