module Wells (..) where

import Audio
import Dict
import Html exposing (..)
import Signal
import String
import Task


--
-- MODEL
--
type Tab
    = SoundCheck
    | TaskMenu

type alias SoundName = String

type alias Script =
    { name : String
    , scriptName : SoundName
    , backgroundLoops : List SoundName
    }

type Action
    = ChangeTab Tab
--     | PlaybackFinished ...
    | ScriptFinished
    | PlayScript Script

type alias Model =
    { sounds : Dict.Dict String (Bool, Audio.Sound)
    , tab : Tab
    , script : Script
    , isScriptPlaying : Bool
    }

backgroundSounds : List SoundName
backgroundSounds =
    [
    ]

backgroundSoundsAndTapping =
    "wellstap2l" :: backgroundSounds

script0 : Script
script0 = Script "Explanation" "wellsscript1l" backgroundSounds

scripts : List Script
scripts =
    [ script0
    , Script "Voice" "wellsscript2l" backgroundSounds
    , Script "Tapping" "wellsscript2l" backgroundSoundsAndTapping
    , Script "Birds" "wellsallbirdsfocusl" backgroundSoundsAndTapping
    , Script "Shift" "wellsbirdshiftl" backgroundSoundsAndTapping
    , Script "Expand" "wellsscriptexpandl" backgroundSoundsAndTapping
    ]

model0 =
    Model Dict.empty SoundCheck script0 False

allSounds =
    List.map .scriptName scripts ++ backgroundSoundsAndTapping




--
-- MODEL
--
update : Action -> Model -> (Model, Task.Task String ())
update action oldModel =
    case action of
        ChangeTab tab ->
            (oldModel, Task.succeed ())

        _ ->
            (oldModel, Task.succeed ())


--
-- VIEW
--
view : Signal.Address Action -> Model -> Html
view address model =
    case model.tab of
        SoundCheck -> text "Sound Check"
        TaskMenu -> text "Task Menu"

