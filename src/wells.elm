module Wells (..) where

import Audio exposing (defaultPlaybackOptions)
import Dict
import Html exposing (..)
import Html.Events exposing (onClick)
import Signal
import String
import Task


--
-- MODEL
--


type Tab
  = SoundCheck
  | TaskMenu


type SoundName
  = Crow
  | Woodpecker
  | Seagull
  | Tapping
  | IntroScript
  | VoiceScript
  | TappingScript
  | BirdsScript
  | ShiftScript
  | ExpandScript


type alias Script =
  { name : String
  , soundName : SoundName
  , backgroundLoops : List SoundName
  }


type Action
  = ChangeTab Tab
  | SoundsLoaded (Dict.Dict String Audio.Sound)
  | PlaySound SoundName
    --     | PlaybackFinished ...
  | ScriptFinished
  | PlayScript Script


type alias SoundState =
  { sound : Audio.Sound
  , isLooping : Bool
  }


type alias Model =
  { sounds : Dict.Dict String SoundState
  , tab : Tab
  , script : Script
  , isScriptPlaying : Bool
  }


type alias SimpleTask =
  Task.Task String ()


type alias TaskFactories =
  { triggerAction : Action -> SimpleTask }


noTask model =
  ( model, Task.succeed () )

taskDrop =
  Task.map (\_ -> ())



backgroundSounds : List SoundName
backgroundSounds =
  [ Crow
  , Woodpecker
  , Seagull
  ]


backgroundSoundsAndTapping =
  Tapping :: backgroundSounds


script0 =
  Script "Explanation" IntroScript backgroundSounds

scriptF =
  Script "Expand" ExpandScript backgroundSoundsAndTapping

scripts : List Script
scripts =
  [ script0
  , Script "Voice" VoiceScript backgroundSounds
  , Script "Tapping" TappingScript backgroundSoundsAndTapping
  , Script "Birds" BirdsScript backgroundSoundsAndTapping
  , Script "Shift" ShiftScript backgroundSoundsAndTapping
  , scriptF
  ]


state0 : TaskFactories -> ( Model, SimpleTask )
state0 factories =
  let
    allSounds =
      List.map .soundName scripts ++ backgroundSoundsAndTapping

    nameAndUri soundName =
      ( toString soundName, "assets/sounds/wells/" ++ toString soundName ++ ".ogg" )

    task0 =
      Task.andThen
        (Audio.loadSoundsDict <| List.map nameAndUri allSounds)
        (factories.triggerAction << SoundsLoaded)

    model0 =
      Model Dict.empty SoundCheck script0 False
  in
    ( model0, task0 )



--
-- start/Stop background sounds
--


resetSoundscape : List SoundName -> Model -> ( Model, SimpleTask )
resetSoundscape soundsToLoop model =
  let
    controlSound soundsToLoop targetName targetState ( sounds, task ) =
      let
        loop = { defaultPlaybackOptions | loop = True }
        shouldLoop = List.member targetName soundsToLoop
        newTask = (if shouldLoop && not targetState.isLooping then Audio.playSound loop else Audio.stopSound) targetState.sound
        newState = { targetState | isLooping = shouldLoop }
        newSounds = Dict.insert targetName newState sounds
      in
        ( newSounds, Task.andThen (Task.spawn newTask) (\_ -> task) )

    -- Why Oh Why can't use use Algebraic types as dictionary keys? =(
    travestyAndLampoonery =
      List.map toString soundsToLoop

    ( sounds, task ) =
      Dict.foldl (controlSound travestyAndLampoonery) ( Dict.empty, Task.succeed () ) model.sounds
  in
    ( { model | sounds = sounds, isScriptPlaying = False }, task )


--
-- MODEL
--


update : Action -> Model -> ( Model, SimpleTask )
update action oldModel =
  case action of

    SoundsLoaded soundsByName ->
      let
        map k v =
          { isLooping = False, sound = v }
      in
        noTask { oldModel | sounds = Dict.map map soundsByName }

    ChangeTab tab ->
      let
        update' =
          case tab of
            TaskMenu -> resetSoundscape oldModel.script.backgroundLoops
            _ -> resetSoundscape []
      in
        update' { oldModel | tab = tab }

    PlayScript script ->
--       let
--           m = { oldModel | script = script, isScriptPlaying = True }
--           (m', task) = resetSoundscape m.script.backgroundLoops
--          task: kill all script sounds
--          task: play script sound andThen trigger soundFinished
      noTask oldModel

    PlaySound soundName ->
      case Dict.get (toString soundName) oldModel.sounds of
        Nothing -> noTask oldModel
        Just soundState -> (oldModel, taskDrop <| Task.spawn <| Audio.playSound defaultPlaybackOptions soundState.sound)

    _ ->
      noTask oldModel



--
-- VIEW
--

-- disableWhenPlaying model sound =
--   disabled <| Maybe.withDefault False <| Dict.get sound


view : Signal.Address Action -> Model -> Html
view address model =
  let
    scriptView script =
      li
        [ onClick address <| PlayScript script ]
        [ text script.name ]


  in case model.tab of
    SoundCheck ->
      div
        []
        [ h1 [] [ text "Sound Check" ]
        , text "Play the sounds and ensure they come from the direction indicated"
        , ul []
          [ li [] [ button [ onClick address (PlaySound Crow) ] [ text "Crow (centre)" ] ]
          , li [] [ button [ onClick address (PlaySound Woodpecker) ] [ text "Woodpecker (right)" ] ]
          , li [] [ button [ onClick address (PlaySound Seagull) ] [ text "Seagull (centre-left)" ] ]
          ]

        , button [ onClick address (ChangeTab TaskMenu) ] [ text "Ok, I'm ready!" ]
        ]

    TaskMenu ->
      div
        []
        [ h1 [] [ text "Wells" ]
        , ul [] <| List.map scriptView scripts
        , button [ ] [ text (if model.script == script0 then "Start" else "Next") ]
        , button [ onClick address (ChangeTab SoundCheck) ] [ text "back to Sound Check" ]
        ]
