module Wells (..) where

import Audio exposing (defaultPlaybackOptions)
import Dict
import Html exposing (..)
import Html.Attributes exposing (class, disabled)
import Html.Events exposing (onClick)
import Signal
import String
import Task


--
-- MODEL
--


type alias SoundName = String


type Tab
  = SoundCheck
  | TaskMenu


type PlaybackState
  = Ready
  | Playing
  | PlayingLoop


type alias SoundState =
  { sound : Audio.Sound
  , playback : PlaybackState
  }


type alias Script =
  { name : String
  , soundName : SoundName
  , backgroundLoops : List SoundName
  }


type Action
  -- Playback
  = SoundLoaded SoundName Audio.Sound
  | PlaybackComplete Audio.Sound

  -- Ui
  | ChangeTab Tab
  | PlayTestSound SoundName
  | PlayScript Script
  | ScriptComplete Script


type alias Model =
  { sounds : Dict.Dict String SoundState
  , tab : Tab
  , script : Script
  }


type alias SimpleTask =
  Task.Task String ()


type alias TaskFactories =
  { triggerAction : Action -> SimpleTask }


--
-- Scripts
--
backgroundSounds : List SoundName
backgroundSounds =
  [ "Crow"
  , "Woodpecker"
  , "Seagull"
  -- , TODO: add all other sounds
  ]


backgroundSoundsAndTapping =
  "Tapping" :: backgroundSounds


script0 =
  Script "Explanation" "IntroScript" backgroundSounds

scriptF =
  Script "Expand" "ExpandScript" backgroundSoundsAndTapping

scripts : List Script
scripts =
  [ script0
  , Script "Voice" "VoiceScript" backgroundSounds
  , Script "Tapping" "TappingScript" backgroundSoundsAndTapping
  , Script "Birds" "BirdsScript" backgroundSoundsAndTapping
  , Script "Shift" "ShiftScript" backgroundSoundsAndTapping
  , scriptF
  ]


allSoundNames =
  List.map .soundName scripts ++ backgroundSoundsAndTapping


--
-- Helpers
--
noTask model =
  ( model, Task.succeed () )

dropTaskResult =
  Task.map (\_ -> ())

toSimpleTask =
  Task.mapError (\_ -> "")

--
-- Sound State helpers
--
-- These functions help ensure that the model is always up to date regarding the playback state of each sound.
--
state0 : TaskFactories -> ( Model, SimpleTask )
state0 factories =
  let
    uri soundName =
      "assets/sounds/wells/" ++ soundName ++ ".ogg"

    loadSound soundName =
      (Audio.loadSound <| uri soundName)
      `Task.andThen`
      (factories.triggerAction << SoundLoaded soundName)

    task0 =
      dropTaskResult <| Task.sequence <| List.map loadSound allSoundNames

    model0 =
      Model Dict.empty SoundCheck script0
  in
    ( model0, task0 )


actionSoundLoaded : SoundName -> Audio.Sound -> Model -> Model
actionSoundLoaded soundName sound model =
  let
      key = soundName
      value = SoundState sound Ready
      sounds = Dict.insert key value model.sounds
  in
     { model | sounds = sounds }


syncSound : TaskFactories -> SoundState -> SimpleTask
syncSound factories soundState =
  case soundState.playback of
    Ready ->
      toSimpleTask <| Audio.stopSound soundState.sound

    PlayingLoop ->
      toSimpleTask <| Audio.playSound { defaultPlaybackOptions | loop = True } soundState.sound

    Playing ->
      (toSimpleTask <| Audio.playSound defaultPlaybackOptions soundState.sound)
      `Task.andThen`
      \_ -> factories.triggerAction <| PlaybackComplete soundState.sound


actionPlaybackComplete : Audio.Sound -> Model -> Model
actionPlaybackComplete sound model =
  let
      readySound soundName soundState = if soundState.sound == sound then { soundState | playback = Ready } else soundState
      sounds = Dict.map readySound model.sounds
  in
     { model | sounds = sounds }


controlSound : PlaybackState -> TaskFactories -> SoundName -> Model -> (Model, SimpleTask)
controlSound playback factories soundName model =
  case Dict.get soundName model.sounds of
    Nothing -> noTask model
    Just soundState ->
      if soundState.playback == playback
      then noTask model
      else
        let
            state = { soundState | playback = playback }
            sounds = Dict.insert soundName state model.sounds
            task = syncSound factories state
        in
           ({ model | sounds = sounds }, task)


playSound = controlSound Playing
loopSound = controlSound PlayingLoop
stopSound = controlSound Ready


--
--
--
resetSoundscape : TaskFactories -> List SoundName -> Model -> ( Model, SimpleTask )
resetSoundscape factories soundsToLoop model =
  let
      fold soundName soundState (oldModel, task) =
        let
          shouldLoop = List.member soundName soundsToLoop
          (newModel, soundTask) = (if shouldLoop then loopSound else stopSound) factories soundName oldModel
        in
          (newModel, (Task.spawn soundTask) `Task.andThen` (\_ -> task))
  in
    Dict.foldl fold (noTask model) model.sounds


--
-- MODEL
--


update : TaskFactories -> Action -> Model -> ( Model, SimpleTask )
update factories action oldModel =
  case action of

    -- Sound control
    SoundLoaded soundName sound ->
      noTask <| actionSoundLoaded soundName sound oldModel

    PlaybackComplete sound ->
      noTask <| actionPlaybackComplete sound oldModel

    -- UI
    ChangeTab tab ->
      let
        update' =
          case tab of
            TaskMenu -> resetSoundscape factories oldModel.script.backgroundLoops
            _ -> resetSoundscape factories []
      in
        update' { oldModel | tab = tab }

    PlayTestSound soundName ->
      case Dict.get soundName oldModel.sounds of
        Nothing -> noTask oldModel
        Just soundState -> (if soundState.playback == Ready then playSound else stopSound) factories soundName oldModel

    PlayScript script ->
      noTask oldModel

    ScriptComplete script ->
      noTask oldModel


--
-- VIEW
--

view : Signal.Address Action -> Model -> Html
view address model =
  let
    scriptView script =
      li
        [ onClick address <| PlayScript script ]
        [ text script.name ]

    soundTestButton soundName description =
      let
        (message, attr) = case Dict.get soundName model.sounds of
          Nothing -> ("Loading...", disabled True)
          Just soundState -> (description, class (if soundState.playback == Ready then "button-ready" else "button-stop"))
      in
        li []
          [ button
              [ attr
              , onClick address <| PlayTestSound soundName
              ]
              [ text message ]
          ]

    readyButton =
      let
          (message, attr) =
            if Dict.size model.sounds < List.length allSoundNames
            then ("Loading...", disabled True)
            else ("Ok, I'm ready!", class "button-ready")
      in
        button
          [ attr
          , onClick address (ChangeTab TaskMenu)
          ]
          [ text message ]

  in case model.tab of
    SoundCheck ->
      div
        []
        [ h1 [] [ text "Sound Check" ]
        , text "Play the sounds and ensure they come from the direction indicated"
        , ul []
          [ soundTestButton "Crow" "Crow (centre)"
          , soundTestButton "Woodpecker" "Woodpecker (right)"
          , soundTestButton "Seagull" "Seagull (centre-left)"
          ]

        , readyButton
        ]

    TaskMenu ->
      div
        []
        [ h1 [] [ text "Wells" ]
        , ul [] <| List.map scriptView scripts
        , button [ ] [ text (if model.script == script0 then "Start" else "Next") ]
        , button [ onClick address (ChangeTab SoundCheck) ] [ text "back to Sound Check" ]
        ]
