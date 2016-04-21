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
  | StartStopTestSound SoundName
  | StartStopScript Script
  | ScriptComplete Script
  | StopAll


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
  [ "background/CarrionCrow"
  , "background/Chaffinch"
  , "background/CommonPheasant"
  , "background/EuropeanGreenfinch"
  , "background/EuropeanNightjarChurring"
  , "background/Fieldfare"
  , "background/GrasshopperWarbler"
  , "background/GreatSpottedWoodpeckerDrumming"
  , "background/GreatTit"
  , "background/HerringGull"
  , "background/HouseSparrow"
  , "background/LittleGrebe"
  , "background/ReedBunting"
  ]


backgroundSoundsAndTapping =
  "Tapping" :: backgroundSounds

allBackgroundSoundNames =
  backgroundSoundsAndTapping

script0 =
  Script "Explanation" "scripts/Intro" backgroundSounds

scriptF =
  Script "Expand" "scripts/Expand" backgroundSoundsAndTapping

scripts : List Script
scripts =
  [ script0
  , Script "Voice" "scripts/Voice" backgroundSounds
  , Script "Tapping" "scripts/Tapping" backgroundSoundsAndTapping
  , Script "Birds" "scripts/Birds" backgroundSoundsAndTapping
  , Script "Shift" "scripts/Shift" backgroundSoundsAndTapping
  , scriptF
  ]

allScriptSoundNames =
  List.map .soundName scripts

allSoundNames =
   allScriptSoundNames ++ allBackgroundSoundNames


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
-- Sound System
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
      toSimpleTask <| Audio.playSound { defaultPlaybackOptions | loop = True, volume = 0.1 } soundState.sound

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
playBackground : TaskFactories -> List SoundName -> (Model, SimpleTask) -> (Model, SimpleTask)
playBackground factories soundsToLoop initialState =
  let
      controlSound soundName (oldModel, task) =
        let
          shouldLoop = List.member soundName soundsToLoop
          (newModel, soundTask) = (if shouldLoop then loopSound else stopSound) factories soundName oldModel
        in
          (newModel, (Task.spawn soundTask) `Task.andThen` (\_ -> task))
  in
    List.foldl controlSound initialState allBackgroundSoundNames


startStopScript : TaskFactories -> SoundName -> (Model, SimpleTask) -> (Model, SimpleTask)
startStopScript factories scriptSoundName initialState =
  let
      isAlreadyPlaying = case Dict.get scriptSoundName (fst initialState).sounds of
        Nothing -> False
        Just state -> state.playback /= Ready

      controlSound soundName (oldModel, task) =
        let
          shouldPlay = soundName == scriptSoundName && not isAlreadyPlaying
          (newModel, soundTask) = (if shouldPlay then playSound else stopSound) factories soundName oldModel
        in
          (newModel, (Task.spawn soundTask) `Task.andThen` (\_ -> task))
  in
    List.foldl controlSound initialState allScriptSoundNames


--
-- UPDATE
--


update : TaskFactories -> Action -> Model -> ( Model, SimpleTask )
update factories action oldModel =
  let
    startStopSound soundName model =
      case Dict.get soundName model.sounds of
        Nothing -> noTask model
        Just soundState -> (if soundState.playback == Ready then playSound else stopSound) factories soundName model

    stopAll state =
      playBackground factories [] state |>
      startStopScript factories ""

  in case action of

    -- Sound control
    SoundLoaded soundName sound ->
      noTask <| actionSoundLoaded soundName sound oldModel

    PlaybackComplete sound ->
      noTask <| actionPlaybackComplete sound oldModel

    -- UI
    ChangeTab tab ->
      let
        state = noTask { oldModel | tab = tab }
      in case tab of
        TaskMenu -> playBackground factories oldModel.script.backgroundLoops state
        _ -> stopAll state

    StartStopTestSound soundName ->
      startStopSound soundName oldModel

    StartStopScript script ->
      playBackground factories script.backgroundLoops (noTask { oldModel | script = script }) |>
      startStopScript factories script.soundName

    StopAll ->
      stopAll <| noTask oldModel

    ScriptComplete script ->
      noTask oldModel


--
-- VIEW
--

view : Signal.Address Action -> Model -> Html
view address model =
  let

    scriptView script =
      li []
        [ div
          [ onClick address <| StartStopScript script
          , class (if model.script == script then "selected" else "idle") ]
          [ text script.name ]
        ]

    soundTestButton soundName description =
      let
        (message, attr) = case Dict.get soundName model.sounds of
          Nothing -> ("Loading...", disabled True)
          Just soundState -> (description, class (if soundState.playback == Ready then "button-ready" else "button-stop"))
      in
        li []
          [ button
              [ attr
              , onClick address <| StartStopTestSound soundName
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

    scriptNextButton =
      let
          message = if model.script == script0 then "Start" else "Next"
      in
        button [ onClick address <| StartStopScript model.script] [ text message ]

  in case model.tab of
    SoundCheck ->
      div
        []
        [ h1 [] [ text "Sound Check" ]
        , text "Play the sounds and ensure they come from the direction indicated"
        , ul []
          [ soundTestButton "background/CarrionCrow" "Crow (centre)"
          , soundTestButton "background/GreatSpottedWoodpeckerDrumming" "Woodpecker (right)"
          , soundTestButton "background/HerringGull" "Seagull (centre-left)"
          ]

        , readyButton
        ]

    TaskMenu ->
      div
        []
        [ h1 [] [ text "Wells" ]
        , ul [] <| List.map scriptView scripts
        , scriptNextButton
        , button [ onClick address (ChangeTab SoundCheck) ] [ text "back to Sound Check" ]
        , button [ onClick address StopAll ] [ text "Stop" ]
        ]
