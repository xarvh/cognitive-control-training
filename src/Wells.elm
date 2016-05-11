module Wells exposing (..)

import Audio exposing (defaultPlaybackOptions)
import Dict
import Html exposing (..)
import Html.Attributes exposing (class, disabled)
import Html.Events exposing (onClick)
import Process
import Platform.Cmd
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
  = Noop

  -- Playback
  | SoundLoaded SoundName Audio.Sound
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
  "background/Tapping" :: backgroundSounds

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
noCmd model =
  ( model, Platform.Cmd.none )


toActionCommand : Task.Task error success -> (success -> Action) -> Cmd Action
toActionCommand task actionConstructor =
  Task.perform (\_ -> Debug.crash "XXX") actionConstructor task


toSilentCommand task =
  toActionCommand task (\_ -> Noop)



--
-- Sound System
--
-- These functions help ensure that the model is always up to date regarding the playback state of each sound.
--
state0 : ( Model, Cmd Action )
state0 =
  let
    uri soundName =
      "assets/sounds/wells/" ++ soundName ++ ".ogg"

    loadSound soundName =
      toActionCommand (Audio.loadSound <| uri soundName) (SoundLoaded soundName)

    cmd0 =
      Platform.Cmd.batch <| List.map loadSound allSoundNames

    model0 =
      Model Dict.empty SoundCheck script0
  in
    ( model0, cmd0 )


actionSoundLoaded : SoundName -> Audio.Sound -> Model -> Model
actionSoundLoaded soundName sound model =
  let
      key = soundName
      value = SoundState sound Ready
      sounds = Dict.insert key value model.sounds
  in
     { model | sounds = sounds }


syncSound : SoundState -> Cmd Action
syncSound soundState =
  case soundState.playback of
    Ready ->
      toSilentCommand <| Audio.stopSound soundState.sound

    PlayingLoop ->
      toSilentCommand <| Audio.playSound { defaultPlaybackOptions | loop = True, volume = 0.1 } soundState.sound

    Playing ->
      toActionCommand (Audio.playSound defaultPlaybackOptions soundState.sound) (\_ -> PlaybackComplete soundState.sound)


actionPlaybackComplete : Audio.Sound -> Model -> Model
actionPlaybackComplete sound model =
  let
      readySound soundName soundState = if soundState.sound == sound then { soundState | playback = Ready } else soundState
      sounds = Dict.map readySound model.sounds
  in
     { model | sounds = sounds }


controlSound : PlaybackState -> SoundName -> Model -> (Model, Cmd Action)
controlSound playback soundName model =
  case Dict.get soundName model.sounds of
    Nothing -> noCmd model
    Just soundState ->
      if soundState.playback == playback
      then noCmd model
      else
        let
            state = { soundState | playback = playback }
            sounds = Dict.insert soundName state model.sounds
            cmd = syncSound state
        in
           ({ model | sounds = sounds }, cmd)


playSound = controlSound Playing
loopSound = controlSound PlayingLoop
stopSound = controlSound Ready


--
--
--
playBackground : List SoundName -> (Model, Cmd Action) -> (Model, Cmd Action)
playBackground soundsToLoop initialState =
  let
      controlSound soundName (oldModel, oldCmd) =
        let
          shouldLoop = List.member soundName soundsToLoop
          (newModel, soundCmd) = (if shouldLoop then loopSound else stopSound) soundName oldModel
        in
          (newModel, Platform.Cmd.batch [oldCmd, soundCmd])
  in
    List.foldl controlSound initialState allBackgroundSoundNames


startStopScript : SoundName -> (Model, Cmd Action) -> (Model, Cmd Action)
startStopScript scriptSoundName initialState =
  let
      isAlreadyPlaying = case Dict.get scriptSoundName (fst initialState).sounds of
        Nothing -> False
        Just state -> state.playback /= Ready

      controlSound soundName (oldModel, oldCmd) =
        let
          shouldPlay = soundName == scriptSoundName && not isAlreadyPlaying
          (newModel, soundCmd) = (if shouldPlay then playSound else stopSound) soundName oldModel
        in
          (newModel, Platform.Cmd.batch [oldCmd, soundCmd])
  in
    List.foldl controlSound initialState allScriptSoundNames


--
-- UPDATE
--


update : Action -> Model -> ( Model, Cmd Action )
update action oldModel =
  let
    startStopSound soundName model =
      case Dict.get soundName model.sounds of
        Nothing -> noCmd model
        Just soundState -> (if soundState.playback == Ready then playSound else stopSound) soundName model

    stopAll state =
      playBackground [] state |>
      startStopScript ""

  in case action of
    Noop ->
      noCmd oldModel

    -- Sound control
    SoundLoaded soundName sound ->
      noCmd <| actionSoundLoaded soundName sound oldModel

    PlaybackComplete sound ->
      noCmd <| actionPlaybackComplete sound oldModel

    -- UI
    ChangeTab tab ->
      let
        state = noCmd { oldModel | tab = tab }
      in case tab of
        TaskMenu -> playBackground oldModel.script.backgroundLoops state
        _ -> stopAll state

    StartStopTestSound soundName ->
      startStopSound soundName oldModel

    StartStopScript script ->
      playBackground script.backgroundLoops (noCmd { oldModel | script = script }) |>
      startStopScript script.soundName

    StopAll ->
      stopAll <| noCmd oldModel

    ScriptComplete script ->
      noCmd oldModel


--
-- VIEW
--

view : Model -> Html Action
view model =
  let

    scriptView script =
      let
          isCurrent = model.script == script

          label = script.name ++ (if isCurrent then " Play/Stop" else "")
      in
        li []
          [ span
            [ onClick <| StartStopScript script
            , class (if isCurrent then "selected" else "idle")
            ]
            [ text label ]
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
              , onClick <| StartStopTestSound soundName
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
          , onClick (ChangeTab TaskMenu)
          ]
          [ text message ]

    scriptNextButton =
      let
          message = if model.script == script0 then "Start" else "Next"
      in
        button [ onClick <| StartStopScript model.script] [ text message ]

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
        , button [ onClick (ChangeTab SoundCheck) ] [ text "back to Sound Check" ]
        , button [ onClick StopAll ] [ text "Stop" ]
        ]
