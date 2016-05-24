module Wells exposing (state0, Message, Model, update, view)

import Audio exposing (defaultPlaybackOptions)
import Dict
import Html exposing (..)
import Html.Attributes exposing (class, disabled)
import Html.Events exposing (onClick)
import Process
import Platform.Cmd
import String
import Task


(&>) = Maybe.andThen
infixl 9 &>


--
-- MODEL
--


type alias SoundName = String


type Tab
  = SoundCheck
  | TaskMenu


type alias Script =
  { name : String
  , soundName : SoundName
  , backgroundLoops : List SoundName
  }


type Message
  = Noop
  | Error String

  | SoundLoads SoundName Audio.Sound
  | UserChangesTab Tab

  | UserPlaysTestSound SoundName

  | UserPlaysCurrentScript
  | UserSkipsCurrentScript
  | CurrentScriptCompletes


type alias Model =
  { sounds : Dict.Dict String Audio.Sound
  , tab : Tab
  , currentScript : Maybe Script
  , isPlaying : Bool
  }


--
-- Scripts
--
baseBackgroundSoundNames : List SoundName
baseBackgroundSoundNames =
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


basePlusTappingSoundNames =
  "background/Tapping" :: baseBackgroundSoundNames

allBackgroundSoundNames =
  basePlusTappingSoundNames

script0 =
  Script "Explanation" "scripts/Intro" baseBackgroundSoundNames

scriptF =
  Script "Expand" "scripts/Expand" basePlusTappingSoundNames

scripts : List Script
scripts =
  [ script0
  , Script "Voice" "scripts/Voice" baseBackgroundSoundNames
  , Script "Tapping" "scripts/Tapping" basePlusTappingSoundNames
  , Script "Birds" "scripts/Birds" basePlusTappingSoundNames
  , Script "Shift" "scripts/Shift" basePlusTappingSoundNames
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


toActionCommand : Task.Task String success -> (success -> Message) -> Cmd Message
toActionCommand task actionConstructor =
  Task.perform Error actionConstructor task


toSilentCommand task =
  toActionCommand task (\_ -> Noop)



--
-- State
--
state0 : ( Model, Cmd Message )
state0 =
  let
    uri soundName =
      "assets/sounds/wells/" ++ soundName ++ ".ogg"

    loadSound soundName =
      toActionCommand (Audio.loadSound <| uri soundName) (SoundLoads soundName)

    cmd0 =
      Platform.Cmd.batch <| List.map loadSound allSoundNames

    model0 =
      Model Dict.empty SoundCheck (Just script0) False
  in
    ( model0, cmd0 )


--
-- Update Helpers
--
playTestSound : String -> Model -> Cmd Message
playTestSound soundName model =
  case Dict.get soundName model.sounds of
    Just sound -> toSilentCommand <| Audio.playSound defaultPlaybackOptions sound
    Nothing -> Cmd.none


stopSounds sounds =
  Cmd.batch <| List.map (\s -> Task.perform (\_ -> Noop) (\_ -> Noop) (Audio.stopSound s)) sounds


stopAllSounds : Model -> ( Model, Cmd Message )
stopAllSounds model =
  ( {model | isPlaying = False}, stopSounds <| Dict.values model.sounds )


stopAllScriptSounds : Model -> ( Model, Cmd Message )
stopAllScriptSounds model =
  let
    allScriptSounds = List.filterMap (\name -> Dict.get name model.sounds) allScriptSoundNames
  in
    ( {model | isPlaying = False}, stopSounds allScriptSounds )


playScript : Model -> ( Model, Cmd Message )
playScript model =
  let
    cmdBg = startBackgroundLoops model
    cmdScript =
      Maybe.withDefault Cmd.none <|
      model.currentScript &> \currentScript ->
      Dict.get currentScript.soundName model.sounds &> \sound ->
      Just <| toActionCommand (Audio.playSound defaultPlaybackOptions sound) (\_ -> CurrentScriptCompletes)
  in
    ( {model | isPlaying = True}, Cmd.batch [cmdBg, cmdScript] )


startBackgroundLoops : Model -> Cmd Message
startBackgroundLoops model =
  -- TODO
  Cmd.none

startBackgroundLoopsWithModel model =
  ( model, startBackgroundLoops model )


selectNextScript : Model -> Model
selectNextScript model =
  -- TODO
  model



update : Message -> Model -> ( Model, Cmd Message )
update message oldModel =
  case message of

    Noop ->
      noCmd oldModel


    -- TODO: show error in page
    Error message ->
      let
          e = Debug.log "wells error" message
      in
         noCmd oldModel


    SoundLoads soundName sound ->
      noCmd { oldModel | sounds = Dict.insert soundName sound oldModel.sounds }


    UserChangesTab tab ->
      let
        updater = case tab of
          SoundCheck -> stopAllSounds
          TaskMenu ->
            case oldModel.currentScript of
              Just script -> startBackgroundLoopsWithModel
              Nothing -> stopAllSounds

      in
        updater { oldModel | tab = tab }


    UserPlaysTestSound soundName ->
      let
        ( newModel, cmdStop ) = stopAllSounds oldModel
        cmdPlay = playTestSound soundName oldModel
      in
        ( oldModel, Cmd.batch [cmdStop, cmdPlay] )


    UserPlaysCurrentScript ->
      let
        ( partialModel, cmdStop ) = stopAllScriptSounds oldModel
        ( newModel, cmdPlay ) = playScript partialModel
      in
        ( newModel, Cmd.batch [cmdStop, cmdPlay] )


    CurrentScriptCompletes ->
      startBackgroundLoopsWithModel <| selectNextScript <| { oldModel | isPlaying = False }


    UserSkipsCurrentScript ->
      stopAllScriptSounds <| selectNextScript <| oldModel





--
-- VIEW
--
viewTaskMenu model =
  case model.currentScript of
    Nothing ->
      text "All done, proceed to PASAT"

    Just script ->
      div
        []
        [ text <| (if model.isPlaying then "Playing: " else "Up Next: ") ++ script.name
        , if model.isPlaying
          then button [ onClick (UserChangesTab SoundCheck) ] [ text "Stop" ]
          else button [ onClick UserPlaysCurrentScript ] [ text "Play" ]
        , button [ onClick <| UserSkipsCurrentScript ] [ text "Skip" ]
        ]


soundTestButton soundName description =
  li []
    [ button [ onClick <| UserPlaysTestSound soundName ] [ text description ] ]


viewSoundCheck model =
  div
    []
    [ h1 [] [ text "Sound Check" ]

    , text "Play the sounds and ensure they come from the direction indicated"

    , ul []
      [ soundTestButton "background/CarrionCrow" "Crow (centre)"
      , soundTestButton "background/GreatSpottedWoodpeckerDrumming" "Woodpecker (right)"
      , soundTestButton "background/HerringGull" "Seagull (centre-left)"
      ]

    , button
      [ class "button-ready"
      , onClick (UserChangesTab TaskMenu)
      ]
      [ text "Start" ]
    ]


view : Model -> Html Message
view model =
  if Dict.size model.sounds < List.length allSoundNames
  then text "Loading..."
  else case model.tab of
    SoundCheck -> viewSoundCheck model
    TaskMenu -> viewTaskMenu model
