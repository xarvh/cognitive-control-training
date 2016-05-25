module Wells exposing (..)

import Dict
import Html exposing (..)
import Html.Attributes exposing (class, disabled)
import Html.Events exposing (onClick)
import Process
import Platform.Cmd
import Set
import Sound
import String
import Task


(&>) = Maybe.andThen
-- https://github.com/elm-lang/elm-compiler/issues/1394
-- infixl 9 &>


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
    = NestedSoundControl SoundName Sound.Message

    | UserPlaysTestSound SoundName

    | UserChangesTab Tab -- Also used to stop all playback
    | UserPlaysCurrentScript
    | UserSkipsCurrentScript


type alias Model =
    { sounds : Dict.Dict String Sound.Model
    , tab : Tab
    , currentScript : Maybe Script
    }



--
-- SCRIPTS
--
scripts : List Script
scripts =
    let
        base =
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

        basePlusTapping =
          "background/Tapping" :: base

    in
        [ Script "Explanation" "scripts/Intro" base
        , Script "Voice" "scripts/Voice" base
        , Script "Tapping" "scripts/Tapping" basePlusTapping
        , Script "Birds" "scripts/Birds" basePlusTapping
        , Script "Shift" "scripts/Shift" basePlusTapping
        , Script "Expand" "scripts/Expand" basePlusTapping
        ]


--
-- INIT
--
init : ( Model, Cmd Message )
init =
    let
        allBgSoundNames =
            Set.toList <| Set.fromList <| List.concat <| List.map .backgroundLoops scripts

        allScriptSoundNames =
            List.map .soundName scripts

        allSoundNames =
            allScriptSoundNames ++ allBgSoundNames

        uri soundName =
            "assets/sounds/wells/" ++ soundName ++ ".ogg"

        ( sounds, cmds ) =
            List.unzip <| List.map (Sound.init << uri) allSoundNames

        soundsByName =
            Dict.fromList <| List.map2 (,) allSoundNames sounds

        model =
            Model soundsByName SoundCheck (List.head scripts)

        cmd =
            Cmd.batch <| List.map (\( name, cmd ) -> Cmd.map (NestedSoundControl name) cmd) <| List.map2 (,) allSoundNames cmds
  in
        ( model, cmd )



--
-- Update Helpers
--
soundsPlay : Maybe SoundName -> List SoundName -> Model -> ( Model, Cmd Message )
soundsPlay scriptSound backgroundSounds oldModel =
    let
        switchSound (soundName, oldSound) =
            let
                mode =
                    if List.member soundName backgroundSounds then Sound.Loop
                    else if scriptSound == Just soundName then Sound.Play
                    else Sound.Idle

                ( newSound, cmd ) =
                    Sound.update (Sound.SwitchTo mode) oldSound
            in
                ( ( soundName, newSound ), ( soundName, cmd ) )

        ( namesAndSounds, namesAndCmds ) = List.unzip <| List.map switchSound <| Dict.toList oldModel.sounds

        sounds = Dict.fromList namesAndSounds

        cmds = List.map (\( name, cmd ) -> Cmd.map (NestedSoundControl name) cmd) namesAndCmds
    in
        ( { oldModel | sounds = sounds }, Cmd.batch cmds )



selectNextScript : Model -> Model
selectNextScript model =
    case model.currentScript of
        Nothing -> { model | currentScript = List.head scripts }
        Just currentScript ->
            let
                dropInit elem list = case list of
                    x :: xs -> if x == elem then List.head xs else dropInit elem xs
                    _ -> Nothing
            in
                { model | currentScript = dropInit currentScript scripts }



update : Message -> Model -> ( Model, Cmd Message )
update message oldModel =
  case message of

    NestedSoundControl soundName nestedMessage ->
        let
            -- Intercept PlaybackComplete
            partialModel =
                if nestedMessage == Sound.PlaybackComplete && Just soundName == (oldModel.currentScript &> (Just << .soundName))
                then selectNextScript oldModel
                else oldModel

        in case Dict.get soundName partialModel.sounds of
            Nothing -> Debug.crash <| "invalid sound " ++ soundName
            Just oldSound ->
                let
                    ( newSound, soundCmd ) = Sound.update nestedMessage oldSound
                    newModel = { partialModel | sounds = Dict.insert soundName newSound partialModel.sounds }
                in
                    ( newModel, Cmd.map (NestedSoundControl soundName) soundCmd )


    UserPlaysTestSound soundName ->
        soundsPlay (Just soundName) [] oldModel


    UserChangesTab tab ->
        let
            background = case tab of
                SoundCheck -> []
                TaskMenu -> Maybe.withDefault [] (oldModel.currentScript &> (Just << .backgroundLoops))
        in
            soundsPlay Nothing background { oldModel | tab = tab }


    UserPlaysCurrentScript ->
        Maybe.withDefault (oldModel, Cmd.none) <|
        oldModel.currentScript &> \currentScript ->
        Just <| soundsPlay (Just currentScript.soundName) currentScript.backgroundLoops oldModel


    UserSkipsCurrentScript ->
        let
            newModel = selectNextScript oldModel
            loops = Maybe.withDefault [] <| newModel.currentScript &> (Just << .backgroundLoops)
        in
            soundsPlay Nothing loops newModel



--
-- VIEW
--
viewTaskMenu model =
    let
        isPlaying = List.any (\s -> s.status == Sound.Play) <| Dict.values model.sounds

    in case model.currentScript of
        Nothing ->
            text "All done, proceed to PASAT"

        Just script ->
            div
                []
                -- TODO is playing or not?
                [ text <| (if isPlaying then "Playing: " else "Up Next: ") ++ script.name
                , if isPlaying
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
  if Dict.values model.sounds |> List.map .sound |> List.any ((==) Nothing)
  then text "Loading..."
  else case model.tab of
    SoundCheck -> viewSoundCheck model
    TaskMenu -> viewTaskMenu model
