module Wells exposing (..)

import Dict
import Html exposing (..)
import Html.Attributes exposing (class, disabled)
import Html.Events exposing (onClick)
import List.Extra
import Process
import Platform.Cmd
import Sound
import String
import Task


--
-- MODEL
--


type alias SoundName =
    String


type alias SoundRef =
    { name : SoundName
    , volume : Float
    }

type Tab
    = SoundCheck
    | TaskMenu


type alias Script =
    { name : String
    , soundRef : SoundRef
    , backgroundLoops : List SoundRef
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
        birdVolume =
            0.16

        tappingVolume =
            1.0

        scriptVolume =
            1.0

        base =
          [ SoundRef "background/CarrionCrow" birdVolume
          , SoundRef "background/Chaffinch" birdVolume
          , SoundRef "background/EuropeanGreenfinch" birdVolume
          , SoundRef "background/Fieldfare" birdVolume
          , SoundRef "background/GrasshopperWarbler" birdVolume
          , SoundRef "background/GreatSpottedWoodpecker" birdVolume
          , SoundRef "background/GreatTit" birdVolume
          , SoundRef "background/HerringGull" birdVolume
          , SoundRef "background/HouseSparrow" birdVolume
          ]

        basePlusTapping =
          (SoundRef "background/Tapping" tappingVolume) :: base

    in
        [ Script "Explanation" (SoundRef "scripts/Intro" scriptVolume) base
        , Script "Voice" (SoundRef "scripts/Voice" scriptVolume) base
        , Script "Tapping" (SoundRef "scripts/Tapping" scriptVolume) basePlusTapping
        , Script "Birds" (SoundRef "scripts/Birds" scriptVolume) basePlusTapping
        , Script "Shift" (SoundRef "scripts/Shift" scriptVolume) basePlusTapping
        , Script "Expand" (SoundRef "scripts/Expand" scriptVolume) basePlusTapping
        ]


--
-- INIT
--
init : ( Model, Cmd Message )
init =
    let
        allBgSoundRefs =
            List.map .backgroundLoops scripts
                |> List.concat
                |> List.Extra.uniqueBy .name

        allScriptSoundRefs =
            List.map .soundRef scripts

        allSoundRefs =
            allScriptSoundRefs ++ allBgSoundRefs

        uri soundName =
            "assets/sounds/wells/" ++ soundName ++ ".ogg"

        initModel soundRef =
            Sound.init (uri soundRef.name) soundRef.volume

        ( sounds, cmds ) =
            List.unzip <| List.map initModel allSoundRefs

        soundsByName =
            Dict.fromList <| List.map2 (,) (List.map .name allSoundRefs) sounds

        model =
            Model soundsByName SoundCheck (List.head scripts)

        cmd =
            List.map2 (,) allSoundRefs cmds
                |> List.map (\( ref, cmd ) -> Cmd.map (NestedSoundControl ref.name) cmd)
                |> Cmd.batch
  in
        ( model, cmd )



--
-- Update Helpers
--
soundsPlay : Maybe SoundName -> List SoundRef -> Model -> ( Model, Cmd Message )
soundsPlay scriptSound backgroundSounds oldModel =
    let
        switchSound (soundName, oldSound) =
            let
                mode =
                    if List.any (\r -> r.name == soundName) backgroundSounds then Sound.Loop
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
                dropInit elem list =
                    case list of
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
            currentScriptHasCompleted =
                case oldModel.currentScript of
                    Nothing -> False
                    Just script -> nestedMessage == Sound.PlaybackComplete && soundName == script.soundRef.name

            partialModel =
                if currentScriptHasCompleted
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
                TaskMenu -> Maybe.withDefault [] (Maybe.map .backgroundLoops oldModel.currentScript)
        in
            soundsPlay Nothing background { oldModel | tab = tab }


    UserPlaysCurrentScript ->
        case oldModel.currentScript of
            Nothing -> (oldModel, Cmd.none)
            Just script -> soundsPlay (Just script.soundRef.name) script.backgroundLoops oldModel

    UserSkipsCurrentScript ->
        let
            newModel =
                selectNextScript oldModel

            loops =
                Maybe.map .backgroundLoops newModel.currentScript
                    |> Maybe.withDefault []
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
            div []
                [ text "All done"
                , button [ onClick UserSkipsCurrentScript ] [ text "Start over" ]
                ]

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
      , soundTestButton "background/GreatSpottedWoodpecker" "Woodpecker (right)"
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
