module Sound exposing (..)


import Audio exposing (defaultPlaybackOptions)
import Task


type Status
  = Idle
  | Play
  | Loop


type alias Model =
  { sound : Maybe Audio.Sound
  , status : Status
  }


type Message
  = Noop
  | Error String
  | LoadComplete Audio.Sound
  | SwitchTo Status
  | PlaybackComplete


init : String -> ( Model, Cmd Message )
init uri =
    let
        model = Model Nothing Idle
        cmd = Task.perform Error LoadComplete <| Audio.loadSound uri
    in
       ( model, cmd )


stopSound : Audio.Sound -> Model -> Cmd Message
stopSound sound model =
  Task.perform (always Noop) (always Noop) <| Audio.stopSound sound


playbackOptions model =
  { defaultPlaybackOptions | loop = model.status == Loop}


playSound : Audio.Sound -> Model -> Cmd Message
playSound sound model =
  Task.perform Error (\_ -> PlaybackComplete) <| Audio.playSound (playbackOptions model) sound


update : Message -> Model -> ( Model, Cmd Message )
update message model =
  case message of

    Noop -> ( model, Cmd.none )

    -- TODO
    Error message -> Debug.crash message

    PlaybackComplete -> ( {model | status = Idle}, Cmd.none )

    LoadComplete sound ->
        let
            newModel = {model | sound = Just sound}
            cmd = if model.status == Idle then Cmd.none else playSound sound newModel
        in
            ( newModel, cmd )

    SwitchTo newStatus ->
      if newStatus == model.status
      then ( model, Cmd.none )
      else case model.sound of
        Nothing -> ( {model | status = newStatus}, Cmd.none )
        Just sound ->
            let
                newModel = {model | status = newStatus}
                cmd = if newStatus == Idle then stopSound else playSound
            in
                ( newModel, cmd sound newModel )
