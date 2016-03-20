--
-- Paced Serial Task
--
-- This is intended as an abstract, pure module that handles all the task score and time keeping.
--
module Psat where

import Task
import Time
import Signal
import Effects
import String

--import Debug exposing (log)


--
-- HELPERS
--
taskDelayedTrigger : Time.Time -> a -> Task.Task x a
taskDelayedTrigger delay successValue =
    Task.andThen (Task.sleep delay) (\_ -> Task.succeed successValue)


--
-- Model
--
type alias Answer = Int
type alias Pq = Int -- Partial Question. It's "partial" because it takes two+ questions to have a single answer.
type alias Key = (List Pq -> Maybe Answer)

type Outcome
    = Right
    | Wrong
    | Missed

type alias Isi = Int
type alias Duration = Int
type alias SessionId = Int

type alias Model =
    { key : Key
    , pqs : List Pq
    , userHasAnswered : Bool
    , isRunning : Bool
    , givenPqs : List Pq
    , isi : Isi

    -- TODO: once log is in place  maybe we can get rid of these three?
    , missedCount : Int
    , wrongCount : Int
    , rightCount : Int

    , sessionId : SessionId
    , duration : Duration
    }

model : Key -> List Pq -> Isi -> Duration -> Model
model key pqs isi duration =
    Model key pqs True False [] isi 0 0 0 0 duration


type Action
    = AnswerTimeout SessionId
    | UserAnswers Answer
    | NewPqGiven Pq
    | Start
    | ManualStop
    | AutomaticStop SessionId
    | UpdateIsi String
    | UpdateDuration String


--
-- PORTS
--
-- TODO: remove all ports from the module, let main provide a method to create the appropriate Task/Effect
-- have update be `update : Action -> Model -> (Model, Maybe Task Action)`
-- There's no reason for this module to have ports or maintain an Effects dependency

-- Outgoing port
portMailboxRequestPq : Signal.Mailbox (List Pq)
portMailboxRequestPq =
  Signal.mailbox []

port requestPq : Signal.Signal (List Pq)
port requestPq =
  portMailboxRequestPq.signal

-- Incoming port
-- TODO: eliminate this and use elm-core random generators instead
port newPq : Signal.Signal Pq
newPqSignal : Signal.Signal Action
newPqSignal =
  Signal.map NewPqGiven newPq


--
-- Update
--
requestNewPq : Model -> Effects.Effects Action
requestNewPq model =
    Effects.task <| Task.andThen
        (Signal.send portMailboxRequestPq.address model.pqs)
        (\_ -> taskDelayedTrigger (toFloat model.isi * Time.millisecond) (AnswerTimeout model.sessionId))


setOutcome : Model -> Outcome -> Model
setOutcome model outcome =
    if model.userHasAnswered
    then model
    else
        let
            m = { model | userHasAnswered = True }
        in case outcome of
            -- TODO is there a less stupid way to do this? >_<
            Right -> { m | rightCount = m.rightCount + 1 }
            Wrong -> { m | wrongCount = m.wrongCount + 1 }
            Missed -> { m | missedCount = m.missedCount + 1 }


setAnswer : Model -> Maybe Answer -> Model
setAnswer model maybeAnswer =
    case model.key model.givenPqs of
        Nothing ->
            model

        Just correctAnswer ->
            case maybeAnswer of
                Nothing ->
                    setOutcome model Missed

                Just answer ->
                    setOutcome model <| if answer == correctAnswer then Right else Wrong



updateWhenRunning : Action -> Model -> (Model, Effects.Effects Action)
updateWhenRunning action model =
    let
        effect = case action of
            AnswerTimeout sessionId ->
                if sessionId /= model.sessionId then Effects.none else requestNewPq model

            _ ->
                Effects.none

        model' = case action of
            ManualStop ->
                { model | isRunning = False }

            AutomaticStop sessionId ->
                if sessionId /= model.sessionId then model else { model | isRunning = False }

            UserAnswers answerValue ->
                setAnswer model <| Just answerValue

            AnswerTimeout sessionId ->
                if sessionId /= model.sessionId then model else setAnswer model Nothing

            NewPqGiven pq ->
                { model | givenPqs = pq :: model.givenPqs, userHasAnswered = False }

            _ ->
                model

    in
       (model', effect)


updateWhenNotRunning : Action -> Model -> (Model, Effects.Effects Action)
updateWhenNotRunning action model =
    let
        effect = case action of
            Start ->
                Effects.batch
                    [ requestNewPq model'
                    , Effects.task <| taskDelayedTrigger (toFloat model.duration * Time.minute) <| AutomaticStop model'.sessionId
                    ]

            _ ->
                Effects.none

        model' = case action of
            Start ->
                { model
                | isRunning = True
                , sessionId = model.sessionId + 1
                , givenPqs = []
                }

            UpdateIsi isiString ->
                case String.toInt isiString of
                    Ok isi ->
                        { model | isi = isi }

                    Err _ ->
                        model

            UpdateDuration durationString ->
                case String.toInt durationString of
                    Ok duration ->
                        { model | duration = duration }

                    Err _ ->
                        model

            _ ->
                model

    in
       (model', effect)


update : Action -> Model -> (Model, Effects.Effects Action)
update action model =
    let
        update' = if model.isRunning then updateWhenRunning else updateWhenNotRunning
    in
        update' action model
