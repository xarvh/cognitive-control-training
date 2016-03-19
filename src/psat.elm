-- Paced (Auditory) Serial Addition Test
module Psat where


import Task
import Time
import Signal
import Effects
import String


import Debug exposing (log)





-- Support types
type alias Answer = Int
type alias Pq = Int -- Partial Question. It's "partial" because it takes two+ questions to have a single answer.
type alias Key = (List Pq -> Maybe Answer)

type alias Isi = Int

type Outcome
    = Right
    | Wrong
    | Missed

type Action
    = AnswerTimeout
    | UserAnswers Answer
    | NewPqGiven Pq
    | Start
    | Stop Int
    | UpdateIsi String
    | UpdateDuration String


-- Model
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

    , sessionId : Int
    , duration : Int
    }

init : Key -> List Pq -> Isi -> (Model, Effects.Effects Action)
init key pqs isi =
    ( Model key pqs True False [] isi 0 0 0 0 5
    , Effects.none
    )



-- HELPERS
taskDelayedTrigger : Time.Time -> a -> Task.Task x a
taskDelayedTrigger delay successValue =
    Task.andThen (Task.sleep delay) (\_ -> Task.succeed successValue)





-- PORTS
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







-- Update
update : Action -> Model -> (Model, Effects.Effects Action)
update action model =

    let
        noop = (model, Effects.none)

        requestNewPq : Effects.Effects Action
        requestNewPq =
            Effects.task <| Task.andThen
                (Signal.send portMailboxRequestPq.address model.pqs)
                (\_ -> taskDelayedTrigger (toFloat(model.isi) * Time.millisecond) AnswerTimeout)


        setOutcome : Outcome -> Model
        setOutcome outcome =
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


        setAnswer : Maybe Answer -> Model
        setAnswer maybeAnswer =
            case model.key model.givenPqs of
                Nothing -> model
                Just correctAnswer ->
                    case maybeAnswer of
                        Nothing -> setOutcome Missed
                        Just answer -> setOutcome (if answer == correctAnswer then Right else Wrong)

    in
        if not model.isRunning
        then
            case action of
                Start ->
                    ({ model | isRunning = True, givenPqs = [] }, requestNewPq)

                UpdateIsi isiString ->
                    case String.toInt isiString of
                        Ok isi -> ({ model | isi = isi }, Effects.none)
                        Err _ -> noop

                UpdateDuration durationString ->
                    case String.toInt durationString of
                        Ok duration -> ({ model | duration = duration }, Effects.none)
                        Err _ -> noop

                _ ->
                    noop

        else
            case action of

                Stop sessionId ->
                    ({ model | isRunning = False, sessionId = model.sessionId + 1}, Effects.none)

                UserAnswers answerValue ->
                    (setAnswer (Just answerValue), Effects.none)

                AnswerTimeout ->
                    (setAnswer Nothing, requestNewPq)

                NewPqGiven pq ->
                    ({ model | givenPqs = pq :: model.givenPqs, userHasAnswered = False }, Effects.none)

                _ ->
                    noop
