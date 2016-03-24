--
-- Paced Serial Task
--
-- This is intended as an abstract, pure module that handles all the task score and time keeping.
--
module Psat where

import Time
import String
import Random
import Debug exposing (log, watch)


--
-- HELPERS
--
-- (This is stuff that should really stay in a libray... -_-
--
randomChoice : List a -> Random.Seed -> (Maybe a, Random.Seed)
randomChoice list seed =
    let
        generator = Random.int 0 (List.length list - 1)
        (index, seed') = Random.generate generator seed
        choice = List.head <| List.drop index list
    in
        (choice, seed')


--
-- Model
--
type Action pq answer
    = AnswerTimeout SessionId
    | UserAnswers answer
    | Start
    | ManualStop
    | AutomaticStop SessionId
    | UpdateIsi String
    | UpdateDuration String


type Outcome
    = Right
    | Wrong
    | Missed


type alias Isi = Int
type alias Duration = Int
type alias SessionId = Int
type alias Key pq answer = (List pq -> Maybe answer)

type alias Model pq answer =
    { key : Key pq answer
    , pqs : List pq
    , userHasAnswered : Bool
    , isRunning : Bool
    , givenPqs : List pq
    , isi : Isi

    -- TODO: once log is in place  maybe we can get rid of these three?
    , missedCount : Int
    , wrongCount : Int
    , rightCount : Int

    , sessionId : SessionId
    , duration : Duration
    , seed : Random.Seed

    , log : List (Time.Time, Action pq answer)
    }

-- TODO: initialise seed with timer
model : Key pq answer -> List pq -> Isi -> Duration -> Model pq answer
model key pqs isi duration =
    Model key pqs True False [] isi 0 0 0 0 duration (Random.initialSeed 0) []


--
-- Triggers
--
-- They are kind of Effects, but they offload the actual dirty work to the caller
--
type Trigger pq answer
    = TriggerDelayedAction Time.Time (Action pq answer)
    | TriggerSound (Maybe pq)


getNewPqTriggers : Model pq answer -> List (Trigger pq answer)
getNewPqTriggers model =
    [ TriggerDelayedAction (toFloat model.isi * Time.millisecond) (AnswerTimeout model.sessionId)
    , TriggerSound (List.head model.givenPqs)
    ]


getTriggers : Model pq answer -> Action pq answer -> List (Trigger pq answer)
getTriggers model action =
    case action of
        Start ->
            TriggerDelayedAction (toFloat model.duration * Time.minute) (AutomaticStop model.sessionId)
            :: getNewPqTriggers model

        AnswerTimeout sessionId ->
            if sessionId /= model.sessionId || not model.isRunning
            then []
            else getNewPqTriggers model

        _ ->
            []


--
-- Model update
--
setOutcome : Model pq answer -> Outcome -> Model pq answer
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


setAnswer : Model pq answer -> Maybe answer -> Model pq answer
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


addRandomPq : Model pq answer -> Model pq answer
addRandomPq model =
    let
        (pq, seed) = randomChoice model.pqs model.seed
    in
       case pq of
           Nothing -> Debug.crash "Zero partial qustions specified!"
           Just pq' ->
               { model
               | givenPqs = pq' :: model.givenPqs
               , userHasAnswered = False
               , seed = seed
               }


updateWhenRunning : Action pq answer -> Model pq answer -> Model pq answer
updateWhenRunning action model =
    case action of
        ManualStop ->
            { model | isRunning = False }

        AutomaticStop sessionId ->
            if sessionId /= model.sessionId then model else { model | isRunning = False }

        UserAnswers answerValue ->
            setAnswer model <| Just answerValue

        AnswerTimeout sessionId ->
            if sessionId /= model.sessionId then model else (setAnswer model Nothing |> addRandomPq)

        _ ->
            model


updateWhenNotRunning : Action pq answer -> Model pq answer -> Model pq answer
updateWhenNotRunning action model =
    case action of
        Start ->
            { model
            | isRunning = True
            , sessionId = model.sessionId + 1
            , givenPqs = []
            } |> addRandomPq

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


--
-- Main update
--
update : (Time.Time, Action pq answer) -> Model pq answer -> (Model pq answer, List (Trigger pq answer))
update (actionTimestamp, action) model =
    let
        updateModel =
            if model.isRunning then updateWhenRunning else updateWhenNotRunning

        updatedModel =
            (updateModel action { model | log = (actionTimestamp, action) :: model.log })
    in
       (updatedModel, getTriggers updatedModel action)
