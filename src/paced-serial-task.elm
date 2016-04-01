--
-- Paced Serial Task
--
-- This is intended as an abstract, pure module that handles all the task score and time keeping.
--
module PacedSerialTask where

import Random
import String
import Task
import Time


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


takeWhile : (a -> Bool) -> List a -> List a
takeWhile predicate list =
    case list of
        x :: xs -> if predicate x then x :: takeWhile predicate xs else []
        [] -> []

runInParallel taskA taskB =
    Task.spawn taskA `Task.andThen` (\_ -> taskB)


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
    , sessionPqs : List pq
    , isi : Isi

    , sessionId : SessionId
    , duration : Duration
    , seed : Random.Seed

    , sessionOutcomes : List Outcome
    }

model : Key pq answer -> List pq -> Isi -> Duration -> Model pq answer
model key pqs isi duration =
    Model key pqs True False [] isi 0 duration (Random.initialSeed 0) []


--
-- Model update
--
-- isi must increase if there are 4 failures in a row, and decrease if there are 4 success in a row.
outcomeDelta : (Outcome -> Bool) -> List Outcome -> Int
outcomeDelta predicate sessionOutcomes =
   let
       count = List.length <| takeWhile predicate sessionOutcomes
   in
      if count > 0 && rem count 4 == 0 then 1 else 0

isiDirection : List Outcome -> Int
isiDirection sessionOutcomes =
    outcomeDelta ((/=) Right) sessionOutcomes - outcomeDelta ((==) Right) sessionOutcomes

setIsi : Model a b -> Model a b
setIsi model =
    { model | isi = isiDirection model.sessionOutcomes * 100 + model.isi }

setOutcome : Model pq answer -> Outcome -> Model pq answer
setOutcome model outcome =
    if model.userHasAnswered
    then model
    else
        { model
        | userHasAnswered = True
        , sessionOutcomes = outcome :: model.sessionOutcomes
        } |> setIsi

setAnswer : Model pq answer -> Maybe answer -> Model pq answer
setAnswer model maybeAnswer =
    case model.key model.sessionPqs of
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
               | sessionPqs = pq' :: model.sessionPqs
               , userHasAnswered = False
               , seed = seed
               }


--
-- Main update
--
type alias TaskFactories pq answer =
    { emitPq : pq -> Task.Task () ()
    , triggerAction : Action pq answer -> Task.Task () ()
    }

update : TaskFactories pq answer -> (Time.Time, Action pq answer) -> Model pq answer -> (Model pq answer, Task.Task () ())
update factories (actionTimestamp, action) oldModel =
    let
        noTask m = (m, Task.succeed ())
        withinSession sessionId m = if sessionId == oldModel.sessionId then m else noTask oldModel
        withinRunning m = if oldModel.isRunning then m else noTask oldModel
        withinWaiting m = if not oldModel.isRunning then m else noTask oldModel

        taskDelayAction delay action =
            Task.andThen (Task.sleep delay) (\_ -> factories.triggerAction action)

        taskNewPq m = runInParallel
            (taskDelayAction (toFloat m.isi * Time.millisecond) (AnswerTimeout m.sessionId))
            (case List.head m.sessionPqs of
                Just pq -> factories.emitPq pq
                Nothing -> Task.succeed ()
            )

    in case action of

        --
        -- Session
        --
        Start -> withinWaiting <|
            let
                updatedModel =
                    { oldModel
                    | isRunning = True
                    , sessionId = oldModel.sessionId + 1
                    , sessionPqs = []
                    , sessionOutcomes = []
                    , seed = Random.initialSeed <| floor actionTimestamp
                    } |> addRandomPq

                task = runInParallel
                    (taskDelayAction (toFloat updatedModel.duration * Time.minute) (AutomaticStop updatedModel.sessionId))
                    (taskNewPq updatedModel)
            in
               (updatedModel, task)

        ManualStop -> withinRunning <|
            noTask { oldModel | isRunning = False }

        AutomaticStop sessionId -> withinRunning <| withinSession sessionId <|
            noTask { oldModel | isRunning = False }

        --
        -- Answers
        --
        UserAnswers answerValue -> withinRunning <|
            noTask <| setAnswer oldModel <| Just answerValue

        AnswerTimeout sessionId -> withinRunning <| withinSession sessionId <|
            let
                updatedModel = setAnswer oldModel Nothing |> addRandomPq
                task = taskNewPq updatedModel
            in
               (updatedModel, task)

        --
        -- Input fields updates
        --
        -- TODO: find a way to deduplicate these
        --
        UpdateIsi isiString -> withinWaiting <|
            noTask <|
                case String.toInt isiString of
                    Ok isi ->
                        { oldModel | isi = isi }

                    Err _ ->
                        oldModel

        UpdateDuration durationString -> withinWaiting <|
            noTask <|
                case String.toInt durationString of
                    Ok duration ->
                        { oldModel | duration = duration }

                    Err _ ->
                        oldModel
