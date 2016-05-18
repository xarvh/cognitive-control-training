--
-- Paced Serial Task
--
-- This is intended as an abstract, pure module that handles all the task score and time keeping.
--


module PacedSerialTask exposing (..)

import Process
import Random
import String
import Task
import Time


--
-- HELPERS
--
-- (This is stuff that should really stay in a libray... -_-)
--


randomChoice : List a -> Random.Seed -> ( Maybe a, Random.Seed )
randomChoice list seed =
  let
    generator =
      Random.int 0 (List.length list - 1)

    ( index, seed' ) =
      Random.step generator seed

    choice =
      List.head <| List.drop index list
  in
    ( choice, seed' )


takeWhile : (a -> Bool) -> List a -> List a
takeWhile predicate list =
  case list of
    x :: xs ->
      if predicate x then
        x :: takeWhile predicate xs
      else
        []

    [] ->
      []


runInParallel cmdA cmdB =
  Cmd.batch [cmdA, cmdB]



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
  | Error String


type Outcome
  = Right
  | Wrong
  | Missed


type alias Isi =
  Int


type alias Duration =
  Int


type alias SessionId =
  Int


type alias Key pq answer =
  List pq -> Maybe answer


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
    count =
      List.length <| takeWhile predicate sessionOutcomes
  in
    if count > 0 && rem count 4 == 0 then
      1
    else
      0


isiDirection : List Outcome -> Int
isiDirection sessionOutcomes =
  outcomeDelta ((/=) Right) sessionOutcomes - outcomeDelta ((==) Right) sessionOutcomes


setIsi : Model a b -> Model a b
setIsi model =
  { model | isi = isiDirection model.sessionOutcomes * 100 + model.isi }


setOutcome : Model pq answer -> Outcome -> Model pq answer
setOutcome model outcome =
  if model.userHasAnswered then
    model
  else
    { model
      | userHasAnswered = True
      , sessionOutcomes = outcome :: model.sessionOutcomes
    }
      |> setIsi


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
          setOutcome model
            <| if answer == correctAnswer then
                Right
               else
                Wrong


addRandomPq : Model pq answer -> Model pq answer
addRandomPq model =
  let
    ( pq, seed ) =
      randomChoice model.pqs model.seed
  in
    case pq of
      Nothing ->
        Debug.crash "Zero partial qustions specified!"

      Just pq' ->
        { model
          | sessionPqs = pq' :: model.sessionPqs
          , userHasAnswered = False
          , seed = seed
        }



--
-- Main update
--

performTask : Task.Task String b -> Action pq answer -> Cmd (Action pq answer)
performTask task action =
  Task.perform Error (\_ -> action) task



type alias EmitPq pq = pq -> Task.Task String ()



update : EmitPq pq -> ( Time.Time, Action pq answer ) -> Model pq answer -> ( Model pq answer, Cmd (Action pq answer) )
update emitPq ( actionTimestamp, action ) oldModel =
  let
    noCmd m =
      ( m, Cmd.none )

    withinSession sessionId m =
      if sessionId == oldModel.sessionId then
        m
      else
        noCmd oldModel

    withinRunning m =
      if oldModel.isRunning then
        m
      else
        noCmd oldModel

    withinWaiting m =
      if not oldModel.isRunning then
        m
      else
        noCmd oldModel

    taskEmit m =
      case List.head m.sessionPqs of
          Just pq -> emitPq pq
          Nothing -> Task.succeed ()

    taskNewPq m =
      Process.spawn (taskEmit m) `Task.andThen` \_ -> Process.sleep (toFloat m.isi * Time.millisecond)

    cmdNewPq m =
      performTask (taskNewPq m) (AnswerTimeout m.sessionId)

    cmdDelayMessage delay message =
      performTask (Process.sleep delay) message


  in
    case action of
      --
      -- Session
      --
      Start ->
        withinWaiting
          <| let
              updatedModel =
                { oldModel
                  | isRunning = True
                  , sessionId = oldModel.sessionId + 1
                  , sessionPqs = []
                  , sessionOutcomes = []
                  , seed = Random.initialSeed <| floor actionTimestamp * 1000
                }
                  |> addRandomPq

              cmd =
                runInParallel
                  (cmdDelayMessage (toFloat updatedModel.duration * Time.minute) (AutomaticStop updatedModel.sessionId))
                  (cmdNewPq updatedModel)
             in
              (updatedModel, cmd)

      ManualStop ->
        withinRunning
          <| noCmd { oldModel | isRunning = False }

      AutomaticStop sessionId ->
        withinRunning
          <| withinSession sessionId
          <| noCmd { oldModel | isRunning = False }

      --
      -- Answers
      --
      UserAnswers answerValue ->
        withinRunning
          <| noCmd
          <| setAnswer oldModel
          <| Just answerValue

      AnswerTimeout sessionId ->
        withinRunning
          <| withinSession sessionId
          <| let
              updatedModel =
                setAnswer oldModel Nothing |> addRandomPq

              task =
                cmdNewPq updatedModel
             in
              ( updatedModel, task )

      --
      -- Input fields updates
      --
      -- TODO: find a way to deduplicate these
      --
      UpdateIsi isiString ->
        withinWaiting
          <| noCmd
          <| case String.toInt isiString of
              Ok isi ->
                { oldModel | isi = isi }

              Err _ ->
                oldModel

      UpdateDuration durationString ->
        withinWaiting
          <| noCmd
          <| case String.toInt durationString of
              Ok duration ->
                { oldModel | duration = duration }

              Err _ ->
                oldModel

      -- TODO: show the error in the page
      Error message ->
        let
            e = Debug.log "error" message
        in
           noCmd oldModel
