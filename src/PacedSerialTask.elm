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


--
-- Model
--


type Message answer
  = InitRandomSeed Time.Time

  | UpdateIsi String
  | UpdateDuration String

  | AnswerTimeout SessionId
  | UserAnswers answer
  | Start
  | ManualStop
  | AutomaticStop SessionId


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


model0 : Key pq answer -> List pq -> Isi -> Duration -> Model pq answer
model0 key pqs isi duration =
  Model key pqs True False [] isi 0 duration (Random.initialSeed 0) []

cmd0 =
  Task.perform (\_ -> ManualStop) InitRandomSeed Time.now


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
        -- TODO: fail at model0 rather than here
        Debug.crash "Zero partial questions specified!"

      Just pq' ->
        { model
          | sessionPqs = pq' :: model.sessionPqs
          , userHasAnswered = False
          , seed = seed
        }



--
-- Main update
--
update : Message answer -> Model pq answer -> ( Model pq answer, Cmd (Message answer), Maybe pq )
update message oldModel =
  let
    modelOnly m = ( m, Cmd.none, Nothing )

    withinSession sessionId newState = if sessionId == oldModel.sessionId then newState else modelOnly oldModel
    withinRunning newState = if oldModel.isRunning then newState else modelOnly oldModel
    withinWaiting newState = if not oldModel.isRunning then newState else modelOnly oldModel

    cmdDelayMessage delay message =
      Task.perform (\_ -> ManualStop) (\_ -> message) (Process.sleep delay)

    newPq m =
      ( List.head m.sessionPqs, cmdDelayMessage (toFloat m.isi * Time.millisecond) (AnswerTimeout m.sessionId) )

  in
    case message of

      InitRandomSeed time ->
        -- `time` values within a minute to each other seem to produce the same initial random digit.
        -- Times further away seem to be more genuinely random. Since the latter is the app use case,
        -- no correction seems necessary.
        modelOnly { oldModel | seed = Random.initialSeed <| floor time }


      --
      -- Session
      --
      Start ->
        withinWaiting
          <| let
              newModel =
                { oldModel
                  | isRunning = True
                  , sessionId = oldModel.sessionId + 1
                  , sessionPqs = []
                  , sessionOutcomes = []
                }
                  |> addRandomPq

              ( maybePq, pqCmd ) = newPq newModel
              stopCmd = cmdDelayMessage (toFloat newModel.duration * Time.minute) (AutomaticStop newModel.sessionId)
             in
              ( newModel, Cmd.batch [pqCmd, stopCmd], maybePq )

      ManualStop ->
        withinRunning
          <| modelOnly { oldModel | isRunning = False }

      AutomaticStop sessionId ->
        withinRunning
          <| withinSession sessionId
          <| modelOnly { oldModel | isRunning = False }


      --
      -- Answers
      --
      UserAnswers answerValue ->
        withinRunning
          <| modelOnly
          <| setAnswer oldModel
          <| Just answerValue

      AnswerTimeout sessionId ->
        withinRunning
          <| withinSession sessionId
          <| let
              newModel = setAnswer oldModel Nothing |> addRandomPq
              ( maybePq, cmd ) = newPq newModel
             in
              ( newModel, cmd, maybePq )

      --
      -- Input fields updates
      --
      -- TODO: find a way to deduplicate these
      --
      UpdateIsi isiString ->
        withinWaiting
          <| modelOnly
          <| case String.toInt isiString of
              Ok isi ->
                { oldModel | isi = isi }

              Err _ ->
                oldModel

      UpdateDuration durationString ->
        withinWaiting
          <| modelOnly
          <| case String.toInt durationString of
              Ok duration ->
                { oldModel | duration = duration }

              Err _ ->
                oldModel

