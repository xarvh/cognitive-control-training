module Pasat where


import Signal
import Time
import Task

import PacedSerialTask

-- import Debug exposing (log)



--
-- MODEL
--
type alias Pq = Int
type alias Answer = Int
type alias PstModel = PacedSerialTask.Model Pq Answer
type alias PstAction = PacedSerialTask.Action Pq Answer
type alias Voice = String
type alias Progress = Float

type Action
    = SelectVoice Voice
    | SoundLoaded Progress
    | NestedPstAction PstAction


sumOfLastTwoDigits : PacedSerialTask.Key Pq Answer
sumOfLastTwoDigits list = case list of
    a :: b :: _ -> Just (a + b)
    _ -> Nothing

possibleDigits = [1..9]


type alias Model =
    { pst : PstModel
    , log : List (Time.Time, PstAction)
    , voice : Voice
    , loadProgress : Progress
    }

model isi duration voice =
    Model (PacedSerialTask.model sumOfLastTwoDigits possibleDigits isi duration) [] voice 0

model0 = model 3000 5 "english/ossi"
-- task0 =
-- state0 = (model0, task0)


--
-- UPDATE
--
noTask m = (m, Task.succeed ())

type alias TaskFactories =
    { playSound : String -> Task.Task () ()
    , triggerAction : Action -> Task.Task () ()
    }

update : TaskFactories -> (Time.Time, Action) -> Model -> (Model, Task.Task () ())
update factories (timestamp, action) oldModel =
    case action of
        SelectVoice voice ->
            noTask oldModel

        SoundLoaded loadProgress ->
            noTask { oldModel | loadProgress = loadProgress }

        NestedPstAction pstAction ->
            if oldModel.loadProgress < 1
            then noTask oldModel
            else
                let
                    factories' =
                        { triggerAction = factories.triggerAction << NestedPstAction
                        , emitPq = \pq -> factories.playSound <| "pasat/" ++ oldModel.voice ++ "/" ++ toString pq
                        }

                    timestampedPstAction = (timestamp, pstAction)

                    (pstModel, task) = PacedSerialTask.update factories' timestampedPstAction oldModel.pst
                in
                    ({ oldModel | pst = pstModel, log = timestampedPstAction :: oldModel.log }, task)
