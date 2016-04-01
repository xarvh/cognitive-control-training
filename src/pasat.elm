module Pasat where


import Date
import Date.Format
import Signal
import String
import Time
import Task

import PacedSerialTask


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
    | DownloadLog
    | DownloadAggregateData


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




log2csv log =
    let
        map (timestamp, action) = String.join "," [toString timestamp, toString action] ++ "\n"
    in
       String.concat <| "Epoc,Action\n" :: List.map map (List.reverse log)

log2aggregate log = ""







--
-- UPDATE
--
type alias TaskFactories =
    { playSound : String -> Task.Task () ()
    , triggerAction : Action -> Task.Task () ()
    , download : (String, String, String) -> Task.Task () ()
    }

update : TaskFactories -> (Time.Time, Action) -> Model -> (Model, Task.Task () ())
update factories (timestamp, action) oldModel =
    let
        noTask m = (m, Task.succeed ())

        downloadFilename name = Date.Format.format "pasat_%Y%m%d_%H%M_" (Date.fromTime timestamp) ++ name ++ ".csv"
        downloadCsv name transform = (oldModel, factories.download (downloadFilename name, "text/csv", transform oldModel.log))

    in case action of
        SelectVoice voice ->
            noTask oldModel

        SoundLoaded loadProgress ->
            noTask { oldModel | loadProgress = loadProgress }

        DownloadLog ->
            downloadCsv "log" log2csv

        DownloadAggregateData ->
            downloadCsv "aggregate" log2aggregate

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
