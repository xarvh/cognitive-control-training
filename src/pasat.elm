module Pasat where


import Date
import Date.Format
import Signal
import String
import Time
import Task

import PacedSerialTask


--
-- HELPERS
--
-- (I do long for Python's "batteries included")
--
groupBy : (a -> b) -> List a -> List (b, List a)
groupBy selector list =
    case list of
        [] -> []
        a :: _ ->
            let
                selected = selector a
                predicate = ((==) selected) << selector
                (inside, outside) = List.partition predicate list
            in
                (selected, inside) :: groupBy selector outside


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

type alias OutcomeLogEntry =
    { timestamp : Time.Time
    , sessionId : Int
    , isi : PacedSerialTask.Isi
    , outcome : Maybe PacedSerialTask.Outcome
    }

type alias Model =
    { pst : PstModel
    , actionsLog : List (Time.Time, PstAction)
    , outcomesLog : List OutcomeLogEntry
    , voice : Voice
    , loadProgress : Progress
    }


--
-- Model Init
--
sumOfLastTwoDigits : PacedSerialTask.Key Pq Answer
sumOfLastTwoDigits list = case list of
    a :: b :: _ -> Just (a + b)
    _ -> Nothing

possibleDigits = [1..9]

model isi duration voice =
    Model (PacedSerialTask.model sumOfLastTwoDigits possibleDigits isi duration) [] [] voice 0


model0 = model 1000 5 "english/ossi"
{-
state0 factories =
    ( model 3000 5 "english/ossi"
    , factories.loadSounds "english/ossi" stuff
    )
-}


table2Csv : List (List String) -> String
table2Csv table =
    String.concat <| List.map (\row -> "\"" ++ String.join "\",\"" row ++ "\"\n") table


log2csv : List OutcomeLogEntry -> String
log2csv log =
    let
        map entry =
            [ toString entry.timestamp
            , toString entry.sessionId
            , toString entry.isi
            , case entry.outcome of
                Nothing -> ""
                Just o -> toString o
            ]
    in
       table2Csv <| ["Epoc", "sessionId", "Isi", "Outcome"] :: List.map map log


log2aggregateCsv : List OutcomeLogEntry -> String
log2aggregateCsv log =
    let
        aggregateSessionModels (sessionId, entries) =
            let
                dummyEntry = OutcomeLogEntry 0 0 0 Nothing
                first = Maybe.withDefault dummyEntry <| List.head entries
                last = Maybe.withDefault dummyEntry <| List.head <| List.reverse entries

                isies = List.map .isi <| List.filter ((/=) Nothing << .outcome) entries
                minIsi = Maybe.withDefault 0 <| List.minimum isies
                maxIsi = Maybe.withDefault 0 <| List.maximum isies

                count o = List.length <| List.filter ((==) (Just o) << .outcome) entries
                right = count PacedSerialTask.Right
                wrong = count PacedSerialTask.Wrong
                missed = count PacedSerialTask.Missed

                formatTimestamp t = Date.Format.format "%Y-%m-%d %H:%M:%S" <| Date.fromTime t
            in
               [ toString sessionId
               , formatTimestamp first.timestamp
               , formatTimestamp last.timestamp

               , toString <| right
               , toString <| wrong
               , toString <| missed
               , toString <| toFloat right / toFloat (right + wrong + missed)

               , toString <| first.isi
               , toString <| maxIsi
               , toString <| minIsi
               , toString <| List.length <| List.filter ((==) (Debug.log "min" minIsi)) (Debug.log "li" isies)
               ]
    in
        table2Csv <| [
            "Session id",
            "Session start",
            "Session end",

            "Right",
            "Wrong",
            "Miss",

            "Accuracy (normalized)",

            "Starting ISI",
            "Max ISI",
            "Min ISI",
            "Trials at minimum ISI"
        ] :: (List.map aggregateSessionModels <| groupBy .sessionId log)


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
        downloadCsv name transform = (oldModel, factories.download (downloadFilename name, "text/csv", transform oldModel.outcomesLog))

    in case action of
        SelectVoice voice ->
            noTask oldModel

        SoundLoaded loadProgress ->
            noTask { oldModel | loadProgress = loadProgress }

        DownloadLog ->
            downloadCsv "log" log2csv

        DownloadAggregateData ->
            downloadCsv "aggregate" log2aggregateCsv

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

                    entries : Maybe PacedSerialTask.Outcome -> List OutcomeLogEntry
                    entries o = [OutcomeLogEntry timestamp pstModel.sessionId oldModel.pst.isi o]
                    append =
                        if List.length pstModel.sessionOutcomes > List.length oldModel.pst.sessionOutcomes then entries <| List.head pstModel.sessionOutcomes
                        else if pstModel.isRunning /= oldModel.pst.isRunning then entries Nothing
                        else []
                in
                    ({ oldModel
                        | pst = pstModel
                        , actionsLog = timestampedPstAction :: oldModel.actionsLog
                        , outcomesLog = List.append oldModel.outcomesLog append
                    }
                    , task)
