module Main where

import DateHandling

import Data.Time
import qualified Data.List
import Data.List.Split
import qualified Data.Map as Map
import System.IO
import System.Directory

import Data.Time.Format (formatTime, parseTime)
import Data.Time.Calendar.WeekDate (toWeekDate)
import Data.Maybe
import Control.Monad

import qualified Data.ByteString.Char8 as BC

import Data.Char (isSpace)

import DateHandling
import Types

data CurrentTask = NoTask
                 | ATask StartedTask
type TaskList = [CompletedTask]
data AppState = AppState CurrentTask TaskList

data Command = CommandStart UTCTime Issue String
             | CommandRename UTCTime Issue String
             | CommandStop UTCTime
             | CommandAgain UTCTime
             | CommandAbandon UTCTime
             | CommandReopen UTCTime
             | CommandExtend UTCTime
             | CommandCurrent UTCTime
             | CommandLast UTCTime
             | CommandToday UTCTime
             | CommandYesterday UTCTime
             | CommandWorked UTCTime
             | CommandWorkedThisWeek UTCTime
             | CommandSummarize UTCTime Issue
             | CommandSummarizeWeek UTCTime Int
             | CommandShowWeek UTCTime
             | NoCommand
             | UnrecognizedCommand

data CommandOutput = CommandOutput AppState String

ltrim = dropWhile isSpace

taskDuration :: CompletedTask -> NominalDiffTime
taskDuration (CompletedTask (StartedTask startTime _ _) endTime) = endTime `diffUTCTime` startTime

tasksDuration = (foldr (+) 0) . (map taskDuration)

realToDecimalPlaces decimals real = (fromInteger $ round $ real * (10^decimals)) / (10.0^^decimals)

durationToHoursHuman = realToDecimalPlaces 2 . ((1 / 3600) *)

-- Task

taskForDay :: Day -> CompletedTask -> Bool
taskForDay day (CompletedTask (StartedTask (UTCTime utcDay _ ) _ _) _) = utcDay == day

taskForIssue :: Issue -> CompletedTask -> Bool
taskForIssue i (CompletedTask (StartedTask _ issue _) _) = issue == i

-- Task list

tasksForDay :: Day -> TaskList -> [CompletedTask]
tasksForDay day taskList = filter (taskForDay day) taskList

tasksForIssue :: Issue -> TaskList -> [CompletedTask]
tasksForIssue issue taskList = filter (taskForIssue issue) taskList

lastCompletedTask :: TaskList -> CompletedTask
lastCompletedTask taskList = last taskList

listWithoutLastElement xs = take (length xs - 1) xs

sortAndGroup assocs = Map.fromListWith (++) [(k, [v]) | (k, v) <- assocs]

-- Last N elelements of a list
lastN' :: Int -> [a] -> [a]
lastN' n x = reverse $ take n $ reverse x

-- Command handling

issueAndDescription :: StartedTask -> String
issueAndDescription startedTask = "(" ++ (humanIssue $ issue startedTask) ++ ") " ++ (description startedTask)

humanIssue :: Issue -> String
humanIssue (Just n) = "Issue " ++ show n
humanIssue Nothing = "No issue"

taskSummary :: CompletedTask -> String
taskSummary completedTask = "[" ++ (show $ durationToHoursHuman $ taskDuration completedTask) ++ "] " ++ (issueAndDescription $ startedTask completedTask)

taskSummaries = map taskSummary

taskSummariesForOutput = (Data.List.intercalate "\n")

-- TODO: Handle at a higher level.
invalidStateChange :: AppState -> CommandOutput
invalidStateChange appState = CommandOutput appState "invalid state change"

cmdStart :: AppState -> UTCTime -> Issue -> String -> CommandOutput
cmdStart (AppState NoTask taskList) time issue description = CommandOutput (AppState (ATask (StartedTask time issue description)) taskList) ""
cmdStart (AppState (ATask startedTask) taskList) time issue description = CommandOutput (AppState (ATask (StartedTask time issue description)) (taskList ++ [(CompletedTask startedTask time)])) ""

cmdRename :: AppState -> UTCTime -> Issue -> String -> CommandOutput
cmdRename (AppState (ATask (StartedTask startTime _ _)) taskList) _ issue description = CommandOutput (AppState (ATask (StartedTask startTime issue description)) taskList) ""
cmdRename appState _ _ _ = invalidStateChange appState

cmdStop :: AppState -> UTCTime -> CommandOutput
cmdStop (AppState (ATask startedTask) taskList) time = CommandOutput (AppState NoTask (taskList ++ [(CompletedTask startedTask time)])) ""
cmdStop appState _ = invalidStateChange appState

cmdAgain :: AppState -> UTCTime -> CommandOutput
cmdAgain (AppState NoTask taskList) time =
  let (CompletedTask (StartedTask _ issue description) _) = lastCompletedTask taskList
  in CommandOutput (AppState (ATask (StartedTask time issue description)) taskList) ""
cmdAgain appState _ = invalidStateChange appState

cmdAbandon :: AppState -> UTCTime -> CommandOutput
cmdAbandon (AppState (ATask _) taskList) _ = CommandOutput (AppState NoTask taskList) ""
cmdAbandon appState _ = invalidStateChange appState

-- Reopens last task to continue work.
cmdReopen :: AppState -> UTCTime -> CommandOutput
cmdReopen (AppState NoTask taskList) time =
  let (CompletedTask startedTask _) = lastCompletedTask taskList
  in CommandOutput (AppState (ATask startedTask) (listWithoutLastElement taskList)) ""
cmdReopen appState _ = invalidStateChange appState

-- Extends last task to end now.
cmdExtend :: AppState -> UTCTime -> CommandOutput
cmdExtend (AppState NoTask taskList) time =
  let (CompletedTask startedTask _) = lastCompletedTask taskList
  in CommandOutput (AppState NoTask (listWithoutLastElement taskList ++ [CompletedTask startedTask time])) ""
cmdExtend appState _ = invalidStateChange appState

cmdCurrent :: AppState -> UTCTime -> CommandOutput
cmdCurrent appState@(AppState (ATask startedTask) _) _ = CommandOutput appState (show startedTask)
cmdCurrent appState@(AppState NoTask _) _ = CommandOutput appState "No task"

cmdLast :: AppState -> UTCTime -> CommandOutput
cmdLast appState@(AppState _ taskList) _ = CommandOutput appState (Data.List.intercalate "\n" $ taskSummaries $ lastN' 10 taskList)

cmdToday :: AppState -> UTCTime -> CommandOutput
cmdToday appState@(AppState _ taskList) (UTCTime day _) = CommandOutput appState (taskSummariesForOutput $ taskSummaries (tasksForDay day taskList))

cmdYesterday :: AppState -> UTCTime -> CommandOutput
cmdYesterday appState@(AppState _ taskList) (UTCTime day _) = CommandOutput appState (taskSummariesForOutput $ taskSummaries (tasksForDay (addDays (-1) day) taskList))

cmdWorked :: AppState -> UTCTime -> CommandOutput
cmdWorked appState@(AppState _ taskList) (UTCTime day _) = CommandOutput appState (show $ durationToHoursHuman $ tasksDuration (tasksForDay day taskList))

cmdWorkedThisWeek :: AppState -> UTCTime -> CommandOutput
cmdWorkedThisWeek appState@(AppState _ taskList) time = cmdSummarizeWeek appState time (weekFromDay $ utctDay time)

cmdSummarize :: AppState -> UTCTime -> Issue -> CommandOutput
cmdSummarize appState _ Nothing = CommandOutput appState "no issue specified"
cmdSummarize appState@(AppState _ taskList) _ issue = CommandOutput appState (show $ durationToHoursHuman $ tasksDuration (tasksForIssue issue taskList))

cmdSummarizeWeek :: AppState -> UTCTime -> Int -> CommandOutput
cmdSummarizeWeek appState@(AppState _ taskList) time weekNumber = CommandOutput appState (show $ durationToHoursHuman $ tasksDuration (tasksForWeek weekNumber $ tasksForYear currentYear taskList))
                                                                  where
                                                                    currentYear = yearFromDay $ utctDay time

cmdShowWeek :: AppState -> UTCTime -> CommandOutput
cmdShowWeek appState time = CommandOutput appState (show $ weekFromDay $ utctDay time)

-- TODO: Tidy

tasksForYear year = filter (\x -> (yearFromDay (utctDay $ endDateForTask x)) == year)

tasksForWeek weekNumber = filter (\x -> (weekFromDay (utctDay $ endDateForTask x)) == weekNumber)

endDateForTask (CompletedTask _ endDate) = endDate

yearFromDay day =
  let (year, week, weekDay) = toWeekDate day
  in year

weekFromDay day =
  let (year, week, weekDay) = toWeekDate day
  in week

-- TODO: End

processCommand :: Command -> AppState -> CommandOutput
processCommand command a = case command of
  (CommandStart time issue description) -> cmdStart a time issue description
  (CommandRename time issue description) -> cmdRename a time issue description
  (CommandStop time) -> cmdStop a time
  (CommandAgain time) -> cmdAgain a time
  (CommandAbandon time) -> cmdAbandon a time
  (CommandReopen time) -> cmdReopen a time
  (CommandExtend time) -> cmdExtend a time
  (CommandCurrent time) -> cmdCurrent a time
  (CommandLast time) -> cmdLast a time
  (CommandToday time) -> cmdToday a time
  (CommandYesterday time) -> cmdYesterday a time
  (CommandWorked time) -> cmdWorked a time
  (CommandWorkedThisWeek time) -> cmdWorkedThisWeek a time
  (CommandSummarize time issue) -> cmdSummarize a time issue
  (CommandSummarizeWeek time weekNumber) -> cmdSummarizeWeek a time weekNumber
  (CommandShowWeek time) -> cmdShowWeek a time
  NoCommand -> CommandOutput a ""
  UnrecognizedCommand -> CommandOutput a "not recognised"

-- Commands from string input

getCommand :: UTCTime -> [String] -> Command
getCommand _ [] = NoCommand
getCommand time [cmd] = getCommandWithoutArgs time cmd
getCommand time (x:xs) = getCommandWithArgs time x xs

getCommandWithoutArgs :: UTCTime -> String -> Command
getCommandWithoutArgs time cmd = case cmd of
  "stop"             -> CommandStop time
  "today"            -> CommandToday time
  "again"            -> CommandAgain time
  "abandon"          -> CommandAbandon time
  "reopen"           -> CommandReopen time
  "extend"           -> CommandExtend time
  "current"          -> CommandCurrent time
  "last"             -> CommandLast time
  "yesterday"        -> CommandYesterday time
  "worked"           -> CommandWorked time
  "worked-this-week" -> CommandWorkedThisWeek time
  "week-number"      -> CommandShowWeek time
  _                  -> UnrecognizedCommand

issueFromStringArgs :: String -> (Issue, String)
issueFromStringArgs x = case (reads x :: [(Int, String)]) of
                         []       -> (Nothing, x)
                         [(y, x)] -> (Just y, ltrim x)

issueFromString x = case (issueFromStringArgs x) of
                      (issue, _) -> issue

getCommandWithArgs :: UTCTime -> String -> [String] -> Command
getCommandWithArgs time command args = case command of
  "start" ->
    let (issue, description) = issueFromStringArgs $ unwords args
    in CommandStart time issue description
  "rename" ->
    let (issue, description) = issueFromStringArgs $ unwords args
    in CommandRename time issue description
  "summarize" ->
    let (issue, description) = issueFromStringArgs $ unwords args
    in CommandSummarize time issue
  "worked-week" ->
    let weekNumber = case (reads (unwords args) :: [(Int, String)]) of
                   [] -> 1
                   [(y, x)] -> y
    in CommandSummarizeWeek time weekNumber
  _ -> UnrecognizedCommand

-- Export

issueToString :: Issue -> String
issueToString Nothing = ""

issueToString (Just x) = show x

-- TODO: Change to instance
taskToString :: CompletedTask -> String
taskToString (CompletedTask (StartedTask startTime issue description) endTime) = iso8601 startTime ++ "|" ++ iso8601 endTime ++ "|" ++ issueToString issue ++ "|" ++ description

taskListFromAppState :: AppState -> TaskList
taskListFromAppState (AppState _ taskList) = taskList

taskListToString :: TaskList -> String
taskListToString taskList = Data.List.intercalate "\n" $ map taskToString taskList

-- Import

taskFromString :: String -> Maybe CompletedTask
taskFromString "" = Nothing
taskFromString s = taskFromParts $ taskPartsFromString $ splitOn "|" s

taskPartsFromString :: [String] -> (Maybe UTCTime, Maybe UTCTime, String, String)
taskPartsFromString [startTime, endTime, issueNumber, description] = ((fromISO8601 startTime), (fromISO8601 endTime), issueNumber, description)

taskFromParts :: (Maybe UTCTime, Maybe UTCTime, String, String) -> Maybe CompletedTask
taskFromParts (Just startTime, Just endTime, issueNumber, description) = Just (CompletedTask (StartedTask startTime (issueFromString issueNumber) description) endTime)

commandOutputWithNewLine :: String -> String
commandOutputWithNewLine "" = ""
commandOutputWithNewLine string = string ++ "\n"

commandLoop :: AppState -> IO Bool
commandLoop state = do
  let (AppState currentTask _) = state
  putStr $ (getPrompt currentTask) ++ " "
  hFlush stdout
  line <- getLine
  let tokens = words line
  currentTime <- getCurrentTime
  let (CommandOutput newState output) = processCommand (getCommand currentTime tokens) state
  putStr $ commandOutputWithNewLine output

  let previousTaskList = taskListFromAppState state
  let newTaskList = taskListFromAppState newState
  when (newTaskList /= previousTaskList) $ writeTaskFile $ taskListToString newTaskList
  commandLoop newState

getPrompt :: CurrentTask -> String
getPrompt ct = case ct of
                 NoTask -> ">"
                 ATask t -> issueAndDescription t ++ ">"

loadTaskFile = do
  byteContents <- BC.readFile "tasks.db"
  return $ BC.unpack byteContents

loadCompletedTasks = do
  contents <- loadTaskFile
  let lines = splitOn "\n" contents
  let completedTasks = map taskFromString lines
  let validCompletedTasks = map fromJust (filter isJust completedTasks)
  return validCompletedTasks

writeTaskFile :: String -> IO ()
writeTaskFile taskData = do
  (tempName, tempHandle) <- openTempFile "." "temp"
  hPutStr tempHandle taskData
  hClose tempHandle
  removeFile "tasks.db"
  renameFile tempName "tasks.db"

main = do
  completedTasksFromFile <- loadCompletedTasks
  putStrLn $ "Loaded " ++ (show $ length completedTasksFromFile) ++ " tasks"
  commandLoop $ AppState NoTask completedTasksFromFile
