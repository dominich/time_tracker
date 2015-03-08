import Data.Time
import qualified Data.List
import Data.List.Split
import qualified Data.Map as Map
import System.IO
import System.Directory  

import System.Locale (defaultTimeLocale)
import Data.Time.Format (formatTime, parseTime)
import Data.Maybe

import qualified Data.ByteString
import qualified Data.ByteString.Char8

import Data.Char (isSpace)

import DateHandling

data Issue = Issue Int | NoIssue
  deriving (Eq, Show)
data StartedTask = StartedTask UTCTime Issue String
  deriving Show
data CompletedTask = CompletedTask StartedTask UTCTime
  deriving Show
data CurrentTask = NoTask
                 | ATask StartedTask
type TaskList = [CompletedTask]
data AppState = AppState CurrentTask TaskList

data Command = CommandStart UTCTime Issue String
             | CommandRename UTCTime Issue String
             | CommandStop UTCTime
             | CommandAgain UTCTime
	     | CommandAbandon UTCTime
	     | CommandCurrent UTCTime
	     | CommandLast UTCTime
	     | CommandToday UTCTime
	     | CommandYesterday UTCTime
	     | CommandWorked UTCTime
             | CommandSummarize UTCTime Issue
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

sortAndGroup assocs = Map.fromListWith (++) [(k, [v]) | (k, v) <- assocs]

-- Last N elelements of a list
lastN' :: Int -> [a] -> [a]
lastN' n x = reverse $ take n $ reverse x

-- Command handling
 
taskSummary :: CompletedTask -> String
taskSummary completedTask@(CompletedTask (StartedTask _ issue description) _)= "[" ++ (show $ durationToHoursHuman $ taskDuration completedTask) ++ "] (" ++ show issue ++ ") " ++ description

taskSummaries = map taskSummary

taskSummariesForOutput = (Data.List.intercalate "\n") . Data.List.sort . Data.List.nub

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

dayTaskPair task@(CompletedTask (StartedTask (UTCTime utcDay _) _ _) _) = (utcDay ,task) 

cmdSummarize :: AppState -> UTCTime -> Issue -> CommandOutput
cmdSummarize appState _ NoIssue = CommandOutput appState "no issue specified"
cmdSummarize appState@(AppState _ taskList) _ issue = CommandOutput appState (show $ sortAndGroup $ map dayTaskPair (tasksForIssue issue taskList))

processCommand :: Command -> AppState -> CommandOutput
processCommand (CommandStart time issue description) a = cmdStart a time issue description
processCommand (CommandRename time issue description) a = cmdRename a time issue description
processCommand (CommandStop time) a = cmdStop a time
processCommand (CommandAgain time) a = cmdAgain a time
processCommand (CommandAbandon time) a = cmdAbandon a time
processCommand (CommandCurrent time) a = cmdCurrent a time
processCommand (CommandLast time) a = cmdLast a time
processCommand (CommandToday time) a = cmdToday a time
processCommand (CommandYesterday time) a = cmdYesterday a time
processCommand (CommandWorked time) a = cmdWorked a time
processCommand (CommandSummarize time issue) a = cmdSummarize a time issue
processCommand NoCommand a = CommandOutput a ""
processCommand UnrecognizedCommand a = CommandOutput a "not recognised"

-- Commands from string input

getCommand :: UTCTime -> [String] -> Command
getCommand _ [] = NoCommand 
getCommand time [cmd] = getCommandWithoutArgs time cmd
getCommand time (x:xs) = getCommandWithArgs time x xs

getCommandWithoutArgs :: UTCTime -> String -> Command
getCommandWithoutArgs time "stop" = CommandStop time
getCommandWithoutArgs time "today" = CommandToday time
getCommandWithoutArgs time "again" = CommandAgain time
getCommandWithoutArgs time "abandon" = CommandAbandon time
getCommandWithoutArgs time "current" = CommandCurrent time
getCommandWithoutArgs time "last" = CommandLast time
getCommandWithoutArgs time "yesterday" = CommandYesterday time
getCommandWithoutArgs time "worked" = CommandWorked time
getCommandWithoutArgs _ _ = UnrecognizedCommand

issueFromStringArgs :: String -> (Issue, String)
issueFromStringArgs x = case (reads x :: [(Int, String)]) of
                         []       -> (NoIssue, x)
                         [(y, x)] -> (Issue y, ltrim x)

issueFromString x = case (issueFromStringArgs x) of
                      (issue, _) -> issue

getCommandWithArgs :: UTCTime -> String -> [String] -> Command
getCommandWithArgs time "start" args = 
  let (issue, description) = issueFromStringArgs $ unwords args
  in CommandStart time issue description
getCommandWithArgs time "rename" args =
  let (issue, description) = issueFromStringArgs $ unwords args
  in CommandRename time issue description
getCommandWithArgs time "summarize" args =
  let (issue, description) = issueFromStringArgs $ unwords args
  in CommandSummarize time issue
getCommandWithArgs _ _ _ = UnrecognizedCommand

-- Export

issueToString :: Issue -> String
issueToString NoIssue = ""

issueToString (Issue x) = show x

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
  putStr "> "
  line <- getLine
  let tokens = words line
  currentTime <- getCurrentTime
  let (CommandOutput newState output) = processCommand (getCommand currentTime tokens) state
  putStr $ commandOutputWithNewLine output
  writeTaskFile $ taskListToString $ taskListFromAppState newState 
  commandLoop newState

loadTaskFile = do
  byteContents <- Data.ByteString.readFile "tasks.db"  
  return $ Data.ByteString.Char8.unpack byteContents

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
