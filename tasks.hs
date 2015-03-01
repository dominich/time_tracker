import Data.Time
import qualified Data.List
import Data.List.Split
import System.IO
import System.Directory  

import System.Locale (defaultTimeLocale)
import Data.Time.Format (formatTime, parseTime)
import Data.Maybe

import qualified Data.ByteString
import qualified Data.ByteString.Char8

import DateHandling

data StartedTask = StartedTask UTCTime String
  deriving Show
data CompletedTask = CompletedTask StartedTask UTCTime
  deriving Show
data CurrentTask = NoTask
                 | ATask StartedTask
type TaskList = [CompletedTask]
data AppState = AppState CurrentTask TaskList

data Command = CommandStart UTCTime String
             | CommandRename UTCTime String
             | CommandStop UTCTime
             | CommandAgain UTCTime
	     | CommandAbandon UTCTime
	     | CommandCurrent UTCTime
	     | CommandLast UTCTime
	     | CommandToday UTCTime
	     | CommandYesterday UTCTime
	     | CommandWorked UTCTime
	     | NoCommand
             | UnrecognizedCommand

data CommandOutput = CommandOutput AppState String

taskDuration :: CompletedTask -> NominalDiffTime
taskDuration (CompletedTask (StartedTask startTime _) endTime) = endTime `diffUTCTime` startTime

tasksDuration = (foldr (+) 0) . (map taskDuration)
durationHours = ((1 / 3600) *)

-- Task

taskForDay :: Day -> CompletedTask -> Bool
taskForDay day (CompletedTask (StartedTask (UTCTime utcDay _ ) _) _) = utcDay == day

-- Task list

tasksForDay :: Day -> TaskList -> [CompletedTask]
tasksForDay day taskList = filter (taskForDay day) taskList

lastCompletedTask :: TaskList -> CompletedTask
lastCompletedTask taskList = last taskList

-- Last N elelements of a list
lastN' :: Int -> [a] -> [a]
lastN' n x = reverse $ take n $ reverse x

-- Command handling
 
taskDescriptions :: TaskList -> [String]
taskDescriptions taskList = [ d | (CompletedTask (StartedTask s d) e) <- taskList]

taskDescriptionsForOutput = (Data.List.intercalate "\n") . Data.List.sort . Data.List.nub

cmdStart :: AppState -> UTCTime -> String -> CommandOutput
cmdStart (AppState NoTask taskList) time description = CommandOutput (AppState (ATask (StartedTask time description)) taskList) ""

cmdRename :: AppState -> UTCTime -> String -> CommandOutput
cmdRename (AppState (ATask (StartedTask startTime _)) taskList) time description = CommandOutput (AppState (ATask (StartedTask startTime description)) taskList) ""

cmdStop :: AppState -> UTCTime -> CommandOutput
cmdStop (AppState (ATask startedTask) taskList) time = CommandOutput (AppState NoTask (taskList ++ [(CompletedTask startedTask time)])) ""

cmdAgain :: AppState -> UTCTime -> CommandOutput
cmdAgain (AppState NoTask taskList) time = CommandOutput (AppState (ATask (StartedTask time (descriptionFromCompletedTask $ lastCompletedTask taskList))) taskList) ""

descriptionFromCompletedTask :: CompletedTask -> String
descriptionFromCompletedTask (CompletedTask (StartedTask _ description) _) = description

cmdAbandon :: AppState -> UTCTime -> CommandOutput
cmdAbandon (AppState (ATask _) taskList) time = CommandOutput (AppState NoTask taskList) ""

cmdCurrent :: AppState -> UTCTime -> CommandOutput 
cmdCurrent appState@(AppState (ATask startedTask) _) time = CommandOutput appState (show startedTask)

cmdLast :: AppState -> UTCTime -> CommandOutput 
cmdLast appState@(AppState _ taskList) _ = CommandOutput appState (Data.List.intercalate "\n" $ taskDescriptions $ lastN' 10 taskList)

cmdToday :: AppState -> UTCTime -> CommandOutput
cmdToday appState@(AppState _ taskList) (UTCTime day _) = CommandOutput appState (taskDescriptionsForOutput $ taskDescriptions (tasksForDay day taskList))

cmdYesterday :: AppState -> UTCTime -> CommandOutput
cmdYesterday appState@(AppState _ taskList) (UTCTime day _) = CommandOutput appState (taskDescriptionsForOutput $ taskDescriptions (tasksForDay (addDays (-1) day) taskList))

cmdWorked :: AppState -> UTCTime -> CommandOutput
cmdWorked appState@(AppState _ taskList) (UTCTime day _) = CommandOutput appState (show $ durationHours $ tasksDuration (tasksForDay day taskList))

processCommand :: Command -> AppState -> CommandOutput
processCommand (CommandStart time description) a = cmdStart a time description
processCommand (CommandRename time description) a = cmdRename a time description
processCommand (CommandStop time) a = cmdStop a time
processCommand (CommandAgain time) a = cmdAgain a time
processCommand (CommandAbandon time) a = cmdAbandon a time
processCommand (CommandCurrent time) a = cmdCurrent a time
processCommand (CommandLast time) a = cmdLast a time
processCommand (CommandToday time) a = cmdToday a time
processCommand (CommandYesterday time) a = cmdYesterday a time
processCommand (CommandWorked time) a = cmdWorked a time
processCommand NoCommand a = CommandOutput a ""
processCommand UnrecognizedCommand a = CommandOutput a ""

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

getCommandWithArgs :: UTCTime -> String -> [String] -> Command
getCommandWithArgs time "start" args = CommandStart time (unwords args)
getCommandWithArgs time "rename" args = CommandRename time (unwords args)

-- Export

-- TODO: Change to instance
taskToString :: CompletedTask -> String
taskToString (CompletedTask (StartedTask startTime description) endTime) = iso8601 startTime ++ "|" ++ iso8601 endTime ++ "|" ++ description

taskListFromAppState :: AppState -> TaskList
taskListFromAppState (AppState _ taskList) = taskList

taskListToString :: TaskList -> String
taskListToString taskList = Data.List.intercalate "\n" $ map taskToString taskList

-- Import

taskFromString :: String -> Maybe CompletedTask
taskFromString "" = Nothing
taskFromString s = taskFromParts $ taskPartsFromString $ splitOn "|" s

taskPartsFromString :: [String] -> (Maybe UTCTime, Maybe UTCTime, String)
taskPartsFromString [startTime, endTime, description] = ((fromISO8601 startTime), (fromISO8601 endTime), description)

taskFromParts :: (Maybe UTCTime, Maybe UTCTime, String) -> Maybe CompletedTask
taskFromParts (Just startTime, Just endTime, description) = Just (CompletedTask (StartedTask startTime description) endTime)

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
