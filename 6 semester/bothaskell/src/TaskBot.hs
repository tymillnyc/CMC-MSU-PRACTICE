module TaskBot
  ( runBot
  ) where

import Consts
import System.IO

type Task = String
type UserName = String
type BotName = String

-- | Run task bot.
-- Commands:
-- /list -- show task list
-- /complete -- complete the last task
-- /exit -- stop bot
-- Any other input is considered as new task.
runBot :: IO ()
runBot = do
  -- disable buffering for stdout
  hSetBuffering stdout NoBuffering
  putStrLn namePrompt
  name <- getLine
  botName <- getLine
  go name botName []
  where
    -- Helper function to interact with user and update tasks list
    go :: UserName -> BotName -> [Task] -> IO ()
    go name botName taskList = do
      putStr $ name ++ "> "
      str <- getLine
      if (str == "/exit")
        then putStrLn goodbyeMsg
        else do
          -- process input unless it is an "/exit" command
          let (output, newTaskList) = processCommand str taskList
          putStrLn (botName ++ "> " ++ output)
          go name botName newTaskList

-- | Process user input. Returns output string to be printed by bot and
-- updated list of tasks in a tuple.
processCommand :: String -> [Task] -> (String, [Task])
processCommand cmd prevTaskList = case cmd of
  "/delete" -> (deleteMsg, [])
  "/list" -> cmdList prevTaskList
  "/complete" -> cmdComplete prevTaskList
  _ -> addTask cmd prevTaskList

-- | Command to show tasks list.
cmdList :: [Task] -> (String, [Task])
cmdList tasks = (concatStrings tasks (length tasks), tasks)

concatStrings :: [Task] -> Int -> String
concatStrings [] _ = []
concatStrings (el:list) count = concatStrings list (count-1) ++ (show(count) ++ ". " ++ el ++ "\n") 
-- | Command to complete the last task.
cmdComplete :: [Task] -> (String, [Task])
cmdComplete [] = (noTasksMsg, [])
cmdComplete (_:xs) = (completeMsg, xs)

-- | Add new task to tasks list.
addTask :: String -> [Task] -> (String, [Task])
addTask task l = (newTaskMsg, task:l)
