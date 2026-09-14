{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Logger(logInfo, logError, logWarning, withLogging) where

import Data.Time (getCurrentTime, formatTime, defaultTimeLocale)
import System.IO (appendFile, hFlush, stdout)
import Control.Exception (catch, SomeException)

logFile :: FilePath
logFile = "app.log"

-- форматирование текущего времени
currentTimestamp :: IO String
currentTimestamp = do
    now <- getCurrentTime
    pure $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" now

writeLog :: String -> String -> IO ()
writeLog level message = do
    ts <- currentTimestamp
    let line = "[" ++ ts ++ "] [" ++ level ++ "] " ++ message ++ "\n"
    appendFile logFile line
        `catch` \(e :: SomeException) ->
            putStrLn $ "Не удалось записать в лог: " ++ show e

-- информационное сообщение
logInfo :: String -> IO ()
logInfo msg = writeLog "INFO" msg

logWarning :: String -> IO ()
logWarning msg = writeLog "WARN" msg
logError :: String -> IO ()
logError msg = writeLog "ERROR" msg

-- логируем начало и конец действи
withLogging :: String -> IO a -> IO a
withLogging actionName action = do
    logInfo $ "Начало: " ++ actionName
    result <- action
    logInfo $ "Завершено: " ++ actionName
    pure result