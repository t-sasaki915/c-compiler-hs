module Main (main) where

import           Control.Exception  (SomeException, try)
import           Data.Maybe         (listToMaybe)
import           System.Environment (getArgs, getProgName)

main :: IO ()
main = do
  args <- getArgs
  progName <- getProgName
  case listToMaybe args of
    Just sourceFileName -> do
      sourceOrErr <- try (readFile sourceFileName) :: IO (Either SomeException String)
      case sourceOrErr of
        Right sourceCode ->
          putStrLn sourceCode

        Left readErr ->
          putStrLn $ show readErr

    Nothing ->
      putStrLn $ "Usage: " ++ progName ++ " [Source File]"
