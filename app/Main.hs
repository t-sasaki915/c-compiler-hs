module Main (main) where

import           Control.Exception  (SomeException, try)
import           Data.Maybe         (listToMaybe)
import           System.Environment (getArgs, getProgName)
import           System.Exit        (exitFailure)

import           LexicalAnalyser

main :: IO ()
main = do
  args <- getArgs
  progName <- getProgName
  case listToMaybe args of
    Just sourceFileName -> do
      sourceOrErr <- try (readFile sourceFileName) :: IO (Either SomeException String)
      case sourceOrErr of
        Right sourceCode -> do
          let lexicalAnalysed = lexicalAnalyse sourceCode
          print lexicalAnalysed

        Left readErr -> do
          print readErr
          exitFailure

    Nothing -> do
      putStrLn $ "Usage: " ++ progName ++ " [Source File]"
      exitFailure
