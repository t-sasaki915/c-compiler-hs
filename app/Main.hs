module Main (main) where

import           Control.Exception  (SomeException, try)
import           Data.Maybe         (listToMaybe)
import           System.Environment (getArgs, getProgName)
import           System.Exit        (exitFailure)

import           LexicalAnalyser
import           SyntaxAnalyser

main :: IO ()
main = do
  args <- getArgs
  progName <- getProgName
  case listToMaybe args of
    Just sourceFileName -> do
      sourceOrErr <- try (readFile sourceFileName) :: IO (Either SomeException String)
      case sourceOrErr of
        Right sourceCode -> do
          case lexicalAnalyse sourceCode of
            Right tokens -> do
              print tokens
              case syntaxAnalyse tokens of
                Right syntaxTree ->
                  print syntaxTree

                Left syntaxAnalyseErr -> do
                  putStrLn "Syntax Analysation failed."
                  putStrLn $ "Because of: " ++ show syntaxAnalyseErr
                  exitFailure

            Left lexicalAnalyseErr -> do
              putStrLn "Lexical Analysation failed."
              putStrLn $ "Because of: " ++ show lexicalAnalyseErr
              exitFailure

        Left readErr -> do
          print readErr
          exitFailure

    Nothing -> do
      putStrLn $ "Usage: " ++ progName ++ " [Source File]"
      exitFailure
