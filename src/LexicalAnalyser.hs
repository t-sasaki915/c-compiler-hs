{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RankNTypes     #-}

module LexicalAnalyser (Token (..), lexicalAnalyse) where

import           Constant


data Token = Keyword String
           | Identifier String
           | Number String
           | Symbol Char
           | Whitespace Char
           | Comment String

instance Show Token where
  show (Keyword s)    = "KEYWORD '" ++ s ++ "'"
  show (Identifier s) = "ID '" ++ s ++ "'"
  show (Number s)     = "NUM '" ++ s ++ "'"
  show (Symbol s)     = "SYMBOL '" ++ [s] ++ "'"
  show (Whitespace s) = "WHITESPACE '" ++ [s] ++ "'"
  show (Comment s)    = "COMMENT '" ++ s ++ "'"

lexicalAnalyse :: String -> [Token]
lexicalAnalyse sourceCode = analyse [] "" False 0
  where
  analyse :: [Token]
          -> String
          -> Bool
          -> Int
          -> [Token]
  analyse tokens memory wordAnalyse index
    | reachedToBottom =
        case () of
          () | wordAnalyse -> tokens ++ [finaliseWordAnalyse]
             | otherwise   -> tokens
    | c `elem` whitespaces =
        case () of
          () | wordAnalyse -> withNewTokens [finaliseWordAnalyse, Whitespace c] $
                                withClearedMemory $
                                withoutWordAnalysing
                                withNextIndex
             | otherwise   -> withNewToken (Whitespace c) $
                                withoutNewMemoryChar $
                                withoutWordAnalysing
                                withNextIndex
    | c `elem` symbols =
        case () of
          () | wordAnalyse -> withNewTokens [finaliseWordAnalyse, Symbol c] $
                                withClearedMemory $
                                withoutWordAnalysing
                                withNextIndex
             | otherwise   -> withNewToken (Symbol c) $
                                withoutNewMemoryChar $
                                withoutWordAnalysing
                                withNextIndex
    | c `elem` letters =
        withoutNewToken $
          withNewMemoryChar c $
          withWordAnalysing
          withNextIndex
    | c `elem` digits =
        withoutNewToken $
          withNewMemoryChar c $
          withWordAnalysing
          withNextIndex
    | otherwise = withoutNewToken $ withoutNewMemoryChar $ withoutWordAnalysing withNextIndex
    where
    reachedToBottom = index >= length sourceCode
    c = sourceCode !! index

    withNewTokens :: [Token] -> ((?token :: [Token]) => [Token]) -> [Token]
    withNewTokens newTokens f = let ?token = tokens ++ newTokens in f
    withNewToken :: Token -> ((?token :: [Token]) => [Token]) -> [Token]
    withNewToken newToken f = let ?token = tokens ++ [newToken] in f
    withoutNewToken :: ((?token :: [Token]) => [Token]) -> [Token]
    withoutNewToken f = let ?token = tokens in f

    withNewMemoryChar :: (?token :: [Token])
                      => Char
                      -> ((?token :: [Token], ?memory :: String) => [Token])
                      -> [Token]
    withNewMemoryChar mc f = let ?memory = memory ++ [mc] in f
    withoutNewMemoryChar :: (?token ::[Token])
                         => ((?token :: [Token], ?memory :: String) => [Token])
                         -> [Token]
    withoutNewMemoryChar f = let ?memory = memory in f
    withClearedMemory :: (?token :: [Token])
                      => ((?token :: [Token], ?memory :: String) => [Token])
                      -> [Token]
    withClearedMemory f = let ?memory = "" in f

    withWordAnalysing :: (?token :: [Token], ?memory :: String)
                      => ((?token :: [Token], ?memory :: String, ?wordAnalyse :: Bool) => [Token])
                      -> [Token]
    withWordAnalysing f = let ?wordAnalyse = True in f
    withoutWordAnalysing :: (?token :: [Token], ?memory :: String)
                         => ((?token :: [Token], ?memory :: String, ?wordAnalyse :: Bool) => [Token])
                         -> [Token]
    withoutWordAnalysing f = let ?wordAnalyse = False in f

    withNextIndex :: (?token :: [Token], ?memory :: String, ?wordAnalyse :: Bool) => [Token]
    withNextIndex = analyse ?token ?memory ?wordAnalyse (index + 1)

    finaliseWordAnalyse :: Token
    finaliseWordAnalyse
      | memory `elem` keywords     = Keyword memory
      | all (`elem` digits) memory = Number memory
      | otherwise                  = Identifier memory

