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

instance Eq Token where
  (==) (Keyword x) (Keyword y)       = x == y
  (==) (Identifier x) (Identifier y) = x == y
  (==) (Number x) (Number y)         = x == y
  (==) (Symbol x) (Symbol y)         = x == y
  (==) (Whitespace x) (Whitespace y) = x == y
  (==) (Comment x) (Comment y)       = x == y
  (==) _ _                           = False

lexicalAnalyse :: String -> [Token]
lexicalAnalyse sourceCode = analyse [] "" False False 0
  where
  analyse :: [Token]
          -> String
          -> Bool
          -> Bool
          -> Int
          -> [Token]
  analyse tokens memory wordAnalyse commentAnalyse index
    | reachedToBottom =
        case () of
          () | commentAnalyse -> tokens ++ [Comment memory] -- TODO: ERROR
             | wordAnalyse    -> tokens ++ [finaliseWordAnalyse]
             | otherwise      -> tokens
    | c `elem` whitespaces =
        case () of
          () | wordAnalyse -> withNewTokens [finaliseWordAnalyse, Whitespace c] $
                                withClearedMemory $
                                withoutWordAnalysing $
                                withoutCommentAnalysing
                                continueAnalysing
             | otherwise   -> withNewToken (Whitespace c) $
                                withoutNewMemoryChar $
                                withoutWordAnalysing $
                                withoutCommentAnalysing
                                continueAnalysing
    | c `elem` symbols =
        case () of
          () | wordAnalyse -> withNewTokens [finaliseWordAnalyse, Symbol c] $
                                withClearedMemory $
                                withoutWordAnalysing $
                                withoutCommentAnalysing
                                continueAnalysing
             | otherwise   -> withNewToken (Symbol c) $
                                withoutNewMemoryChar $
                                withoutWordAnalysing $
                                withoutCommentAnalysing
                                continueAnalysing
    | c `elem` letters =
        withoutNewToken $
          withNewMemoryChar c $
          withWordAnalysing $
          withoutCommentAnalysing
          continueAnalysing
    | c `elem` digits =
        withoutNewToken $
          withNewMemoryChar c $
          withWordAnalysing $
          withoutCommentAnalysing
          continueAnalysing
    | otherwise = withoutNewToken $ withoutNewMemoryChar $ withoutWordAnalysing $ withoutCommentAnalysing continueAnalysing
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

    withCommentAnalysing :: (?token :: [Token], ?memory :: String, ?wordAnalyse :: Bool)
                         => ((?token :: [Token], ?memory :: String, ?wordAnalyse :: Bool, ?commentAnalyse :: Bool) => [Token])
                         -> [Token]
    withCommentAnalysing f = let ?commentAnalyse = True in f
    withoutCommentAnalysing :: (?token :: [Token], ?memory :: String, ?wordAnalyse :: Bool)
                            => ((?token :: [Token], ?memory :: String, ?wordAnalyse :: Bool, ?commentAnalyse :: Bool) => [Token])
                            -> [Token]
    withoutCommentAnalysing f = let ?commentAnalyse = False in f

    continueAnalysing :: (?token :: [Token], ?memory :: String, ?wordAnalyse :: Bool, ?commentAnalyse :: Bool) => [Token]
    continueAnalysing = analyse ?token ?memory ?wordAnalyse ?commentAnalyse (index + 1)

    finaliseWordAnalyse :: Token
    finaliseWordAnalyse
      | memory `elem` keywords     = Keyword memory
      | all (`elem` digits) memory = Number memory
      | otherwise                  = Identifier memory

