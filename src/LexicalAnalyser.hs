{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RankNTypes     #-}

module LexicalAnalyser (lexicalAnalyse) where

import           Constant
import           LexicalAnalyseException
import           Token

type AnalyseResult = Either LexicalAnalyseException [Token]

lexicalAnalyse :: String -> AnalyseResult
lexicalAnalyse sourceCode = analyse [] "" False False 0
  where
  analyse :: [Token]
          -> String
          -> Bool
          -> Bool
          -> Int
          -> AnalyseResult
  analyse tokens memory wordAnalyse commentAnalyse index
    | reachedToBottom =
        case () of
          () | commentAnalyse -> Left UnclosingComment
             | wordAnalyse    -> Right $ tokens ++ [finaliseWordAnalyse]
             | otherwise      -> Right tokens
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
    | c `elem` newLines =
        case () of
          () | wordAnalyse -> withNewTokens [finaliseWordAnalyse, NewLine c] $
                                withClearedMemory $
                                withoutWordAnalysing $
                                withoutCommentAnalysing
                                continueAnalysing
             | otherwise   -> withNewToken (NewLine c) $
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
    | otherwise = Left $ UnexpectedCharacter c
    where
    reachedToBottom = index >= length sourceCode
    c = sourceCode !! index

    withNewTokens :: [Token] -> ((?token :: [Token]) => AnalyseResult) -> AnalyseResult
    withNewTokens newTokens f = let ?token = tokens ++ newTokens in f
    withNewToken :: Token -> ((?token :: [Token]) => AnalyseResult) -> AnalyseResult
    withNewToken newToken f = let ?token = tokens ++ [newToken] in f
    withoutNewToken :: ((?token :: [Token]) => AnalyseResult) -> AnalyseResult
    withoutNewToken f = let ?token = tokens in f

    withNewMemoryChar :: (?token :: [Token])
                      => Char
                      -> ((?token :: [Token], ?memory :: String) => AnalyseResult)
                      -> AnalyseResult
    withNewMemoryChar mc f = let ?memory = memory ++ [mc] in f
    withoutNewMemoryChar :: (?token ::[Token])
                         => ((?token :: [Token], ?memory :: String) => AnalyseResult)
                         -> AnalyseResult
    withoutNewMemoryChar f = let ?memory = memory in f
    withClearedMemory :: (?token :: [Token])
                      => ((?token :: [Token], ?memory :: String) => AnalyseResult)
                      -> AnalyseResult
    withClearedMemory f = let ?memory = "" in f

    withWordAnalysing :: (?token :: [Token], ?memory :: String)
                      => ((?token :: [Token], ?memory :: String, ?wordAnalyse :: Bool) => AnalyseResult)
                      -> AnalyseResult
    withWordAnalysing f = let ?wordAnalyse = True in f
    withoutWordAnalysing :: (?token :: [Token], ?memory :: String)
                         => ((?token :: [Token], ?memory :: String, ?wordAnalyse :: Bool) => AnalyseResult)
                         -> AnalyseResult
    withoutWordAnalysing f = let ?wordAnalyse = False in f

    withCommentAnalysing :: (?token :: [Token], ?memory :: String, ?wordAnalyse :: Bool)
                         => ((?token :: [Token], ?memory :: String, ?wordAnalyse :: Bool, ?commentAnalyse :: Bool) => AnalyseResult)
                         -> AnalyseResult
    withCommentAnalysing f = let ?commentAnalyse = True in f
    withoutCommentAnalysing :: (?token :: [Token], ?memory :: String, ?wordAnalyse :: Bool)
                            => ((?token :: [Token], ?memory :: String, ?wordAnalyse :: Bool, ?commentAnalyse :: Bool) => AnalyseResult)
                            -> AnalyseResult
    withoutCommentAnalysing f = let ?commentAnalyse = False in f

    continueAnalysing :: (?token :: [Token], ?memory :: String, ?wordAnalyse :: Bool, ?commentAnalyse :: Bool) => AnalyseResult
    continueAnalysing = analyse ?token ?memory ?wordAnalyse ?commentAnalyse (index + 1)

    finaliseWordAnalyse :: Token
    finaliseWordAnalyse
      | memory `elem` keywords     = Keyword memory
      | all (`elem` digits) memory = Number memory
      | otherwise                  = Identifier memory

