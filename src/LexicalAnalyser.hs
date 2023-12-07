module LexicalAnalyser (TokenType (..), lexicalAnalyse) where

import           Constant

data TokenType = Keyword
               | Identifier
               | Number
               | Symbol
               | Whitespace
               | Comment

instance Show TokenType where
  show Keyword    = "KEYWORD"
  show Identifier = "ID"
  show Number     = "NUM"
  show Symbol     = "SYMBOL"
  show Whitespace = "WHITESPACE"
  show Comment    = "COMMENT"

lexicalAnalyse :: String -> [(TokenType, String)]
lexicalAnalyse sourceCode = analyse [] 0
  where
  analyse :: [(TokenType, String)] -> Int -> [(TokenType, String)]
  analyse previous index
    | reachedToBottom    = exitLoop
    | elem c whitespaces = addTokenAndNext (Whitespace, c')
    | elem c symbols     = addTokenAndNext (Symbol, c')
    | otherwise          = doNothingAndNext
    where
    reachedToBottom = index >= (length sourceCode)
    c = sourceCode !! index
    c' = [c] :: String

    exitLoop = previous
    doNothingAndNext = analyse previous (index + 1)
    addTokenAndNext token = analyse (previous ++ [token]) (index + 1)

