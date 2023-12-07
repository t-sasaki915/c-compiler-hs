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
lexicalAnalyse sourceCode = analyse [] "" False 0
  where
  analyse :: [(TokenType, String)]
          -> String
          -> Bool
          -> Int
          -> [(TokenType, String)]
  analyse previous memory keywordAnalyse index
    | reachedToBottom =
        case () of
          () | keywordAnalyse -> previous ++ [(Keyword, memory)]
             | otherwise      -> previous

    | c `elem` whitespaces =
        case () of
          () | keywordAnalyse -> analyse (previous ++ [(Keyword, memory), (Whitespace, c')]) "" False (index + 1)
             | otherwise      -> analyse (previous ++ [(Whitespace, c')]) memory keywordAnalyse (index + 1)

    | c `elem` symbols =
        case () of
          () | keywordAnalyse -> analyse (previous ++ [(Keyword, memory), (Symbol, c')]) "" False (index + 1)
             | otherwise      -> analyse (previous ++ [(Symbol, c')]) memory keywordAnalyse (index + 1)

    | c `elem` letters =
        case () of
          () | keywordAnalyse -> analyse previous (memory ++ c') True (index + 1)
             | otherwise      -> analyse previous (memory ++ c') True (index + 1)

    | c `elem` digits =
        case () of
          () | keywordAnalyse -> analyse previous (memory ++ c') True (index + 1)
             | otherwise      -> analyse (previous ++ [(Number, c')]) memory keywordAnalyse (index + 1)

    | otherwise = analyse previous memory keywordAnalyse (index + 1)
    where
    reachedToBottom = index >= length sourceCode
    c = sourceCode !! index
    c' = [c] :: String

