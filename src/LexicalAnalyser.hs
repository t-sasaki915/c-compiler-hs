module LexicalAnalyser (TokenType (..), lexicalAnalyse) where

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
lexicalAnalyse sourceCode = []
