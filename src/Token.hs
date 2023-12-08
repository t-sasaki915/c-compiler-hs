module Token (Token (..)) where

data Token = Keyword String
           | Identifier String
           | Number String
           | Symbol Char
           | Whitespace Char
           | NewLine Char
           | Comment String

instance Show Token where
  show (Keyword s)    = "KEYWORD '" ++ s ++ "'"
  show (Identifier s) = "ID '" ++ s ++ "'"
  show (Number s)     = "NUM '" ++ s ++ "'"
  show (Symbol c)     = "SYMBOL '" ++ [c] ++ "'"
  show (Whitespace c) = "WHITESPACE '" ++ [c] ++ "'"
  show (NewLine c)    = "NEWLINE '" ++ [c] ++ "'"
  show (Comment s)    = "COMMENT '" ++ s ++ "'"

instance Eq Token where
  (==) (Keyword x) (Keyword y)       = x == y
  (==) (Identifier x) (Identifier y) = x == y
  (==) (Number x) (Number y)         = x == y
  (==) (Symbol x) (Symbol y)         = x == y
  (==) (Whitespace x) (Whitespace y) = x == y
  (==) (NewLine x) (NewLine y)       = x == y
  (==) (Comment x) (Comment y)       = x == y
  (==) _ _                           = False

