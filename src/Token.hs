module Token (Token (..)) where

data Token = Keyword String
           | Identifier String
           | Number String
           | Symbol Char
           | Whitespace Char
           | NewLine Char
           | Comment String
           deriving Eq

instance Show Token where
  show (Keyword s)    = "KEYWORD '" ++ s ++ "'"
  show (Identifier s) = "ID '" ++ s ++ "'"
  show (Number s)     = "NUM '" ++ s ++ "'"
  show (Symbol c)     = "SYMBOL '" ++ [c] ++ "'"
  show (Whitespace c) = "WHITESPACE '" ++ [c] ++ "'"
  show (NewLine c)    = "NEWLINE '" ++ [c] ++ "'"
  show (Comment s)    = "COMMENT '" ++ s ++ "'"

