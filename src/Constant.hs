module Constant
  ( keywords
  , typeKeywords
  , symbols
  , letters
  , digits
  , whitespaces
  , newLines
  , numberFormat
  ) where

keywords :: [String]
keywords = [ "int" , "return", "void" ]

typeKeywords :: [String]
typeKeywords = [ "int", "void" ]

symbols :: [Char]
symbols = [ '(' , ')' , '{' , '}' , ';', '/', '*', ',' ]

letters :: [Char]
letters = [ 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm'
          , 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z'
          , 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M'
          , 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z'
          ]

digits :: [Char]
digits = [ '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' ]

whitespaces :: [Char]
whitespaces = [ ' ', '\r', '\t', '\v', '\f' ]

newLines :: [Char]
newLines = [ '\n' ]

numberFormat :: String
numberFormat = "[0-9]+(\\.[0-9]+)?"
