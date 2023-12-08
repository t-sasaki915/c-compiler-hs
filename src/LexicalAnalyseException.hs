module LexicalAnalyseException (LexicalAnalyseException (..)) where

import           Control.Exception

data LexicalAnalyseException = UnexpectedCharacter Char
                             | UnclosingComment
                             deriving Eq

instance Exception LexicalAnalyseException

instance Show LexicalAnalyseException where
  show (UnexpectedCharacter c) = "Unexpected character '" ++ [c] ++ "'"
  show UnclosingComment        = "Unclosing comment"

