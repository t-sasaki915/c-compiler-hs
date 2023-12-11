module LexicalAnalyseException (LexicalAnalyseException (..)) where

import           SourceCodeUtil

import           Control.Exception

data LexicalAnalyseException = UnexpectedCharacter Int String Char
                             | UnclosingComment Int String
                             deriving Eq

instance Exception LexicalAnalyseException

lineAndIndex :: Int -> String -> String
lineAndIndex index sourceCode =
  "Line " ++ line ++ ", Index " ++ index'
  where
  line = show $ calculateLine index sourceCode
  index' = show $ calculateIndexOfLine index sourceCode


instance Show LexicalAnalyseException where
  show (UnexpectedCharacter i src c) =
    "Unexpected character '" ++ [c] ++ "' at " ++ lineAndIndex i src
  show (UnclosingComment i src) =
    "Unclosing comment at" ++ lineAndIndex i src

