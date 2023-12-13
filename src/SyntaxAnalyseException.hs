module SyntaxAnalyseException (SyntaxAnalyseException (..)) where

import           Token

import           Control.Exception

data SyntaxAnalyseException = UnexpectedToken Token String
                            deriving Eq

instance Exception SyntaxAnalyseException

instance Show SyntaxAnalyseException where
  show (UnexpectedToken t e) =
    "Unexpected token (" ++ show t ++ ") (expected " ++ e ++ ")"

