module SyntaxAnalyseException (SyntaxAnalyseException (..)) where

import           Token

import           Control.Exception

newtype SyntaxAnalyseException = UnexpectedToken Token
                            deriving Eq

instance Exception SyntaxAnalyseException

instance Show SyntaxAnalyseException where
  show (UnexpectedToken t) =
    "Unexpected token (" ++ show t ++ ")"

