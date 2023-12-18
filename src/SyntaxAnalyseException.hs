module SyntaxAnalyseException (SyntaxAnalyseException (..)) where

import           Token

import           Control.Exception

data SyntaxAnalyseException = UnexpectedToken Token String
                            | IllegalArgumentDeclaration
                            | UnclosingDeclaration
                            deriving Eq

instance Exception SyntaxAnalyseException

instance Show SyntaxAnalyseException where
  show (UnexpectedToken t e) =
    "Unexpected token (" ++ show t ++ ") (expected " ++ e ++ ")"

  show IllegalArgumentDeclaration =
    "Illegal argument declaration"

  show UnclosingDeclaration =
    "Unclosing declaration"

