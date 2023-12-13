module SyntaxAnalyser (syntaxAnalyse) where

import           SyntaxAnalyseException
import           SyntaxTree
import           Token

type AnalyseResult = Either SyntaxAnalyseException SyntaxTree

syntaxAnalyse :: [Token] -> AnalyseResult
syntaxAnalyse tokens = Right $ Node Program []
