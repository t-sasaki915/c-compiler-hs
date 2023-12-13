module SyntaxAnalyser (syntaxAnalyse) where

import           SyntaxAnalyseException
import           SyntaxTree
import           Token

type AnalyseResult = Either SyntaxAnalyseException SyntaxTree

data DeclarationAnalyseStep = AnalyseType
                            | AnalyseLabel

data State = State
  { declarationList :: [SyntaxTree]
  , declarationStep :: DeclarationAnalyseStep
  , index           :: Int
  }

syntaxAnalyse :: [Token] -> AnalyseResult
syntaxAnalyse tokens = analyse $ State [] AnalyseType 0
  where
  analyse :: State -> AnalyseResult
  analyse state
    | reachedToBottom =
        Right $
          Node Program
            [ Node DeclarationList declarations
            ]
    | otherwise =
        case t of
          (Whitespace _) ->
            analyse $ State declarations declarationStep' (index' + 1)

          (NewLine _) ->
            analyse $ State declarations declarationStep' (index' + 1)

          (Comment _) ->
            analyse $ State declarations declarationStep' (index' + 1)

          (Keyword _) ->
            case declarationStep' of
              AnalyseType ->
                analyse $ State (declarations ++ [Node (TypeSpecifier t) []]) AnalyseLabel (index' + 1)

              AnalyseLabel ->
                Left $ UnexpectedToken t "Identifier"

          (Identifier _) ->
            case declarationStep' of
              AnalyseType ->
                Left $ UnexpectedToken t "Type"

              AnalyseLabel ->
                analyse $ State (declarations ++ [Node (DeclarationLabel t) []]) AnalyseType (index' + 1)

          _ ->
            analyse $ State declarations declarationStep' (index' + 1)
    where
    index' = index state
    declarations = declarationList state
    declarationStep' = declarationStep state

    reachedToBottom = index' >= length tokens
    t = tokens !! index'
