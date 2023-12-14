module SyntaxAnalyser (syntaxAnalyse) where

import           SyntaxAnalyseException
import           SyntaxTree
import           Token

type AnalyseResult = Either SyntaxAnalyseException SyntaxTree

data DeclarationAnalyseStep = AnalyseType
                            | AnalyseLabel
                            | AnalyseOpenParentheses
                            | AnalyseArgumentType
                            | AnalyseArgumentLabel
                            | AnalyseArgumentSeparator
                            | AnalyseCloseParentheses
                            | AnalyseOpenBracket
                            | AnalyseCloseBracket

data State = State
  { declarationList :: [SyntaxTree]
  , declarationStep :: DeclarationAnalyseStep
  , index           :: Int
  }

-- TODO: Refactor
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

          (Keyword keyword) ->
            case declarationStep' of
              AnalyseType ->
                case keyword of
                  "void" -> analyse $ State (declarations ++ [Node (TypeSpecifier t) []]) AnalyseLabel (index' + 1)
                  "int"  -> analyse $ State (declarations ++ [Node (TypeSpecifier t) []]) AnalyseLabel (index' + 1)
                  _      -> Left $ UnexpectedToken t "Type"

              AnalyseLabel ->
                Left $ UnexpectedToken t "Identifier"

              AnalyseOpenParentheses ->
                Left $ UnexpectedToken t "'('"

              AnalyseArgumentType ->
                case keyword of
                  "void" -> analyse $ State (declarations ++ [Node (TypeSpecifier t) []]) AnalyseCloseParentheses (index' + 1)
                  "int"  -> analyse $ State (declarations ++ [Node (TypeSpecifier t) []]) AnalyseArgumentLabel (index' + 1)
                  _      -> Left $ UnexpectedToken t "Type"

              AnalyseArgumentLabel ->
                Left $ UnexpectedToken t "Identifier"

              AnalyseArgumentSeparator ->
                Left $ UnexpectedToken t "','"

              AnalyseCloseParentheses ->
                Left $ UnexpectedToken t "')'"

              AnalyseOpenBracket ->
                Left $ UnexpectedToken t "'{'"

              AnalyseCloseBracket ->
                Left $ UnexpectedToken t "'}'"

          (Identifier _) ->
            case declarationStep' of
              AnalyseType ->
                Left $ UnexpectedToken t "Type"

              AnalyseLabel ->
                analyse $ State (declarations ++ [Node (DeclarationLabel t) []]) AnalyseOpenParentheses (index' + 1)

              AnalyseOpenParentheses ->
                Left $ UnexpectedToken t "'('"

              AnalyseArgumentType ->
                Left $ UnexpectedToken t "Type"

              AnalyseArgumentLabel ->
                analyse $ State (declarations ++ [Node (DeclarationLabel t) []]) AnalyseArgumentSeparator (index' + 1)

              AnalyseArgumentSeparator ->
                Left $ UnexpectedToken t "','"

              AnalyseCloseParentheses ->
                Left $ UnexpectedToken t "')'"

              AnalyseOpenBracket ->
                Left $ UnexpectedToken t "'{'"

              AnalyseCloseBracket ->
                Left $ UnexpectedToken t "'}'"

          (Symbol symbol) ->
            case declarationStep' of
              AnalyseType ->
                Left $ UnexpectedToken t "Type"

              AnalyseLabel ->
                Left $ UnexpectedToken t "Identifier"

              AnalyseOpenParentheses ->
                case symbol of
                  '(' -> analyse $ State declarations AnalyseArgumentType (index' + 1)
                  _   -> Left $ UnexpectedToken t "'('"

              AnalyseArgumentType ->
                Left $ UnexpectedToken t "Type"

              AnalyseArgumentLabel ->
                Left $ UnexpectedToken t "Identifier"

              AnalyseArgumentSeparator ->
                case symbol of
                  ',' -> analyse $ State declarations AnalyseArgumentType (index' + 1)
                  ')' -> analyse $ State declarations AnalyseOpenBracket (index' + 1)
                  _   -> Left $ UnexpectedToken t "','"

              AnalyseCloseParentheses ->
                case symbol of
                  ')' -> analyse $ State declarations AnalyseOpenBracket (index' + 1)
                  _   -> Left $ UnexpectedToken t "')'"

              AnalyseOpenBracket ->
                case symbol of
                  '{' -> analyse $ State declarations AnalyseCloseBracket (index' + 1)
                  _   -> Left $ UnexpectedToken t "'{'"

              AnalyseCloseBracket ->
                case symbol of
                  '}' -> analyse $ State declarations AnalyseType (index' + 1)
                  _   -> Left $ UnexpectedToken t "'}'"

          _ ->
            analyse $ State declarations declarationStep' (index' + 1)
    where
    index' = index state
    declarations = declarationList state
    declarationStep' = declarationStep state

    reachedToBottom = index' >= length tokens
    t = tokens !! index'
