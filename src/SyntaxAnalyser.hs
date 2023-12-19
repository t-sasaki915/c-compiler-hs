{-# LANGUAGE TemplateHaskell #-}

module SyntaxAnalyser (syntaxAnalyse) where

import           Constant               (typeKeywords)
import           SyntaxAnalyseException
import           SyntaxTree
import           Token
import           Util                   (combineList)

import           Control.Lens           hiding (index)
import           Data.Maybe             (fromJust)

type AnalyseResult = Either SyntaxAnalyseException SyntaxTree

data DeclarationAnalyseStep = AnalyseType
                            | AnalyseLabel
                            | AnalyseOpenParentheses
                            | AnalyseArgumentType
                            | AnalyseArgumentLabel
                            | AnalyseArgumentSeparator
                            | AnalyseCloseParentheses
                            | AnalyseOpenBraces
                            | AnalyseFunctionValue

data FunctionValueAnalyseStep = AnalyseVerb
                              | AnalyseVerbArgument
                              | AnalyseSemicolon

data State = State
  { _declarationList       :: [SyntaxTree]
  , _declarationType       :: Maybe Token
  , _declarationLabel      :: Maybe Token
  , _declarationArgTypes   :: [Token]
  , _declarationArgLabels  :: [Token]
  , _declarationStep       :: DeclarationAnalyseStep
  , _functionValueStep     :: FunctionValueAnalyseStep
  , _functionVerbs         :: [Token]
  , _functionVerbArgs      :: [[Token]]
  , _functionVerbArgMemory :: [Token]
  , _index                 :: Int
  }

makeLenses ''State

declarationTree :: State -> SyntaxTree
declarationTree s =
  Node Declaration
    (
      [ Node (TypeSpecifier $ fromJust (_declarationType s)) []
      , Node (DeclarationLabel $ fromJust (_declarationLabel s)) []
      ] ++ map makeArgTree (combineList (_declarationArgTypes s) (_declarationArgLabels s))
        ++ map makeOperationTree (combineList (_functionVerbs s) (_functionVerbArgs s))
    )
  where
  makeArgTree arg =
    Node DeclarationArgument
      [ Node (TypeSpecifier $ fst arg) []
      , Node (DeclarationLabel $ snd arg) []
      ]
  makeOperationTree operation =
    Node (uncurry Operation operation) []

syntaxAnalyse :: [Token] -> AnalyseResult
syntaxAnalyse tokens = analyse $ State [] Nothing Nothing [] [] AnalyseType AnalyseVerb [] [] [] 0
  where
  analyse :: State -> AnalyseResult
  analyse state
    | reachedToBottom =
        case declarationStep' of
          AnalyseType ->
            Right $
              Node Program
                [ Node DeclarationList declarations
                ]
          _ ->
            Left UnclosingDeclaration
    | otherwise =
        case t of
          (Whitespace _) ->
            ignoreAndDoNothing

          (NewLine _) ->
            ignoreAndDoNothing

          (Comment _) ->
            ignoreAndDoNothing

          (Keyword keyword) ->
            case declarationStep' of
              AnalyseType ->
                case () of
                  () | keyword `elem` typeKeywords ->
                         continueAnalysing $
                           set declarationType (Just t) .
                           set declarationStep AnalyseLabel .
                           over index (+ 1)
                     | otherwise ->
                         Left $ UnexpectedToken t "Type"

              AnalyseArgumentType ->
                case keyword of
                  "void" ->
                    case () of
                      () | null $ _declarationArgTypes state ->
                             continueAnalysing $
                               set declarationStep AnalyseCloseParentheses .
                               over index (+ 1)
                         | otherwise ->
                             Left IllegalArgumentDeclaration
                  _ ->
                    case () of
                      () | keyword `elem` typeKeywords ->
                             continueAnalysing $
                               over declarationArgTypes (++ [t]) .
                               set declarationStep AnalyseArgumentLabel .
                               over index (+ 1)
                         | otherwise ->
                             Left $ UnexpectedToken t "Type or ')'"

              AnalyseFunctionValue ->
                case functionValueStep' of
                  AnalyseVerb ->
                    case keyword of
                      "return" ->
                        continueAnalysing $
                          over functionVerbs (++ [t]) .
                          set functionValueStep AnalyseVerbArgument .
                          over index (+ 1)
                      _ ->
                        contextualUnexpectedTokenHalt

                  _ ->
                    contextualUnexpectedTokenHalt

              _ ->
                contextualUnexpectedTokenHalt

          (Identifier _) ->
            case declarationStep' of
              AnalyseLabel ->
                continueAnalysing $
                  set declarationLabel (Just t) .
                  set declarationStep AnalyseOpenParentheses .
                  over index (+ 1)

              AnalyseArgumentLabel ->
                continueAnalysing $
                  over declarationArgLabels (++ [t]) .
                  set declarationStep AnalyseArgumentSeparator .
                  over index (+ 1)

              AnalyseFunctionValue ->
                case functionValueStep' of
                  AnalyseVerbArgument ->
                    continueAnalysing $
                      over functionVerbArgMemory (++ [t]) .
                      set functionValueStep AnalyseSemicolon .
                      over index (+ 1)

                  _ ->
                    contextualUnexpectedTokenHalt

              _ ->
                contextualUnexpectedTokenHalt

          (Symbol symbol) ->
            case declarationStep' of
              AnalyseOpenParentheses ->
                case symbol of
                  '(' ->
                    continueAnalysing $
                      set declarationStep AnalyseArgumentType .
                      over index (+ 1)
                  _ ->
                    Left $ UnexpectedToken t "'('"

              AnalyseArgumentType ->
                case symbol of
                  ')' ->
                    continueAnalysing $
                      set declarationStep AnalyseOpenBraces .
                      over index (+ 1)
                  _ ->
                    Left $ UnexpectedToken t "Type or ')'"

              AnalyseArgumentSeparator ->
                case symbol of
                  ',' ->
                    continueAnalysing $
                      set declarationStep AnalyseArgumentType .
                      over index (+ 1)
                  ')' ->
                    continueAnalysing $
                      set declarationStep AnalyseOpenBraces .
                      over index (+ 1)
                  _ ->
                    Left $ UnexpectedToken t "',' or ')'"

              AnalyseCloseParentheses ->
                case symbol of
                  ')' ->
                    continueAnalysing $
                      set declarationStep AnalyseOpenBraces .
                      over index (+ 1)
                  _ ->
                    Left $ UnexpectedToken t "')'"

              AnalyseOpenBraces ->
                case symbol of
                  '{' ->
                    continueAnalysing $
                      set declarationStep AnalyseFunctionValue .
                      over index (+ 1)
                  _ ->
                    Left $ UnexpectedToken t "'{'"

              AnalyseFunctionValue ->
                case functionValueStep' of
                  AnalyseVerb ->
                    case symbol of
                      '}' ->
                        continueAnalysing $
                          over declarationList (++ [declarationTree state]) .
                          set declarationType Nothing .
                          set declarationLabel Nothing .
                          set declarationArgTypes [] .
                          set declarationArgLabels [] .
                          set declarationStep AnalyseType .
                          set functionValueStep AnalyseVerb .
                          set functionVerbs [] .
                          set functionVerbArgs [] .
                          set functionVerbArgMemory [] .
                          over index (+ 1)
                      _ ->
                        contextualUnexpectedTokenHalt

                  AnalyseVerbArgument ->
                    case symbol of
                      ';' ->
                        continueAnalysing $
                          over functionVerbArgs (++ [_functionVerbArgMemory state]) .
                          set functionVerbArgMemory [] .
                          set functionValueStep AnalyseVerb .
                          over index (+ 1)
                      _ ->
                        contextualUnexpectedTokenHalt

                  AnalyseSemicolon ->
                    case symbol of
                      ';' ->
                        continueAnalysing $
                          over functionVerbArgs (++ [_functionVerbArgMemory state]) .
                          set functionVerbArgMemory [] .
                          set functionValueStep AnalyseVerb .
                          over index (+ 1)
                      _ ->
                        contextualUnexpectedTokenHalt

              _ ->
                contextualUnexpectedTokenHalt

          (Number _) ->
            case declarationStep' of
              AnalyseFunctionValue ->
                case functionValueStep' of
                  AnalyseVerbArgument ->
                    continueAnalysing $
                      over functionVerbArgMemory (++ [t]) .
                      set functionValueStep AnalyseSemicolon .
                      over index (+ 1)

                  _ ->
                    contextualUnexpectedTokenHalt

              _ ->
                contextualUnexpectedTokenHalt
    where
    index' = _index state
    declarations = _declarationList state
    declarationStep' = _declarationStep state
    functionValueStep' = _functionValueStep state

    reachedToBottom = index' >= length tokens
    t = tokens !! index'

    continueAnalysing :: (State -> State) -> AnalyseResult
    continueAnalysing f = analyse $ f state

    ignoreAndDoNothing :: AnalyseResult
    ignoreAndDoNothing =
      continueAnalysing $
        over index (+1)

    contextualUnexpectedTokenHalt :: AnalyseResult
    contextualUnexpectedTokenHalt = Left $ UnexpectedToken t expectation
      where
        expectation =
          case declarationStep' of
            AnalyseType              -> "Type"
            AnalyseLabel             -> "Identifier"
            AnalyseOpenParentheses   -> "'('"
            AnalyseArgumentType      -> "Type or ')'"
            AnalyseArgumentLabel     -> "Identifier"
            AnalyseArgumentSeparator -> "',' or ')'"
            AnalyseCloseParentheses  -> "')'"
            AnalyseOpenBraces        -> "'{'"
            AnalyseFunctionValue     ->
              case functionValueStep' of
                AnalyseVerb         -> "Verb or '}'"
                AnalyseVerbArgument -> "Verb argument"
                AnalyseSemicolon    -> "';'"

