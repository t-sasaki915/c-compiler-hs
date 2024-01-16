{-# LANGUAGE TemplateHaskell #-}

module SyntaxAnalyser (syntaxAnalyse) where

import           Constant               (typeKeywords)
import           ExpressionAnalyser
import           SyntaxAnalyseException
import           SyntaxTree
import           Token
import           Util                   (combineList)

import           Control.Lens           hiding (index, op)
import           Data.Maybe             (fromJust)

type AnalyseResult = Either SyntaxAnalyseException SyntaxTree

data DefinitionAnalyseStep = AnalyseDefType
                           | AnalyseDefLabel
                           | AnalyseEqualOrOpenParentheses
                           | AnalyseVarDefSemicolon
                           | AnalyseFunDefArgType
                           | AnalyseFunDefArgLabel
                           | AnalyseFunDefArgSeparatorOrCloseParentheses
                           | AnalyseFunDefCloseParentheses
                           | AnalyseFunDefOpenBraces
                           | AnalyseFunDefValue

data OperationAnalyseStep = AnalyseOpType
                          | AnalyseReturnSemicolon

data State = State
  { _definitionList          :: [SyntaxTree]
  , _defType                 :: Maybe Token
  , _defLabel                :: Maybe Token
  , _varDefValueTokens       :: [Token]
  , _funDefArgTypes          :: [Token]
  , _funDefArgLabels         :: [Token]
  , _defAnalyseStep          :: DefinitionAnalyseStep
  , _funDefOperations        :: [Token]
  , _funDefOperationArgs     :: [[SyntaxTree]]
  , _funDefOperationArgsTemp :: [SyntaxTree]
  , _opAnalyseStep           :: OperationAnalyseStep
  , _index                   :: Int
  }

makeLenses ''State

mkVarDefTree :: State -> SyntaxTree
mkVarDefTree s =
  Node (VarDefinition (fromJust $ _defType s) (fromJust $ _defLabel s))
    [ Node (Expression $ _varDefValueTokens s) []
    ]

mkFunDefTree :: State -> SyntaxTree
mkFunDefTree s =
  Node (FunDefinition (fromJust $ _defType s) (fromJust $ _defLabel s)) (argDefTrees ++ opTrees)
  where
  argDefTrees = map (\arg -> Node (uncurry VarDefinition arg) [])
                      (combineList (_funDefArgTypes s) (_funDefArgLabels s))
  opTrees = map (\op -> Node (Operation $ fst op) (snd op))
                  (combineList (_funDefOperations s) (_funDefOperationArgs s))

syntaxAnalyse :: [Token] -> AnalyseResult
syntaxAnalyse tokens = analyse $ State [] Nothing Nothing [] [] [] AnalyseDefType [] [] [] AnalyseOpType 0
  where
  analyse :: State -> AnalyseResult
  analyse state
    | reachedToBottom =
        case defAnalyseStep' of
          AnalyseDefType ->
            Right $
              Node Program
                [ Node DefinitionList definitions
                ]
          _ ->
            Left UnclosingDefinition
    | otherwise =
        case t of
          (Whitespace _) ->
            ignoreAndDoNothing

          (NewLine _) ->
            ignoreAndDoNothing

          (Comment _) ->
            ignoreAndDoNothing

          (Keyword keyword) ->
            case defAnalyseStep' of
              AnalyseDefType ->
                case () of
                  () | keyword `elem` typeKeywords ->
                         continueAnalysing $
                           set defType (Just t) .
                           set defAnalyseStep AnalyseDefLabel .
                           over index (+ 1)
                     | otherwise ->
                         contextualUnexpectedTokenHalt

              AnalyseFunDefArgType ->
                case keyword of
                  "void" ->
                    case () of
                      () | null $ _funDefArgTypes state ->
                             continueAnalysing $
                               set defAnalyseStep AnalyseFunDefCloseParentheses .
                               over index (+ 1)
                         | otherwise ->
                             Left IllegalArgumentDefinition
                  _ ->
                    case () of
                      () | keyword `elem` typeKeywords ->
                             continueAnalysing $
                               over funDefArgTypes (++ [t]) .
                               set defAnalyseStep AnalyseFunDefArgLabel .
                               over index (+ 1)
                         | otherwise ->
                             contextualUnexpectedTokenHalt

              AnalyseFunDefValue ->
                case opAnalyseStep' of
                  AnalyseOpType ->
                    case keyword of
                      "return" ->
                        case expressionAnalyse tokens (index' + 1) of
                          Just ([], newIndex) ->
                            continueAnalysing $
                              over funDefOperations (++ [t]) .
                              over funDefOperationArgs (++ [[]]) .
                              set opAnalyseStep AnalyseReturnSemicolon .
                              set index newIndex
                          Just (expr, newIndex) ->
                            continueAnalysing $
                              over funDefOperations (++ [t]) .
                              over funDefOperationArgs (++ [[Node (Expression expr) []]]) .
                              set opAnalyseStep AnalyseReturnSemicolon .
                              set index newIndex
                          Nothing ->
                            Left UnrecognisableExpression
                      _ ->
                        contextualUnexpectedTokenHalt

                  _ ->
                    contextualUnexpectedTokenHalt

              _ ->
                contextualUnexpectedTokenHalt

          (Identifier _) ->
            case defAnalyseStep' of
              AnalyseDefLabel ->
                continueAnalysing $
                  set defLabel (Just t) .
                  set defAnalyseStep AnalyseEqualOrOpenParentheses .
                  over index (+ 1)

              AnalyseFunDefArgLabel ->
                continueAnalysing $
                  over funDefArgLabels (++ [t]) .
                  set defAnalyseStep AnalyseFunDefArgSeparatorOrCloseParentheses .
                  over index (+ 1)

              _ ->
                contextualUnexpectedTokenHalt

          (Symbol symbol) ->
            case defAnalyseStep' of
              AnalyseEqualOrOpenParentheses ->
                case symbol of
                  '=' ->
                    case expressionAnalyse tokens (index' + 1) of
                      Just ([], _) ->
                        Left UnrecognisableExpression
                      Just (expr, newIndex) ->
                        continueAnalysing $
                          set varDefValueTokens expr .
                          set defAnalyseStep AnalyseVarDefSemicolon .
                          set index newIndex
                      Nothing ->
                        Left UnrecognisableExpression
                  '(' ->
                    continueAnalysing $
                      set defAnalyseStep AnalyseFunDefArgType .
                      over index (+ 1)
                  _ ->
                    contextualUnexpectedTokenHalt

              AnalyseVarDefSemicolon ->
                case symbol of
                  ';' ->
                    continueAnalysing $
                      over definitionList (++ [mkVarDefTree state]) .
                      set defType Nothing .
                      set defLabel Nothing .
                      set varDefValueTokens [] .
                      set defAnalyseStep AnalyseDefType .
                      over index (+ 1)
                  _ ->
                    contextualUnexpectedTokenHalt

              AnalyseFunDefArgType ->
                case symbol of
                  ')' ->
                    continueAnalysing $
                      set defAnalyseStep AnalyseFunDefOpenBraces .
                      over index (+ 1)
                  _ ->
                    contextualUnexpectedTokenHalt

              AnalyseFunDefArgSeparatorOrCloseParentheses ->
                case symbol of
                  ',' ->
                    continueAnalysing $
                      set defAnalyseStep AnalyseFunDefArgType .
                      over index (+ 1)
                  ')' ->
                    continueAnalysing $
                      set defAnalyseStep AnalyseFunDefOpenBraces .
                      over index (+ 1)
                  _ ->
                    contextualUnexpectedTokenHalt

              AnalyseFunDefCloseParentheses ->
                case symbol of
                  ')' ->
                    continueAnalysing $
                      set defAnalyseStep AnalyseFunDefOpenBraces .
                      over index (+ 1)
                  _ ->
                    contextualUnexpectedTokenHalt

              AnalyseFunDefOpenBraces ->
                case symbol of
                  '{' ->
                    continueAnalysing $
                      set defAnalyseStep AnalyseFunDefValue .
                      over index (+ 1)
                  _ ->
                    contextualUnexpectedTokenHalt

              AnalyseFunDefValue ->
                case opAnalyseStep' of
                  AnalyseOpType ->
                    case symbol of
                      '}' ->
                        continueAnalysing $
                          over definitionList (++ [mkFunDefTree state]) .
                          set defType Nothing .
                          set defLabel Nothing .
                          set funDefArgTypes [] .
                          set funDefArgLabels [] .
                          set defAnalyseStep AnalyseDefType .
                          set funDefOperations [] .
                          set funDefOperationArgs [] .
                          set funDefOperationArgsTemp [] .
                          set opAnalyseStep AnalyseOpType .
                          over index (+ 1)
                      _ ->
                        contextualUnexpectedTokenHalt

                  AnalyseReturnSemicolon ->
                    case symbol of
                      ';' ->
                        continueAnalysing $
                          set opAnalyseStep AnalyseOpType .
                          over index (+ 1)
                      _ ->
                        contextualUnexpectedTokenHalt

              _ ->
                contextualUnexpectedTokenHalt

          (Number _) ->
            contextualUnexpectedTokenHalt

    where
    index' = _index state
    definitions = _definitionList state
    defAnalyseStep' = _defAnalyseStep state
    opAnalyseStep' = _opAnalyseStep state

    reachedToBottom = index' >= length tokens
    t = tokens !! index'

    continueAnalysing :: (State -> State) -> AnalyseResult
    continueAnalysing f = analyse $ f state

    ignoreAndDoNothing :: AnalyseResult
    ignoreAndDoNothing =
      continueAnalysing $
        over index (+ 1)

    contextualUnexpectedTokenHalt :: AnalyseResult
    contextualUnexpectedTokenHalt = Left $ UnexpectedToken t expectation
      where
        expectation =
          case defAnalyseStep' of
            AnalyseDefType                              -> "Type"
            AnalyseDefLabel                             -> "Identifier"
            AnalyseEqualOrOpenParentheses               -> "'=' or '('"
            AnalyseVarDefSemicolon                      -> "';'"
            AnalyseFunDefArgType                        -> "Type or ')'"
            AnalyseFunDefArgLabel                       -> "Identifier"
            AnalyseFunDefArgSeparatorOrCloseParentheses -> "',' or ')'"
            AnalyseFunDefCloseParentheses               -> "')'"
            AnalyseFunDefOpenBraces                     -> "'{'"
            AnalyseFunDefValue ->
              case opAnalyseStep' of
                AnalyseOpType          -> "Keyword or '}'"
                AnalyseReturnSemicolon -> "';'"
