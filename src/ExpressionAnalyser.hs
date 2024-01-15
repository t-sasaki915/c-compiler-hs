{-# LANGUAGE TemplateHaskell #-}

module ExpressionAnalyser (expressionAnalyse) where

import           Constant     (calculationSymbols)
import           Token
import           Util         (maybeLast)

import           Control.Lens hiding (index)

type AnalyseResult = Maybe ([Token], Int)

data State = State
  { _expTokens                  :: [Token]
  , _numberOfParenthesesToClose :: Int
  , _index                      :: Int
  }

makeLenses ''State

expressionAnalyse :: [Token] -> Int -> AnalyseResult
expressionAnalyse tokens i = analyse $ State [] 0 i
  where
  analyse :: State -> AnalyseResult
  analyse state
    | reachedToBottom = Nothing
    | otherwise =
        case t of
          Whitespace _ ->
            ignoreAndDoNothing

          NewLine _ ->
            ignoreAndDoNothing

          Comment _ ->
            ignoreAndDoNothing

          Symbol ';' ->
            case () of
              () | null $ _expTokens state                -> Nothing
                 | _numberOfParenthesesToClose state /= 0 -> Nothing
                 | otherwise                              -> Just (_expTokens state, index')

          Symbol ',' ->
            case () of
              () | null $ _expTokens state                -> Nothing
                 | _numberOfParenthesesToClose state /= 0 -> Nothing
                 | otherwise                              -> Just (_expTokens state, index')

          Identifier _ ->
            case lastToken of
              Just (Symbol ')') ->
                Nothing
              Just (Symbol _) ->
                continueAnalysing $
                  over expTokens (++ [t]) .
                  over index (+ 1)
              Nothing ->
                continueAnalysing $
                  over expTokens (++ [t]) .
                  over index (+ 1)
              _ ->
                Nothing

          Number _ ->
            case lastToken of
              Just (Symbol ')') ->
                Nothing
              Just (Symbol _) ->
                continueAnalysing $
                  over expTokens (++ [t]) .
                  over index (+ 1)
              Nothing ->
                continueAnalysing $
                  over expTokens (++ [t]) .
                  over index (+ 1)
              _ ->
                Nothing

          Symbol s ->
            case () of
              () | s `elem` calculationSymbols ->
                     case lastToken of
                       Just (Identifier _) ->
                         continueAnalysing $
                           over expTokens (++ [t]) .
                           over index (+ 1)
                       Just (Symbol ')') ->
                         continueAnalysing $
                           over expTokens (++ [t]) .
                           over index (+ 1)
                       _ ->
                         Nothing
                 | s == '(' ->
                     case lastToken of
                       Just (Symbol ')') ->
                         Nothing
                       Just (Symbol _) ->
                         continueAnalysing $
                           over expTokens (++ [t]) .
                           over numberOfParenthesesToClose (+ 1) .
                           over index (+ 1)
                       Nothing ->
                         continueAnalysing $
                           over expTokens (++ [t]) .
                           over numberOfParenthesesToClose (+ 1) .
                           over index (+ 1)
                       _ ->
                        Nothing
                 | s == ')' ->
                     case () of
                       () | _numberOfParenthesesToClose state - 1 < 0 ->
                              case () of
                                () | null $ _expTokens state -> Nothing
                                   | otherwise               -> Just (_expTokens state, index')
                          | otherwise ->
                              case lastToken of
                                Just (Identifier _) ->
                                  continueAnalysing $
                                    over expTokens (++ [t]) .
                                    over numberOfParenthesesToClose (subtract 1) .
                                    over index (+ 1)
                                Just (Number _) ->
                                  continueAnalysing $
                                    over expTokens (++ [t]) .
                                    over numberOfParenthesesToClose (subtract 1) .
                                    over index (+ 1)
                                _ ->
                                  Nothing
                 | otherwise ->
                     Nothing

          _ ->
            Nothing
    where
    index' = _index state

    reachedToBottom = index' >= length tokens
    t = tokens !! index'
    lastToken = maybeLast $ _expTokens state

    ignoreAndDoNothing :: AnalyseResult
    ignoreAndDoNothing =
      continueAnalysing $
        over index (+ 1)

    continueAnalysing :: (State -> State) -> AnalyseResult
    continueAnalysing f = analyse $ f state

