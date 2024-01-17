module ExpressionStructureAnalyser (expressionStructureAnalyse) where

import           ExpressionAnalyser
import           ExpressionStructure
import           Token

type AnalyseResult = Maybe (ExpressionStructure, Int)

expressionStructureAnalyse :: [Token] -> Int -> AnalyseResult
expressionStructureAnalyse rawTokens i =
  case expressionAnalyse rawTokens i of
    Just (tokens, newIndex) ->
      Nothing

    Nothing ->
      Nothing
