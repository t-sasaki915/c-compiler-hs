module SourceCodeUtil (calculateLine, calculateIndexOfLine) where

import           Constant

import           Data.List       (findIndices)
import           Data.List.Extra (unsnoc)

dropRight :: Int -> [a] -> [a]
dropRight n xs = reverse $ drop n (reverse xs)

calculateLine :: Int -> String -> Int
calculateLine n sourceCode =
  (+) 1 $
    length $
    filter (`elem` newLines) $
    dropRight (length sourceCode - n) sourceCode

calculateIndexOfLine :: Int -> String -> Int
calculateIndexOfLine n sourceCode =
  case unsnoc $
         findIndices (`elem` newLines) $
         dropRight (length sourceCode - n) sourceCode of
    Just (_, ln) -> n - ln
    Nothing      -> 1 + n
