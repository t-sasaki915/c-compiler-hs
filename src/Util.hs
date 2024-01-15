module Util
  ( (!?)
  , isPrefixOf'
  , matchesPerfectly
  , combineList
  , maybeLast
  , calculateLine
  , calculateIndexOfLine
) where

import           Constant         (newLines)

import           Data.List        (findIndices, isPrefixOf)
import           Text.Regex.Posix ((=~))

(!?) :: String -> Int -> Maybe Char
(!?) xs n
  | n < 0          = Nothing
  | n >= length xs = Nothing
  | otherwise      = Just $ xs !! n

dropRight :: Int -> [a] -> [a]
dropRight n xs = reverse $ drop n (reverse xs)

unsnoc :: [a] -> Maybe ([a], a)
unsnoc [] = Nothing
unsnoc xs = Just (dropRight 1 xs, last xs)

isPrefixOf' :: (Eq a) => a -> [a] -> Bool
isPrefixOf' x = isPrefixOf [x]

matchesPerfectly :: String -> String -> Bool
matchesPerfectly regex str = str == ((str =~ regex) :: String)

combineList :: [a] -> [b] -> [(a, b)]
combineList xs ys = combineList' 0 []
  where
  combineList' i combined
    | i >= length xs = combined
    | i >= length ys = combined
    | otherwise = combineList' (i + 1) (combined ++ [(xs !! i, ys !! i)])

maybeLast :: [a] -> Maybe a
maybeLast xs
  | null xs   = Nothing
  | otherwise = Just $ last xs

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

