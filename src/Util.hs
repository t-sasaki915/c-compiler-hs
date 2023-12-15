module Util ((!?), isPrefixOf', matchesPerfectly, combineList) where

import           Data.List        (isPrefixOf)
import           Text.Regex.Posix ((=~))

(!?) :: String -> Int -> Maybe Char
(!?) xs n
  | n < 0          = Nothing
  | n >= length xs = Nothing
  | otherwise      = Just $ xs !! n

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
