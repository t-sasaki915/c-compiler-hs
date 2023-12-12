module Util ((!?), isPrefixOf', matchesPerfectly) where

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
