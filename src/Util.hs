module Util ((!?), isPrefixOf') where

import           Data.List (isPrefixOf)

(!?) :: String -> Int -> Maybe Char
(!?) xs n
  | n < 0          = Nothing
  | n >= length xs = Nothing
  | otherwise      = Just $ xs !! n

isPrefixOf' :: (Eq a) => a -> [a] -> Bool
isPrefixOf' x = isPrefixOf [x]
