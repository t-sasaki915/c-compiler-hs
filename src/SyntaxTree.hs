module SyntaxTree (Syntax (..), SyntaxTree (..)) where

import           Token

import           Data.List (intercalate)

data Syntax = Program
            | DeclarationList
            | Declaration
            | TypeSpecifier Token
            deriving (Eq, Show)

data SyntaxTree = Node
  { rootLabel :: Syntax
  , subForest :: [SyntaxTree]
  }
  deriving Eq

instance Show SyntaxTree where
  show (Node root []) = show root
  show (Node root children) =
    show root ++ " [" ++ intercalate ", " (map show children) ++ "]"
