module SyntaxTree (TreeToken (..), SyntaxTree (..)) where

import           Token

import           Data.List (intercalate)

data TreeToken = Program
               | DefinitionList
               | FunDefinition Token Token
               | VarDefinition Token Token
               | Expression [Token]
               | Return
               | VarReassign Token
               deriving (Eq, Show)

data SyntaxTree = Node
  { rootLabel :: TreeToken
  , subForest :: [SyntaxTree]
  }
  deriving Eq

instance Show SyntaxTree where
  show (Node root []) = show root
  show (Node root children) =
    show root ++ " [" ++ intercalate ", " (map show children) ++ "]"
