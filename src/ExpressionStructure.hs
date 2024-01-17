module ExpressionStructure (StructureToken (..), ExpressionStructure(..)) where

import           Token

import           Data.List (intercalate)

data StructureToken = NumRef Token
                    | VarRef Token
                    | FunCall Token
                    | Calc Token
                    deriving (Eq, Show)

data ExpressionStructure = Node
  { rootLabel :: StructureToken
  , subForest :: [ExpressionStructure]
  }
  deriving Eq

instance Show ExpressionStructure where
  show (Node root []) = show root
  show (Node root children) =
    show root ++ " [" ++ intercalate ", " (map show children) ++ "]"
