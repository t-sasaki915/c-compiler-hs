module SyntaxAnalyserTest
  ( syntaxAnalyseTest1
  ) where

import           Data.Either     (fromRight)
import           Test.HUnit

import           LexicalAnalyser
import           SyntaxAnalyser
import           SyntaxTree
import           Token

syntaxAnalyseTest1 :: Test
syntaxAnalyseTest1 = TestCase (
    assertEqual "syntaxAnalyserTest 1"
                (syntaxAnalyse $ fromRight [] (lexicalAnalyse sourceCode))
                expected
  )
  where
  sourceCode = concat
    [ "int main (void) {}\n"
    , "void aaa (void) {}\n"
    , "void bbb (int a) {}\n"
    , "int ccc (int a, int b) {}\n"
    , "int ddd () {}"
    ]
  expected = Right $
    Node Program
      [ Node DeclarationList
          [ Node Declaration
              [ Node (TypeSpecifier (Keyword "int")) []
              , Node (DeclarationLabel (Identifier "main")) []
              ]
          , Node Declaration
              [ Node (TypeSpecifier (Keyword "void")) []
              , Node (DeclarationLabel (Identifier "aaa")) []
              ]
          , Node Declaration
              [ Node (TypeSpecifier (Keyword "void")) []
              , Node (DeclarationLabel (Identifier "bbb")) []
              , Node DeclarationArgument
                  [ Node (TypeSpecifier (Keyword "int")) []
                  , Node (DeclarationLabel (Identifier "a")) []
                  ]
              ]
          , Node Declaration
              [ Node (TypeSpecifier (Keyword "int")) []
              , Node (DeclarationLabel (Identifier "ccc")) []
              , Node DeclarationArgument
                  [ Node (TypeSpecifier (Keyword "int")) []
                  , Node (DeclarationLabel (Identifier "a")) []
                  ]
              , Node DeclarationArgument
                  [ Node (TypeSpecifier (Keyword "int")) []
                  , Node (DeclarationLabel (Identifier "b")) []
                  ]
              ]
          , Node Declaration
              [ Node (TypeSpecifier (Keyword "int")) []
              , Node (DeclarationLabel (Identifier "ddd")) []
              ]
          ]
      ]
