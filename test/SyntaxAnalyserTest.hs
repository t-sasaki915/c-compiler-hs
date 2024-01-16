module SyntaxAnalyserTest
  ( syntaxAnalyseTest1
  , syntaxAnalyseTest2
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
      [ Node DefinitionList
          [ Node (FunDefinition (Keyword "int") (Identifier "main")) []
          , Node (FunDefinition (Keyword "void") (Identifier "aaa")) []
          , Node (FunDefinition (Keyword "void") (Identifier "bbb"))
              [ Node (VarDefinition (Keyword "int") (Identifier "a")) []
              ]
          , Node (FunDefinition (Keyword "int") (Identifier "ccc"))
              [ Node (VarDefinition (Keyword "int") (Identifier "a")) []
              , Node (VarDefinition (Keyword "int") (Identifier "b")) []
              ]
          , Node (FunDefinition (Keyword "int") (Identifier "ddd")) []
          ]
      ]

syntaxAnalyseTest2 :: Test
syntaxAnalyseTest2 = TestCase (
    assertEqual "syntaxAnalyseTest 2"
                (syntaxAnalyse $ fromRight [] (lexicalAnalyse sourceCode))
                expected
  )
  where
  sourceCode = concat
    [ "int main (void) {\n"
    , "  return 0;\n"
    , "}\n"
    , "void nothing() {\n"
    , "  return;\n"
    , "}\n"
    , "int identity (int x) {\n"
    , "  return x;\n"
    , "}"
    ]
  expected = Right $
    Node Program
      [ Node DefinitionList
          [ Node (FunDefinition (Keyword "int") (Identifier "main"))
              [ Node (Operation (Keyword "return"))
                  [ Node (Expression [Number "0"]) []
                  ]
              ]
          , Node (FunDefinition (Keyword "void") (Identifier "nothing"))
              [ Node (Operation (Keyword "return")) []
              ]
          , Node (FunDefinition (Keyword "int") (Identifier "identity"))
              [ Node (VarDefinition (Keyword "int") (Identifier "x")) []
              , Node (Operation (Keyword "return"))
                  [ Node (Expression [Identifier "x"]) []
                  ]
              ]
          ]
      ]
