module SyntaxAnalyserTest
  ( syntaxAnalyseTest1
  , syntaxAnalyseTest2
  , syntaxAnalyseTest3
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

syntaxAnalyseTest3 :: Test
syntaxAnalyseTest3 = TestCase (
    assertEqual "syntaxAnalyseTest3"
                (syntaxAnalyse $ fromRight [] (lexicalAnalyse sourceCode))
                expected
  )
  where
  sourceCode = concat
    [ "int VERSION = 1;\n"
    , "int getNextVersion() {\n"
    , "  return VERSION + 1;\n"
    , "}\n"
    , "int add(int a, int b) {\n"
    , "  return a + b;\n"
    , "}\n"
    , "int average(int a, int b) {\n"
    , "  return (a + b) / 2;\n"
    , "}\n"
    ]
  expected = Right $
    Node Program
      [ Node DefinitionList
          [ Node (VarDefinition (Keyword "int") (Identifier "VERSION"))
              [ Node (Expression [Number "1"]) []
              ]
          , Node (FunDefinition (Keyword "int") (Identifier "getNextVersion"))
              [ Node (Operation (Keyword "return"))
                  [ Node (Expression [Identifier "VERSION", Symbol '+', Number "1"]) []
                  ]
              ]
          , Node (FunDefinition (Keyword "int") (Identifier "add"))
              [ Node (VarDefinition (Keyword "int") (Identifier "a")) []
              , Node (VarDefinition (Keyword "int") (Identifier "b")) []
              , Node (Operation (Keyword "return"))
                  [ Node (Expression [Identifier "a", Symbol '+', Identifier "b"]) []
                  ]
              ]
          , Node (FunDefinition (Keyword "int") (Identifier "average"))
              [ Node (VarDefinition (Keyword "int") (Identifier "a")) []
              , Node (VarDefinition (Keyword "int") (Identifier "b")) []
              , Node (Operation (Keyword "return"))
                  [ Node (Expression [Symbol '(', Identifier "a", Symbol '+', Identifier "b", Symbol ')', Symbol '/', Number "2"]) []
                  ]
              ]
          ]
      ]
