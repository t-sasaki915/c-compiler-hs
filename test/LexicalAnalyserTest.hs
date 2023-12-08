module LexicalAnalyserTest
  ( lexicalAnalyseTest1
  , lexicalAnalyseTest2
  ) where

import           LexicalAnalyser

import           Test.HUnit

lexicalAnalyseTest1 :: Test
lexicalAnalyseTest1 = TestCase (
    assertEqual "LexicalAnalyseTest 1"
                (lexicalAnalyse sourceCode)
                expected
  )
  where
  sourceCode = concat
    [ "int main (void) {\n"
    , "  return 2;\n"
    , "}"
    ]
  expected =
    [ Keyword "int", Whitespace ' ', Identifier "main", Whitespace ' '
    , Symbol '(', Keyword "void", Symbol ')', Whitespace ' ', Symbol '{'
    , Whitespace ' ', Whitespace ' ', Keyword "return", Whitespace ' '
    , Number "2", Symbol ';', Symbol '}'
    ]

lexicalAnalyseTest2 :: Test
lexicalAnalyseTest2 = TestCase (
    assertEqual "LexicalAnalyseTest 2"
                (lexicalAnalyse sourceCode)
                expected
  )
  where
  sourceCode = "int main(void){return 2;}"
  expected =
    [ Keyword "int", Whitespace ' ', Identifier "main", Symbol '('
    , Keyword "void", Symbol ')', Symbol '{', Keyword "return"
    , Whitespace ' ', Number "2", Symbol ';', Symbol '}'
    ]
