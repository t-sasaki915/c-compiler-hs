module LexicalAnalyserTest
  ( lexicalAnalyseTest1
  , lexicalAnalyseTest2
  , lexicalAnalyseTest3
  ) where

import           LexicalAnalyser
import           Token

import           Test.HUnit

lexicalAnalyseTest1 :: Test
lexicalAnalyseTest1 = TestCase (
    assertEqual "lexicalAnalyseTest 1"
                (lexicalAnalyse sourceCode)
                expected
  )
  where
  sourceCode = concat
    [ "int main (void) {\n"
    , "  return 2;\n"
    , "}"
    ]
  expected = Right
    [ Keyword "int", Whitespace ' ', Identifier "main", Whitespace ' '
    , Symbol '(', Keyword "void", Symbol ')', Whitespace ' ', Symbol '{'
    , NewLine '\n', Whitespace ' ', Whitespace ' ', Keyword "return"
    , Whitespace ' ', Number "2", Symbol ';', NewLine '\n', Symbol '}'
    ]

lexicalAnalyseTest2 :: Test
lexicalAnalyseTest2 = TestCase (
    assertEqual "lexicalAnalyseTest 2"
                (lexicalAnalyse sourceCode)
                expected
  )
  where
  sourceCode = "int main(void){return 2;}"
  expected = Right
    [ Keyword "int", Whitespace ' ', Identifier "main", Symbol '('
    , Keyword "void", Symbol ')', Symbol '{', Keyword "return"
    , Whitespace ' ', Number "2", Symbol ';', Symbol '}'
    ]

lexicalAnalyseTest3 :: Test
lexicalAnalyseTest3 = TestCase (
    assertEqual "lexicalAnalyseTest 3"
                (lexicalAnalyse sourceCode)
                expected
  )
  where
  sourceCode = concat
    [ "// Comment 1\n"
    , "int main (void) //Comment2\n"
    , "{\n"
    , "  // Comment 3!!!!\n"
    , "  return 0;\n"
    , "}"
    ]
  expected = Right
    [ Comment "// Comment 1", NewLine '\n', Keyword "int", Whitespace ' ', Identifier "main"
    , Whitespace ' ', Symbol '(', Keyword "void", Symbol ')', Whitespace ' ', Comment "//Comment2"
    , NewLine '\n', Symbol '{', NewLine '\n', Whitespace ' ', Whitespace ' ', Comment "// Comment 3!!!!"
    , NewLine '\n', Whitespace ' ', Whitespace ' ', Keyword "return", Whitespace ' ', Number "0"
    , Symbol ';', NewLine '\n', Symbol '}'
    ]
