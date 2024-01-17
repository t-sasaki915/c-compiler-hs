module ExpressionAnalyserTest
  ( expressionAnalyseTest1
  , expressionAnalyseTest2
  , expressionAnalyseTest3
  , expressionAnalyseTest4
  , expressionAnalyseTest5
  , expressionAnalyseTest6
  , expressionAnalyseTest7
  , expressionAnalyseTest8
  , expressionAnalyseTest9
  ) where

import           ExpressionAnalyser
import           Token

import           Test.HUnit

expressionAnalyseTest1 :: Test
expressionAnalyseTest1 = TestCase (
    assertEqual "expressionAnalyseTest1"
                (expressionAnalyse tokens 0)
                expected
  )
  where
  tokens = [ Number "0", Symbol ';' ]
  expected = Just ([ Number "0" ], 1)

expressionAnalyseTest2 :: Test
expressionAnalyseTest2 = TestCase (
    assertEqual "expressionAnalyseTest2"
                (expressionAnalyse tokens 0)
                expected
  )
  where
  tokens = [ Identifier "x", Symbol ';' ]
  expected = Just ([ Identifier "x" ], 1)

expressionAnalyseTest3 :: Test
expressionAnalyseTest3 = TestCase (
    assertEqual "expressionAnalyseTest3"
                (expressionAnalyse tokens 0)
                expected
  )
  where
  tokens = [ Symbol '(', Identifier "x", Symbol ')', Symbol ';' ]
  expected = Just ([ Symbol '(', Identifier "x", Symbol ')' ], 3)

expressionAnalyseTest4 :: Test
expressionAnalyseTest4 = TestCase (
    assertEqual "expressionAnalyseTest4"
                (expressionAnalyse tokens 0)
                expected
  )
  where
  tokens = [ Number "1", Symbol '+', Number "1", Symbol ';' ]
  expected = Just ([ Number "1", Symbol '+', Number "1" ], 3)

expressionAnalyseTest5 :: Test
expressionAnalyseTest5 = TestCase (
    assertEqual "expressionAnalyseTest5"
                (expressionAnalyse tokens 0)
                expected
  )
  where
  tokens = [ Number "1", Symbol '+', Symbol '(', Identifier "x", Symbol '*', Number "2", Symbol ')', Symbol ';' ]
  expected = Just ([ Number "1", Symbol '+', Symbol '(', Identifier "x", Symbol '*', Number "2", Symbol ')' ], 7)

expressionAnalyseTest6 :: Test
expressionAnalyseTest6 = TestCase (
    assertEqual "expressionAnalyseTest6"
                (expressionAnalyse tokens 0)
                expected
  )
  where
  tokens = [ Symbol '(', Number "1", Symbol '+', Symbol '(', Number "1", Symbol '+', Number "1", Symbol ')', Symbol ')', Symbol '*', Number "0", Symbol ';' ]
  expected = Just ([ Symbol '(', Number "1", Symbol '+', Symbol '(', Number "1", Symbol '+', Number "1", Symbol ')', Symbol ')', Symbol '*', Number "0" ], 11)

expressionAnalyseTest7 :: Test
expressionAnalyseTest7 = TestCase (
    assertEqual "expressionAnalyseTest7"
                (expressionAnalyse tokens 0)
                expected
  )
  where
  tokens = [ Identifier "add", Symbol '(', Number "1", Symbol ',', Number "1", Symbol ')', Symbol ';' ]
  expected = Just ([ Identifier "add", Symbol '(', Number "1", Symbol ',', Number "1", Symbol ')' ], 6)

expressionAnalyseTest8 :: Test
expressionAnalyseTest8 = TestCase (
    assertEqual "expressionAnalyseTest8"
                (expressionAnalyse tokens 0)
                expected
  )
  where
  tokens = [ Identifier "add", Symbol '(', Identifier "add", Symbol '(', Number "1", Symbol ',', Number "1", Symbol ')', Symbol ',', Identifier "x", Symbol ')', Symbol ';' ]
  expected = Just ([ Identifier "add", Symbol '(', Identifier "add", Symbol '(', Number "1", Symbol ',', Number "1", Symbol ')', Symbol ',', Identifier "x", Symbol ')' ], 11)

expressionAnalyseTest9 :: Test
expressionAnalyseTest9 = TestCase (
    assertEqual "expressionAnalyseTest9"
                (expressionAnalyse tokens 0)
                expected
  )
  where
  tokens = [ Identifier "A", Symbol '*', Identifier "sin", Symbol '(', Number "2", Symbol '*', Identifier "getPi", Symbol '(', Symbol ')', Symbol '*', Identifier "f", Symbol '*', Identifier "t", Symbol ')', Symbol ';' ]
  expected = Just ([ Identifier "A", Symbol '*', Identifier "sin", Symbol '(', Number "2", Symbol '*', Identifier "getPi", Symbol '(', Symbol ')', Symbol '*', Identifier "f", Symbol '*', Identifier "t", Symbol ')' ], 14)
