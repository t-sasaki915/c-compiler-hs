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

import           Data.Either        (fromRight)

import           ExpressionAnalyser
import           LexicalAnalyser
import           Token

import           Test.HUnit

expressionAnalyseTest1 :: Test
expressionAnalyseTest1 = TestCase (
    assertEqual "expressionAnalyseTest1"
                (expressionAnalyse (fromRight [] (lexicalAnalyse expression)) 0)
                expected
  )
  where
  expression = "0;"
  expected = Just ([ Number "0" ], 1)

expressionAnalyseTest2 :: Test
expressionAnalyseTest2 = TestCase (
    assertEqual "expressionAnalyseTest2"
                (expressionAnalyse (fromRight [] (lexicalAnalyse expression)) 0)
                expected
  )
  where
  expression = "x;"
  expected = Just ([ Identifier "x" ], 1)

expressionAnalyseTest3 :: Test
expressionAnalyseTest3 = TestCase (
    assertEqual "expressionAnalyseTest3"
                (expressionAnalyse (fromRight [] (lexicalAnalyse expression)) 0)
                expected
  )
  where
  expression = "(x);"
  expected = Just ([ Symbol '(', Identifier "x", Symbol ')' ], 3)

expressionAnalyseTest4 :: Test
expressionAnalyseTest4 = TestCase (
    assertEqual "expressionAnalyseTest4"
                (expressionAnalyse (fromRight [] (lexicalAnalyse expression)) 0)
                expected
  )
  where
  expression = "1 + 1;"
  expected = Just ([ Number "1", Symbol '+', Number "1" ], 5)

expressionAnalyseTest5 :: Test
expressionAnalyseTest5 = TestCase (
    assertEqual "expressionAnalyseTest5"
                (expressionAnalyse (fromRight [] (lexicalAnalyse expression)) 0)
                expected
  )
  where
  expression = "1 + (x * 2);"
  expected = Just ([ Number "1", Symbol '+', Symbol '(', Identifier "x", Symbol '*', Number "2", Symbol ')' ], 11)

expressionAnalyseTest6 :: Test
expressionAnalyseTest6 = TestCase (
    assertEqual "expressionAnalyseTest6"
                (expressionAnalyse (fromRight [] (lexicalAnalyse expression)) 0)
                expected
  )
  where
  expression = "(1 + (1 + 1)) * 0;"
  expected = Just ([ Symbol '(', Number "1", Symbol '+', Symbol '(', Number "1", Symbol '+', Number "1", Symbol ')', Symbol ')', Symbol '*', Number "0" ], 17)

expressionAnalyseTest7 :: Test
expressionAnalyseTest7 = TestCase (
    assertEqual "expressionAnalyseTest7"
                (expressionAnalyse (fromRight [] (lexicalAnalyse expression)) 0)
                expected
  )
  where
  expression = "add(1, 1);"
  expected = Just ([ Identifier "add", Symbol '(', Number "1", Symbol ',', Number "1", Symbol ')' ], 7)

expressionAnalyseTest8 :: Test
expressionAnalyseTest8 = TestCase (
    assertEqual "expressionAnalyseTest8"
                (expressionAnalyse (fromRight [] (lexicalAnalyse expression)) 0)
                expected
  )
  where
  expression = "add(add(1, 1), x);"
  expected = Just ([ Identifier "add", Symbol '(', Identifier "add", Symbol '(', Number "1", Symbol ',', Number "1", Symbol ')', Symbol ',', Identifier "x", Symbol ')' ], 13)

expressionAnalyseTest9 :: Test
expressionAnalyseTest9 = TestCase (
    assertEqual "expressionAnalyseTest9"
                (expressionAnalyse (fromRight [] (lexicalAnalyse expression)) 0)
                expected
  )
  where
  expression = "A * sin(2 * getPi() * f * t);"
  expected = Just ([ Identifier "A", Symbol '*', Identifier "sin", Symbol '(', Number "2", Symbol '*', Identifier "getPi", Symbol '(', Symbol ')', Symbol '*', Identifier "f", Symbol '*', Identifier "t", Symbol ')' ], 22)
