module SourceCodeUtilTest
  ( calculateLineTest1
  , calculateLineTest2
  , calculateLineTest3
  , calculateIndexOfLineTest1
  , calculateIndexOfLineTest2
  , calculateIndexOfLineTest3
  ) where

import           SourceCodeUtil

import           Test.HUnit

calculateLineTest1 :: Test
calculateLineTest1 = TestCase (
    assertEqual "calculateLineTest 1"
                (calculateLine 0 "abc")
                1

  )

calculateLineTest2 :: Test
calculateLineTest2 = TestCase (
    assertEqual "calculateLineTest 2"
                (calculateLine 4 "abc\ndef\ngh")
                2
  )

calculateLineTest3 :: Test
calculateLineTest3 = TestCase (
    assertEqual "calculateLineTest 3"
                (calculateLine 12 "abc\n\ndef\n\n\ngh")
                6
  )

calculateIndexOfLineTest1 :: Test
calculateIndexOfLineTest1 = TestCase (
    assertEqual "calculateIndexOfLineTest 1"
                (calculateIndexOfLine 0 "abc")
                1
  )

calculateIndexOfLineTest2 :: Test
calculateIndexOfLineTest2 = TestCase (
    assertEqual "calculateIndexOfLineTest 2"
                (calculateIndexOfLine 4 "abc\ndef\ngh")
                1
  )

calculateIndexOfLineTest3 :: Test
calculateIndexOfLineTest3 = TestCase (
    assertEqual "calculateIndexOfLineTest 3"
                (calculateIndexOfLine 7 "abc\n\ndef\n\n\ngh")
                3
  )
