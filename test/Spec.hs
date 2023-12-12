import           System.Exit         (exitFailure, exitSuccess)
import           Test.HUnit

import           LexicalAnalyserTest
import           SourceCodeUtilTest

main :: IO ()
main = do
  let
    tests = TestList
      [ TestLabel "lexicalAnalyseTest 1"       lexicalAnalyseTest1
      , TestLabel "lexicalAnalyseTest 2"       lexicalAnalyseTest2
      , TestLabel "lexicalAnalyseTest 3"       lexicalAnalyseTest3
      , TestLabel "lexicalAnalyseTest 4"       lexicalAnalyseTest4
      , TestLabel "lexicalAnalyseTest 5"       lexicalAnalyseTest5
      , TestLabel "lexicalAnalyseTest 6"       lexicalAnalyseTest6
      , TestLabel "lexicalAnalyseTest 7"       lexicalAnalyseTest7
      , TestLabel "calculateLineTest 1"        calculateLineTest1
      , TestLabel "calculateLineTest 2"        calculateLineTest2
      , TestLabel "calculateLineTest 3"        calculateLineTest3
      , TestLabel "calculateIndexOfLineTest 1" calculateIndexOfLineTest1
      , TestLabel "calculateIndexOfLineTest 2" calculateIndexOfLineTest2
      , TestLabel "calculateIndexOfLineTest 3" calculateIndexOfLineTest3
      ]
  result <- runTestTT tests
  if failures result /= 0 || errors result /= 0 then
    exitFailure

  else
    exitSuccess

