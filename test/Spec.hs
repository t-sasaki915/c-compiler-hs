import           System.Exit         (exitFailure, exitSuccess)
import           Test.HUnit

import           LexicalAnalyserTest

main :: IO ()
main = do
  let
    tests = TestList
      [ TestLabel "LexicalAnalyseTest 1" lexicalAnalyseTest1
      , TestLabel "LexicalAnalyseTest 2" lexicalAnalyseTest2
      ]
  result <- runTestTT tests
  if failures result /= 0 || errors result /= 0 then
    exitFailure

  else
    exitSuccess

