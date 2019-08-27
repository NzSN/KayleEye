import Test.HUnit

import LetterBox
import Homer
import Logger

tests = TestList [TestLabel "boxTest" boxTest,
                  TestLabel "homerTest" homerTest,
                  TestLabel "loggerTest" loggerTest]

main = do
  runTestTT tests
