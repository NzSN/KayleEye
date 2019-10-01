import Test.HUnit

import LetterBox
import Homer
import Logger
import Time
import Modules.ConfigReader

tests = TestList [TestLabel "homerTest" homerTest,
                  TestLabel "loggerTest" loggerTest,
                  TestLabel "ParserTest" parserTest,
                  TestLabel "TimeTest" timeTest]

main = do
  runTestTT tests
