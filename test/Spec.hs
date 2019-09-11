import Test.HUnit

import LetterBox
import Homer
import Logger
import Modules.ConfigReader

tests = TestList [TestLabel "boxTest" boxTest,
                  TestLabel "homerTest" homerTest,
                  TestLabel "loggerTest" loggerTest,
                  TestLabel "ParserTest" parserTest]

main = do
  runTestTT tests
