import Test.HUnit

import LetterBox
import Homer
import Logger
import Time
import Modules.ConfigReader
import KayleDefined

tests = TestList [TestLabel "loggerTest" loggerTest,
                  TestLabel "ParserTest" parserTest,
                  TestLabel "TimeTest" timeTest,
                  TestLabel "DefinedTest" definedTest]

main = do
  runTestTT tests
