import Test.HUnit

import LetterBox
import Homer
import Logger
import Time
import Modules.ConfigReader
import KayleDefined
import Notifier
import DoorKeeper

tests = TestList [TestLabel "loggerTest" loggerTest,
                  TestLabel "ParserTest" parserTest,
                  TestLabel "TimeTest" timeTest,
                  TestLabel "DefinedTest" definedTest,
                  --TestLabel "Notifier" notifierTest,
                  TestLabel "DoorKeeper" doorKeeperUnitTest]


main = do
  runTestTT tests
