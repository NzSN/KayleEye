import Test.HUnit

import LetterBox
import Homer
import Logger
import Time
import Modules.ConfigReader
import KayleDefined
import Notifier
import DoorKeeper
import Room
import Encrypt

tests = TestList [TestLabel "loggerTest" loggerTest,
                  TestLabel "ParserTest" parserTest,
                  TestLabel "TimeTest" timeTest,
                  TestLabel "DefinedTest" definedTest,
                  --TestLabel "Notifier" notifierTest,
                  TestLabel "RoomTest" roomUnitTest,
                  TestLabel "HomerTest" homerTest,
                  TestLabel "EncryptTest" encryptTest]

main = do
  runTestTT tests
