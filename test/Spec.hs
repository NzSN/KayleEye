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
import RepoOps

tests = TestList [TestLabel "loggerTest" loggerTest,
                  TestLabel "ParserTest" parserTest,
                  TestLabel "TimeTest" timeTest,
                  TestLabel "DefinedTest" definedTest,
                  --TestLabel "Notifier" notifierTest,
                  TestLabel "RoomTest" roomUnitTest,
                  TestLabel "HomerTest" homerTest,
                  TestLabel "EncryptTest" encryptTest,
                  TestLabel "RepoTest" repoTest]

main = do
  runTestTT tests
