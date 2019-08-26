import Test.HUnit

import LetterBox
import Homer

tests = TestList [TestLabel "boxTest" boxTest,
                  TestLabel "homerTest" homerTest]

main = do
  runTestTT tests
