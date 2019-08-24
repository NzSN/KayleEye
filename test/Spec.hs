import Test.HUnit

test1 = TestCase (assertEqual "Not Equal" 1 2)
tests = TestList [TestLabel "test1" test1]

main = do
  runTestTT tests
