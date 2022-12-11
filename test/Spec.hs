import           Test.HUnit

import           Functions.Application

main :: IO ()
main = do
  testCounts <- runTestTT testList
  print testCounts

testList :: Test
testList = TestList [
    testUnbreakSpaces,
    testDistanceFrom5
  ]

testUnbreakSpaces :: Test
testUnbreakSpaces = TestCase $ assertEqual
  "replaceSpaces should replace all spaces with &nbsp;"
  "Hello,&nbsp;World!"
  (unBreakSpaces "Hello, World!")

testDistanceFrom5 :: Test
testDistanceFrom5 = TestCase $ do
  assertEqual "distanceFrom5 should return 0 for multiples of 5" 0 (distanceFrom5 5)
  assertEqual "distanceFrom5 should return 1 for numbers one less than a multiple of 5" 1 (distanceFrom5 4)
  assertEqual "distanceFrom5 should return 2 for numbers two less than a multiple of 5" 2 (distanceFrom5 3)
  assertEqual "distanceFrom5 should return 2 for numbers three less than a multiple of 5" 2 (distanceFrom5 2)
  assertEqual "distanceFrom5 should return 1 for numbers four less than a multiple of 5" 1 (distanceFrom5 1)
