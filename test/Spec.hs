import Functions.Application
import Test.HUnit
import Text.Printf

main :: IO ()
main = do
  testCounts <- runTestTT testList
  print testCounts

testList :: Test
testList =
  TestList
    [ testunBreakCharacters,
      testDistanceFrom5,
      testMean
    ]

meanTestCases :: [(Float, [Int])]
meanTestCases = [
    (1.0, replicate 5 1),
    (3.0, [1..5])
  ]

testMean :: Test
testMean = TestList $ map (\(n, ns) -> TestCase $ assertEqual (printf "Mean of %s should equal %d" (show ns) n) n (mean ns)) meanTestCases

testunBreakCharacters :: Test
testunBreakCharacters =
  TestCase $
    assertEqual
      "replaceSpaces should replace all spaces with &nbsp;"
      "Hello,&nbsp;World!"
      (unBreakCharacters "Hello, World!")

testDistanceFrom5 :: Test
testDistanceFrom5 = TestCase $ do
  assertEqual "distanceFrom5 should return 0 for multiples of 5" 0 (distanceFrom5 5)
  assertEqual "distanceFrom5 should return 1 for numbers one less than a multiple of 5" 1 (distanceFrom5 4)
  assertEqual "distanceFrom5 should return 2 for numbers two less than a multiple of 5" 2 (distanceFrom5 3)
  assertEqual "distanceFrom5 should return 2 for numbers three less than a multiple of 5" 2 (distanceFrom5 2)
  assertEqual "distanceFrom5 should return 1 for numbers four less than a multiple of 5" 1 (distanceFrom5 1)
