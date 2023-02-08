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

testMeanCases :: [(Float, [Int])]
testMeanCases =
  [ (1.0, replicate 5 1),
    (3.0, [1 .. 5])
  ]

testMean :: Test
testMean =
  TestList $
    map
      ( \(n, ns) ->
          TestCase $
            assertEqual
              (printf "Mean of %s should equal %d" (show ns) n)
              n
              (mean ns)
      )
      testMeanCases

testunBreakCharacters :: Test
testunBreakCharacters =
  TestCase $
    assertEqual
      "replaceSpaces should replace all spaces with &nbsp;"
      "Hello,&nbsp;World!"
      (unBreakCharacters "Hello, World!")

-- (Result, testCase)
testDistanceFrom5Cases :: [(Int, Int)]
testDistanceFrom5Cases =
  [ (5, 0),
    (4, 1),
    (3, 2),
    (2, 2),
    (1, 1)
  ]

testDistanceFrom5 :: Test
testDistanceFrom5 =
  TestList $
    map
      ( \(testCase, result) ->
          TestCase $
            assertEqual
              (printf "distanceFrom5 should return %d for multiples of %d" result testCase)
              result
              (distanceFrom5 testCase)
      )
      testDistanceFrom5Cases
