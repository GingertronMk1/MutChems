import Functions.Application
import Test.HUnit
import Text.Printf

main :: IO Counts
main = runTestTT testList

type TestInput a b = (a, b)

testList :: Test
testList =
  TestList
    [ testunBreakCharacters,
      testDistanceFrom5,
      testMean,
      testMaximumValues
    ]

testMeanCases :: [TestInput [Int] Float]
testMeanCases =
  [ (replicate 5 1, 1.0),
    ([1 .. 5], 3.0),
    ([1 .. 10], 5.5)
  ]

testMean :: Test
testMean =
  TestList $
    map
      ( \(testCase, result) ->
          TestCase $
            assertEqual
              (printf "Mean of %s should equal %d" (show testCase) result)
              result
              (mean testCase)
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
testDistanceFrom5Cases :: [TestInput Int Int]
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

testMaximumValuesCases :: [TestInput [Int] [Int]]
testMaximumValuesCases =
  [ ([1, 1, 5, 3, 4, 5], [5, 5])
  ]

testMaximumValues :: Test
testMaximumValues =
  TestList $
    map
      ( \(testCase, result) ->
          TestCase $
            assertEqual
              (printf "maximumValues should return %s for %s" (show result) (show testCase))
              result
              (maximumValues testCase)
      )
      testMaximumValuesCases
