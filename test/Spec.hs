import Functions.Application
import Test.HUnit
import Text.Printf
import Types.Position

main :: IO Counts
main = runTestTT testList

type TestInputs a b = [(a, b)]

type TestCases a b = (String, a -> b, TestInputs a b)

testList :: Test
testList =
  TestList
    [ testunBreakCharacters,
      testDistanceFrom5,
      testMean,
      testMaximumValues,
      testPrintThingsWithAnd,
      testEnumPositionData
    ]

testTestCases :: (Eq b, Show a, Show b) => TestCases a b -> Test
testTestCases (str, fn, testCases) =
  TestList $
    map
      ( \(testCase, result) ->
          TestCase $
            assertEqual
              (printf str (show testCase) (show result))
              result
              (fn testCase)
      )
      testCases

testMeanCases :: TestCases [Int] Float
testMeanCases =
  ( "Mean of %s should equal %d",
    mean,
    [ (replicate 5 1, 1.0),
      ([1 .. 5], 3.0),
      ([1 .. 10], 5.5)
    ]
  )

testMean :: Test
testMean = testTestCases testMeanCases

testunBreakCharacters :: Test
testunBreakCharacters =
  TestCase $
    assertEqual
      "replaceSpaces should replace all spaces with &nbsp;"
      "Hello,&nbsp;World!"
      (unBreakCharacters "Hello, World!")

-- (Result, testCase)
testDistanceFrom5Cases :: TestCases Int Int
testDistanceFrom5Cases =
  ( "distanceFrom5 should return %d for multiples of %d",
    distanceFrom5,
    [ (5, 0),
      (4, 1),
      (3, 2),
      (2, 2),
      (1, 1)
    ]
  )

testDistanceFrom5 :: Test
testDistanceFrom5 = testTestCases testDistanceFrom5Cases

testMaximumValuesCases :: TestCases [Int] [Int]
testMaximumValuesCases =
  ( "maximumValues should return %s for %s",
    maximumValues,
    [ ([1, 1, 5, 3, 4, 5], [5, 5])
    ]
  )

testMaximumValues :: Test
testMaximumValues = testTestCases testMaximumValuesCases

testPrintThingsWithAndCases :: TestCases [String] String
testPrintThingsWithAndCases =
  ( "printThingsWithAnd should return %s for %s",
    printThingsWithAnd,
    [ (map show ['a'], "'a'"),
      (map show ['a', 'b'], "'a' and 'b'"),
      (map show ['a' .. 'c'], "'a', 'b', and 'c'")
    ]
  )

testPrintThingsWithAnd :: Test
testPrintThingsWithAnd = testTestCases testPrintThingsWithAndCases

testEnumPositionData :: Test
testEnumPositionData = TestList $ map (testEnumPositionData' . (+ (-1))) [1 .. (length positionDatas)]

testEnumPositionData' :: Int -> Test
testEnumPositionData' n =
  let toPositionData = toEnum n :: Position
   in TestCase $
        assertEqual
          (printf "%s remains the same when taken to and from Position" n)
          n
          (fromEnum toPositionData)
