import           Test.HUnit

import           Functions.Application
import           Functions.Display
import           Functions.Domain

main :: IO ()
main = do
  testCounts <- runTestTT testList
  print testCounts

testList :: Test
testList = TestList [
    testRmDups,
    testPPNumber
  ]

testRmDups :: Test
testRmDups =
  let testDataList = [1..10] :: [Int]
   in TestCase $
        assertEqual
          "rmDups"
          testDataList
          (rmDups $ testDataList ++ reverse testDataList)

testPPNumber :: Test
testPPNumber = TestList
             $ map (
              TestList
              . map TestCase
              . testPPNumber'
              . reverse
              . ppNumber
              . (10^)
              ) ([1..100] :: [Integer])

testPPNumber' :: String -> [Assertion]
testPPNumber' s =
  let (thisString, theRest) = splitAt 4 s
  in case thisString of
    [_,_,_,c] -> assertEqual "C is a comma" c ',' : testPPNumber' theRest
    s'        -> [assertBool "Does not contain a comma" (',' `notElem` s')]
