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
   TestCase $
        assertEqual
          "rmDups"
          testDataList
          (rmDups $ testDataList ++ reverse testDataList)

testDataList :: [Int]
testDataList = [1..10]

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
testPPNumber' s = case take 4 s of
  [_,_,_,c] -> assertEqual "C is a comma" c ',' : testPPNumber' (drop 4 s)
  s' -> [assertBool "Does not contain a comma" (',' `notElem` s')]
