import           Test.HUnit

import           Functions.Application

main :: IO ()
main = do
  testCounts <- runTestTT testList
  print testCounts

testList :: Test
testList = TestList [
    testRmDups
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
