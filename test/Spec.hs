import Test.HUnit

import Functions.Application

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
  let forwardList = [1..10] :: [Int]
      backwardList = reverse forwardList :: [Int]
   in TestCase $
        assertEqual
          "rmDups"
          forwardList
          (rmDups $ forwardList ++ backwardList)