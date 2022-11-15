import Test.HUnit

import Functions.Application

main :: IO ()
main = do
  counts <- runTestTT $ TestList [testRmDups]
  print counts

testRmDups :: Test
testRmDups =
  let forwardList = [1..10]
      backwardList = reverse forwardList
   in TestCase $
        assertEqual
          "rmDups"
          forwardList
          (rmDups $ forwardList ++ backwardList)