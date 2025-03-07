module Test.RAList (testRAList) where

import qualified RAList as L
import qualified Tree as T
import qualified Data.List
import Text.Printf 

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

runFromList :: (Show a, Eq a) => [a] -> TestTree 
runFromList xs = testCase (show xs) $ do 
  let raList = L.fromList xs
  assertBool "RAList is not well-formed" (L.wellFormed raList)

runFetch :: (Show a, Eq a) => [a] -> TestTree
runFetch xs = testCase (show xs) $ do 
    let raList = L.fromList xs 
    mapM_ (go raList) (zip [0..] xs)
  where
    go raList (k, x) =
      L.fetch k raList @?= x

runUpdate :: (Show a, Eq a) => a -> [a] -> TestTree
runUpdate x xs = 
    let raList = L.fromList xs in 
    testGroup (printf "update n %s %s" (show x) (show xs)) $ map (go raList) [0 .. length xs - 1]
  where
    go raList k = testCase (show k) $
      L.update k x raList @?= L.fromList (updateList k x xs)
    updateList k x (h:t)
      | k <= 0 = x : t
      | otherwise = h : updateList (k - 1) x t


testRAList :: TestTree
testRAList =
  testGroup "Random Access List"
    [ testFromList 
    , testFetch
    , testUpdate
    ]

testFromList :: TestTree 
testFromList = 
  testGroup "FromList" $ map runFromList (Data.List.inits [0..10])

testUpdate :: TestTree
testUpdate =
  testGroup "Update" $ map (runUpdate 13) (tail $ Data.List.inits [0..10])

testFetch :: TestTree
testFetch =
  testGroup "Fetch" $ map runFetch $ Data.List.inits [0..10]
