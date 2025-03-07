{-# LANGUAGE TypeApplications #-}
module Test.List (testList) where 

import Test.Tasty.HUnit ( (@?=), testCase )
import Test.Tasty ( testGroup, TestTree )

import Data.List (sort)

import List ( perms, collapse )

testPerms :: TestTree 
testPerms = 
    testGroup "permutations" 
      [ testCase "perms [] == []" $ perms @Int [] @?= [[]] 
      , testCase "perms [13] == [[13]]" $ perms [13] @?= [[13]]
      , testCase "perms [1,2] == [[1,2], [2,1]]" $ sort (perms [1,2]) @?= sort [[1,2], [2,1]]
      , testCase "perms [1,2,3] == [[1,2,3], [1,3,2], [2,1,3], [2,3,1], [3,1,2], [3,2,1]]" $ sort (perms [1,2,3]) @?= sort [[1,2,3], [1,3,2], [2,1,3], [2,3,1], [3,1,2], [3,2,1]]
      , testCase "length (perms [1,1,1]) = 6" $ length (perms [1,1,1]) @?= 6
      , testCase "length (perms [1..6]) = 6!" $ let n = 6 in length (perms [1..n]) @?= fact n
      ]
  where 
    fact = go 1 where go acc n = if n <= 1 then acc else go (n*acc) (n-1)

testCollapse :: TestTree 
testCollapse = 
    testGroup "collapse"
      [ testCase "[] => []" $ collapse [] @?= []
      , testCase "[[1], [-3], [2,4]] => [1]" $ collapse [[1], [-3], [2,4]] @?= [1]
      , testCase "[[-2,1], [-3], [2,4]] => [-2, 1, -3, 2, 4]" $ collapse [[-2,1], [-3], [2,4]] @?= [-2, 1, -3, 2, 4]
      , testCase "[[-2,1], [3], [2,4]] => [-2,1,3]" $ collapse [[-2,1], [3], [2,4]] @?= [-2,1,3]
      , testCase "[[-1,-2], [-3], [-4, -5]] => [-1,-2,-3,-4,-5]" $ collapse [[-1,-2], [-3], [-4, -5]] @?= [-1,-2,-3,-4,-5]
      ]

testList :: TestTree 
testList = 
  testGroup "List" [testPerms, testCollapse]