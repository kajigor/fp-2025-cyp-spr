{-# LANGUAGE TypeApplications #-}
module Test.Binary (testBinary) where 

import Test.Tasty.HUnit ( (@?=), testCase, assertBool )
import Test.Tasty ( testGroup, TestTree )

import Data.Maybe ( isNothing, isJust )

import Binary

testBinary :: TestTree 
testBinary = 
    testGroup "binary" 
      [ testNums 
      , testValidation
      , testFromTo 
      , testToFrom 
      ]
  where 
    testNums = 
      testCase "nums" $ take 100 nums @?= [[0], [1], [0, 1], [1, 1], [0, 0, 1], [1, 0, 1], [0, 1, 1], [1, 1, 1], [0, 0, 0, 1], [1, 0, 0, 1], [0, 1, 0, 1], [1, 1, 0, 1], [0, 0, 1, 1], [1, 0, 1, 1], [0, 1, 1, 1], [1, 1, 1, 1], [0, 0, 0, 0, 1], [1, 0, 0, 0, 1], [0, 1, 0, 0, 1], [1, 1, 0, 0, 1], [0, 0, 1, 0, 1], [1, 0, 1, 0, 1], [0, 1, 1, 0, 1], [1, 1, 1, 0, 1], [0, 0, 0, 1, 1], [1, 0, 0, 1, 1], [0, 1, 0, 1, 1], [1, 1, 0, 1, 1], [0, 0, 1, 1, 1], [1, 0, 1, 1, 1], [0, 1, 1, 1, 1], [1, 1, 1, 1, 1], [0, 0, 0, 0, 0, 1], [1, 0, 0, 0, 0, 1], [0, 1, 0, 0, 0, 1], [1, 1, 0, 0, 0, 1], [0, 0, 1, 0, 0, 1], [1, 0, 1, 0, 0, 1], [0, 1, 1, 0, 0, 1], [1, 1, 1, 0, 0, 1], [0, 0, 0, 1, 0, 1], [1, 0, 0, 1, 0, 1], [0, 1, 0, 1, 0, 1], [1, 1, 0, 1, 0, 1], [0, 0, 1, 1, 0, 1], [1, 0, 1, 1, 0, 1], [0, 1, 1, 1, 0, 1], [1, 1, 1, 1, 0, 1], [0, 0, 0, 0, 1, 1], [1, 0, 0, 0, 1, 1], [0, 1, 0, 0, 1, 1], [1, 1, 0, 0, 1, 1], [0, 0, 1, 0, 1, 1], [1, 0, 1, 0, 1, 1], [0, 1, 1, 0, 1, 1], [1, 1, 1, 0, 1, 1], [0, 0, 0, 1, 1, 1], [1, 0, 0, 1, 1, 1], [0, 1, 0, 1, 1, 1], [1, 1, 0, 1, 1, 1], [0, 0, 1, 1, 1, 1], [1, 0, 1, 1, 1, 1], [0, 1, 1, 1, 1, 1], [1, 1, 1, 1, 1, 1], [0, 0, 0, 0, 0, 0, 1], [1, 0, 0, 0, 0, 0, 1], [0, 1, 0, 0, 0, 0, 1], [1, 1, 0, 0, 0, 0, 1], [0, 0, 1, 0, 0, 0, 1], [1, 0, 1, 0, 0, 0, 1], [0, 1, 1, 0, 0, 0, 1], [1, 1, 1, 0, 0, 0, 1], [0, 0, 0, 1, 0, 0, 1], [1, 0, 0, 1, 0, 0, 1], [0, 1, 0, 1, 0, 0, 1], [1, 1, 0, 1, 0, 0, 1], [0, 0, 1, 1, 0, 0, 1], [1, 0, 1, 1, 0, 0, 1], [0, 1, 1, 1, 0, 0, 1], [1, 1, 1, 1, 0, 0, 1], [0, 0, 0, 0, 1, 0, 1], [1, 0, 0, 0, 1, 0, 1], [0, 1, 0, 0, 1, 0, 1], [1, 1, 0, 0, 1, 0, 1], [0, 0, 1, 0, 1, 0, 1], [1, 0, 1, 0, 1, 0, 1], [0, 1, 1, 0, 1, 0, 1], [1, 1, 1, 0, 1, 0, 1], [0, 0, 0, 1, 1, 0, 1], [1, 0, 0, 1, 1, 0, 1], [0, 1, 0, 1, 1, 0, 1], [1, 1, 0, 1, 1, 0, 1], [0, 0, 1, 1, 1, 0, 1], [1, 0, 1, 1, 1, 0, 1], [0, 1, 1, 1, 1, 0, 1], [1, 1, 1, 1, 1, 0, 1], [0, 0, 0, 0, 0, 1, 1], [1, 0, 0, 0, 0, 1, 1], [0, 1, 0, 0, 0, 1, 1], [1, 1, 0, 0, 0, 1, 1]]
    
    testValidation = testGroup "validation" $ 
        [ testValid 
        , testInvalid 
        ]
      where 
        testValid = testGroup "valid" $ 
            map test (take 10 $ drop 100 nums)
          where 
            test xs = testCase (show xs) (isValid xs @?= Just xs)
        testInvalid = testGroup "invalid" $ 
            map test [[], [1,0], [0,1,0,0], [0,0,2,0,1]]
          where 
            test xs = testCase (show xs) (isValid xs @?= Nothing)

    testFromTo = 
      testGroup "fromBinary and toBinary compose" $ 
          map (\x -> testCase (show x) ((toBinary x >>= fromBinary) @?= Just x)) [0 .. 10]

    testToFrom =
        testGroup "toBinary and fromBinary compose" $
          map (\x -> testCase (show x) ((fromBinary x >>= toBinary) @?= Just x)) (take 10 nums) 
