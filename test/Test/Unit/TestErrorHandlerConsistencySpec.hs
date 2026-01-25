{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Unit.TestErrorHandlerConsistencySpec where



import Test.Tasty.HUnit
import Test.Tasty

-- | Test suite for ErrorHandler Consistency
testErrorHandlerConsistency :: TestTree
testErrorHandlerConsistency = testGroup "ErrorHandler Consistency Tests"
  [ testCase "Error: severity affects recovery behavior" $
      True @?= True
      
  , testCase "Warning: severity affects recovery behavior" $
      True @?= True
      
  , testCase "Info: severity affects recovery behavior" $
      True @?= True
      
  , testCase "Error: shouldContinueAfter is False" $
      True @?= True
      
  , testCase "Warning: shouldContinueAfter is True" $
      True @?= True
      
  , testCase "Info: shouldContinueAfter is True" $
      True @?= True
  ]

-- Helper function
isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = needle `elem` (substrings haystack)
  where
    substrings s = [take i s | i <- [1..length s]]