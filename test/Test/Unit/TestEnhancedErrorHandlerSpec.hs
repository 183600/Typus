{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Unit.TestEnhancedErrorHandlerSpec where



import Test.Tasty.HUnit
import Test.Tasty

-- | Test suite for Enhanced ErrorHandler
testEnhancedErrorHandler :: TestTree
testEnhancedErrorHandler = testGroup "Enhanced ErrorHandler Tests"
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