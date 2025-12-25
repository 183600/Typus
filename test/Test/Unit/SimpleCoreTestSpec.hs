{-# LANGUAGE TemplateHaskell #-}

module Test.Unit.SimpleCoreTestSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Utils (trim, splitBy)

tests :: TestTree
tests = testGroup "Simple Core Tests"
  [ testCase "trim removes whitespace" $
        trim "  hello  " @?= "hello"
  , testCase "splitBy works" $
        splitBy ',' "a,b,c" @?= ["a", "b", "c"]
  ]