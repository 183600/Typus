{-# LANGUAGE OverloadedStrings #-}
module Test.Unit.NewComprehensiveQuickCheckTestSuite2 where

import Test.Tasty

import Test.Unit.NewParserQuickCheckSpec (essentialTests)
import Test.Unit.NewSourceLocationQuickCheckSpec (essentialTests)
import Test.Unit.NewUtilsQuickCheckSpec (essentialTests)

-- | 轻量级综合测试套件，包含所有新创建的QuickCheck测试的精简版本
newComprehensiveQuickCheckTestSuite2 :: TestTree
newComprehensiveQuickCheckTestSuite2 = testGroup "New Comprehensive QuickCheck Test Suite 2 (Essential)"
  [ Test.Unit.NewParserQuickCheckSpec.essentialTests
  , Test.Unit.NewSourceLocationQuickCheckSpec.essentialTests
  , Test.Unit.NewUtilsQuickCheckSpec.essentialTests
  ]