{-# LANGUAGE OverloadedStrings #-}
module Test.Unit.NewComprehensiveQuickCheckTestSuite where

import Test.Tasty

import Test.Unit.NewParserQuickCheckSpec (tests)
import Test.Unit.NewCompilerQuickCheckSpec (tests)
import Test.Unit.NewSourceLocationQuickCheckSpec (tests)
import Test.Unit.NewUtilsQuickCheckSpec (tests)

-- | 综合测试套件，包含所有新创建的QuickCheck测试
newComprehensiveQuickCheckTestSuite :: TestTree
newComprehensiveQuickCheckTestSuite = testGroup "New Comprehensive QuickCheck Test Suite"
  [ Test.Unit.NewParserQuickCheckSpec.tests
  , Test.Unit.NewCompilerQuickCheckSpec.tests
  , Test.Unit.NewSourceLocationQuickCheckSpec.tests
  , Test.Unit.NewUtilsQuickCheckSpec.tests
  ]