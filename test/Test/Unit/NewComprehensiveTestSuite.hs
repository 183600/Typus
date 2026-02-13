{-# LANGUAGE OverloadedStrings #-}
module Test.Unit.NewComprehensiveTestSuite where

import Test.Tasty

-- 导入所有新创建的测试模块
import qualified Test.Unit.ParserComprehensiveQuickCheckSpec as ParserTests
import qualified Test.Unit.CompilerComprehensiveQuickCheckSpec as CompilerTests
import qualified Test.Unit.SourceLocationComprehensiveQuickCheckSpec as SourceLocationTests
import qualified Test.Unit.ErrorHandlerComprehensiveQuickCheckSpec as ErrorHandlerTests
import qualified Test.Unit.DependenciesComprehensiveQuickCheckSpec as DependenciesTests
import qualified Test.Unit.OwnershipComprehensiveQuickCheckSpec as OwnershipTests
import qualified Test.Unit.DependentTypesParserComprehensiveQuickCheckSpec as DependentTypesParserTests
import qualified Test.Unit.SyntaxValidatorComprehensiveQuickCheckSpec as SyntaxValidatorTests

-- | 新增的Comprehensive测试套件，包含约200个QuickCheck测试用例
testSuite :: TestTree
testSuite = testGroup "新增Comprehensive QuickCheck测试套件 (约200个测试用例)"
  [ ParserTests.testSuite
  , CompilerTests.testSuite
  , SourceLocationTests.testSuite
  , ErrorHandlerTests.testSuite
  , DependenciesTests.testSuite
  , OwnershipTests.testSuite
  , DependentTypesParserTests.testSuite
  , SyntaxValidatorTests.testSuite
  ]