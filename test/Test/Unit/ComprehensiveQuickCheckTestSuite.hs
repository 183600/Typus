{-# LANGUAGE OverloadedStrings #-}
module Test.Unit.ComprehensiveQuickCheckTestSuite where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import TestSupport.MemoryLimits 
  ( withMemoryLimits
  , memoryLimitedTestGroup
  , memoryLevelTestGroup
  , MemoryLevel(..)
  , withMemoryLevel
  , gcBetweenTests
  )

-- 导入所有新的测试模块
import qualified Test.Unit.CoreUtilsQuickCheckTests as CoreUtilsQuickCheckTests
import qualified Test.Unit.ParserQuickCheckTests as ParserQuickCheckTests
import qualified Test.Unit.CompilerCoreQuickCheckTests as CompilerCoreQuickCheckTests
import qualified Test.Unit.DependencyAnalysisQuickCheckTests as DependencyAnalysisQuickCheckTests
import qualified Test.Unit.OwnershipAnalysisQuickCheckTests as OwnershipAnalysisQuickCheckTests
import qualified Test.Unit.ErrorHandlingQuickCheckTests as ErrorHandlingQuickCheckTests

-- | 组合所有QuickCheck测试套件
comprehensiveQuickCheckTestSuite :: TestTree
comprehensiveQuickCheckTestSuite = testGroup "Comprehensive QuickCheck Test Suite"
  [ memoryLimitedTestGroup "Core Utils Tests" 
      [CoreUtilsQuickCheckTests.coreUtilsTests]
  , memoryLimitedTestGroup "Parser Tests"
      [ParserQuickCheckTests.parserQuickCheckTests]
  , memoryLimitedTestGroup "Compiler Core Tests"
      [CompilerCoreQuickCheckTests.compilerCoreQuickCheckTests]
  , memoryLimitedTestGroup "Dependency Analysis Tests"
      [DependencyAnalysisQuickCheckTests.dependencyAnalysisQuickCheckTests]
  , memoryLimitedTestGroup "Ownership Analysis Tests"
      [OwnershipAnalysisQuickCheckTests.ownershipAnalysisQuickCheckTests]
  , memoryLimitedTestGroup "Error Handling Tests"
      [ErrorHandlingQuickCheckTests.errorHandlingQuickCheckTests]
  ]

-- | 内存优化的测试套件
memoryOptimizedTestSuite :: TestTree
memoryOptimizedTestSuite = memoryLevelTestGroup "Memory Optimized QuickCheck Tests" LowMemory
  [ gcBetweenTests $ CoreUtilsQuickCheckTests.coreUtilsTests
  , gcBetweenTests $ ParserQuickCheckTests.parserQuickCheckTests
  , gcBetweenTests $ CompilerCoreQuickCheckTests.compilerCoreQuickCheckTests
  , gcBetweenTests $ DependencyAnalysisQuickCheckTests.dependencyAnalysisQuickCheckTests
  , gcBetweenTests $ OwnershipAnalysisQuickCheckTests.ownershipAnalysisQuickCheckTests
  , gcBetweenTests $ ErrorHandlingQuickCheckTests.errorHandlingQuickCheckTests
  ]

-- | 快速测试套件（仅包含关键测试）
quickTestSuite :: TestTree
quickTestSuite = testGroup "Quick QuickCheck Tests"
  [ testGroup "Core Utils" 
      [ testProperty "trim idempotent" CoreUtilsQuickCheckTests.prop_trim_idempotent
      , testProperty "splitBy length" CoreUtilsQuickCheckTests.prop_split_by_length
      , testProperty "remove line comments preserves strings" CoreUtilsQuickCheckTests.prop_remove_line_comments_preserves_strings
      ]
  , testGroup "Parser"
      [ testProperty "parse identifier basic" ParserQuickCheckTests.prop_parse_identifier_basic
      , testProperty "parse number" ParserQuickCheckTests.prop_parse_number
      , testProperty "parse string literal" ParserQuickCheckTests.prop_parse_string_literal
      ]
  , testGroup "Compiler"
      [ testProperty "compile basic" CompilerCoreQuickCheckTests.prop_compile_basic
      , testProperty "ir generation consistent" CompilerCoreQuickCheckTests.prop_ir_generation_consistent
      , testProperty "type check correct" CompilerCoreQuickCheckTests.prop_type_check_correct
      ]
  , testGroup "Dependencies"
      [ testProperty "dependency analysis basic" DependencyAnalysisQuickCheckTests.prop_dependency_analysis_basic
      , testProperty "cycle detection" DependencyAnalysisQuickCheckTests.prop_cycle_detection
      , testProperty "no cycle detection" DependencyAnalysisQuickCheckTests.prop_no_cycle_detection
      ]
  , testGroup "Ownership"
      [ testProperty "ownership analysis basic" OwnershipAnalysisQuickCheckTests.prop_ownership_analysis_basic
      , testProperty "ownership transfer detection" OwnershipAnalysisQuickCheckTests.prop_ownership_transfer_detection
      , testProperty "borrow checking" OwnershipAnalysisQuickCheckTests.prop_borrow_checking
      ]
  , testGroup "Error Handling"
      [ testProperty "error handler basic" ErrorHandlingQuickCheckTests.prop_error_handler_basic
      , testProperty "error collection completeness" ErrorHandlingQuickCheckTests.prop_error_collection_completeness
      , testProperty "error recovery" ErrorHandlingQuickCheckTests.prop_error_recovery
      ]
  ]