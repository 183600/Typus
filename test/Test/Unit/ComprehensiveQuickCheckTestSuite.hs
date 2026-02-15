{-# LANGUAGE OverloadedStrings #-}
module Test.Unit.ComprehensiveQuickCheckTestSuite where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import TestSupport.UnifiedMemoryOptimization 
  ( getUnifiedMemoryConfig
  , withUnifiedMemoryOptimization
  , unifiedMemoryTestGroup
  , criticalMemoryConfig
  , minimalMemoryConfig
  , efficientMemoryConfig
  , balancedMemoryConfig
  , comprehensiveMemoryConfig
  , unifiedMemoryGC
  , runTestsWithMemoryOptimization
  )
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
memoryOptimizedTestSuite = memoryLevelTestGroup Minimal "Memory Optimized QuickCheck Tests"
  [ CoreUtilsQuickCheckTests.coreUtilsTests
  , ParserQuickCheckTests.parserQuickCheckTests
  , CompilerCoreQuickCheckTests.compilerCoreQuickCheckTests
  , DependencyAnalysisQuickCheckTests.dependencyAnalysisQuickCheckTests
  , OwnershipAnalysisQuickCheckTests.ownershipAnalysisQuickCheckTests
  , ErrorHandlingQuickCheckTests.errorHandlingQuickCheckTests
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
      [ testProperty "validate identifier" ParserQuickCheckTests.prop_validate_identifier
      , testProperty "parse empty input" ParserQuickCheckTests.prop_parse_empty_input
      , testProperty "parse simple expression" ParserQuickCheckTests.prop_parse_simple_expression
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

-- | 统一内存优化的测试套件（自适应）
unifiedMemoryOptimizedTestSuite :: IO TestTree
unifiedMemoryOptimizedTestSuite = do
  config <- getUnifiedMemoryConfig
  return $ unifiedMemoryTestGroup config "Unified Memory Optimized Tests"
    [ CoreUtilsQuickCheckTests.coreUtilsTests
    , ParserQuickCheckTests.parserQuickCheckTests
    , CompilerCoreQuickCheckTests.compilerCoreQuickCheckTests
    , DependencyAnalysisQuickCheckTests.dependencyAnalysisQuickCheckTests
    , OwnershipAnalysisQuickCheckTests.ownershipAnalysisQuickCheckTests
    , ErrorHandlingQuickCheckTests.errorHandlingQuickCheckTests
    ]

-- | 关键内存优化的测试套件
criticalMemoryOptimizedTestSuite :: TestTree
criticalMemoryOptimizedTestSuite = unifiedMemoryTestGroup criticalMemoryConfig "Critical Memory Tests"
  [ testProperty "trim idempotent" CoreUtilsQuickCheckTests.prop_trim_idempotent
  , testProperty "validate identifier" ParserQuickCheckTests.prop_validate_identifier
  , testProperty "compile basic" CompilerCoreQuickCheckTests.prop_compile_basic
  , testProperty "dependency analysis basic" DependencyAnalysisQuickCheckTests.prop_dependency_analysis_basic
  , testProperty "ownership analysis basic" OwnershipAnalysisQuickCheckTests.prop_ownership_analysis_basic
  , testProperty "error handler basic" ErrorHandlingQuickCheckTests.prop_error_handler_basic
  ]

-- | 最小内存优化的测试套件
minimalMemoryOptimizedTestSuite :: TestTree
minimalMemoryOptimizedTestSuite = unifiedMemoryTestGroup minimalMemoryConfig "Minimal Memory Tests"
  [ testProperty "trim idempotent" CoreUtilsQuickCheckTests.prop_trim_idempotent
  , testProperty "splitBy length" CoreUtilsQuickCheckTests.prop_split_by_length
  , testProperty "validate identifier" ParserQuickCheckTests.prop_validate_identifier
  , testProperty "parse empty input" ParserQuickCheckTests.prop_parse_empty_input
  , testProperty "compile basic" CompilerCoreQuickCheckTests.prop_compile_basic
  , testProperty "ir generation consistent" CompilerCoreQuickCheckTests.prop_ir_generation_consistent
  , testProperty "dependency analysis basic" DependencyAnalysisQuickCheckTests.prop_dependency_analysis_basic
  , testProperty "cycle detection" DependencyAnalysisQuickCheckTests.prop_cycle_detection
  , testProperty "ownership analysis basic" OwnershipAnalysisQuickCheckTests.prop_ownership_analysis_basic
  , testProperty "ownership transfer detection" OwnershipAnalysisQuickCheckTests.prop_ownership_transfer_detection
  , testProperty "error handler basic" ErrorHandlingQuickCheckTests.prop_error_handler_basic
  , testProperty "error collection completeness" ErrorHandlingQuickCheckTests.prop_error_collection_completeness
  ]

-- | 高效内存优化的测试套件
efficientMemoryOptimizedTestSuite :: TestTree
efficientMemoryOptimizedTestSuite = unifiedMemoryTestGroup efficientMemoryConfig "Efficient Memory Tests"
  [ CoreUtilsQuickCheckTests.coreUtilsTests
  , ParserQuickCheckTests.parserQuickCheckTests
  , CompilerCoreQuickCheckTests.compilerCoreQuickCheckTests
  , DependencyAnalysisQuickCheckTests.dependencyAnalysisQuickCheckTests
  , OwnershipAnalysisQuickCheckTests.ownershipAnalysisQuickCheckTests
  , ErrorHandlingQuickCheckTests.errorHandlingQuickCheckTests
  ]

-- | 平衡内存优化的测试套件
balancedMemoryOptimizedTestSuite :: TestTree
balancedMemoryOptimizedTestSuite = unifiedMemoryTestGroup balancedMemoryConfig "Balanced Memory Tests"
  [ CoreUtilsQuickCheckTests.coreUtilsTests
  , ParserQuickCheckTests.parserQuickCheckTests
  , CompilerCoreQuickCheckTests.compilerCoreQuickCheckTests
  , DependencyAnalysisQuickCheckTests.dependencyAnalysisQuickCheckTests
  , OwnershipAnalysisQuickCheckTests.ownershipAnalysisQuickCheckTests
  , ErrorHandlingQuickCheckTests.errorHandlingQuickCheckTests
  ]

-- | 全面内存优化的测试套件
comprehensiveMemoryOptimizedTestSuite :: TestTree
comprehensiveMemoryOptimizedTestSuite = unifiedMemoryTestGroup comprehensiveMemoryConfig "Comprehensive Memory Tests"
  [ CoreUtilsQuickCheckTests.coreUtilsTests
  , ParserQuickCheckTests.parserQuickCheckTests
  , CompilerCoreQuickCheckTests.compilerCoreQuickCheckTests
  , DependencyAnalysisQuickCheckTests.dependencyAnalysisQuickCheckTests
  , OwnershipAnalysisQuickCheckTests.ownershipAnalysisQuickCheckTests
  , ErrorHandlingQuickCheckTests.errorHandlingQuickCheckTests
  ]