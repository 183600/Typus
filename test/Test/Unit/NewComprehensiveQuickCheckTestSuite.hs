{-# LANGUAGE OverloadedStrings #-}
module Test.Unit.NewComprehensiveQuickCheckTestSuite where

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
import TestSupport.EnhancedMemoryOptimization 
  ( enhancedMemoryCleanup
  , strategicMemoryCleanup
  , cleanupBetweenTests
  , withEnhancedMemoryControl
  , withStrictMemoryLimits
  , applyMemoryOptimizations
  )
import TestSupport.OptimizedStringOperations 
  ( genMinimalString
  , genUltraMinimalString
  , safeTake
  , safeLength
  , efficientTrim
  , efficientIsEmpty
  , withUltraStringLimit
  , minimizeStringUsage
  , optimizeStringProperty
  )
import TestSupport.TestPropertyMemoryCleanup 
  ( testGroupWithCleanup
  , testGroupWithStrategicCleanup
  , memoryAwareProperty
  , memoryOptimizedProperty
  , withPropertyMemoryCleanup
  )

-- 导入所有新的测试模块
import qualified Test.Unit.DependentTypesQuickCheckSpec as DependentTypesQuickCheckSpec
import qualified Test.Unit.OwnershipQuickCheckSpec as OwnershipQuickCheckSpec
import qualified Test.Unit.ParserQuickCheckSpec as ParserQuickCheckSpec
import qualified Test.Unit.CompilerQuickCheckSpec as CompilerQuickCheckSpec
import qualified Test.Unit.ErrorHandlingQuickCheckSpec as ErrorHandlingQuickCheckSpec
import qualified Test.Unit.SourceLocationQuickCheckSpec as SourceLocationQuickCheckSpec
import qualified Test.Unit.GoToolchainQuickCheckSpec as GoToolchainQuickCheckSpec

-- | 组合所有新的QuickCheck测试套件
newComprehensiveQuickCheckTestSuite :: TestTree
newComprehensiveQuickCheckTestSuite = testGroupWithStrategicCleanup "New Comprehensive QuickCheck Test Suite"
  [ memoryLimitedTestGroup "Dependent Types Tests" 
      [DependentTypesQuickCheckSpec.tests]
  , memoryLimitedTestGroup "Ownership Tests"
      [OwnershipQuickCheckSpec.tests]
  , memoryLimitedTestGroup "Parser Tests"
      [ParserQuickCheckSpec.tests]
  , memoryLimitedTestGroup "Compiler Tests"
      [CompilerQuickCheckSpec.tests]
  , memoryLimitedTestGroup "Error Handling Tests"
      [ErrorHandlingQuickCheckSpec.tests]
  , memoryLimitedTestGroup "Source Location Tests"
      [SourceLocationQuickCheckSpec.tests]
  , memoryLimitedTestGroup "Go Toolchain Tests"
      [GoToolchainQuickCheckSpec.tests]
  ]

-- | 内存优化的测试套件
memoryOptimizedTestSuite :: TestTree
memoryOptimizedTestSuite = memoryLevelTestGroup Minimal "New Comprehensive Memory Optimized Tests"
  [ DependentTypesQuickCheckSpec.memoryOptimizedTests
  , OwnershipQuickCheckSpec.memoryOptimizedTests
  , ParserQuickCheckSpec.memoryOptimizedTests
  , CompilerQuickCheckSpec.memoryOptimizedTests
  , ErrorHandlingQuickCheckSpec.memoryOptimizedTests
  , SourceLocationQuickCheckSpec.memoryOptimizedTests
  , GoToolchainQuickCheckSpec.memoryOptimizedTests
  ]

-- | 快速测试套件（仅包含关键测试）
quickTestSuite :: TestTree
quickTestSuite = testGroup "New Comprehensive Quick Tests"
  [ testGroup "Dependent Types" 
      [ testProperty "Value parameterized type parsing" DependentTypesQuickCheckSpec.prop_value_parameterized_type_parsing
      , testProperty "Refined type parsing" DependentTypesQuickCheckSpec.prop_refined_type_parsing
      , testProperty "Dependent function signature parsing" DependentTypesQuickCheckSpec.prop_dependent_function_signature_parsing
      ]
  , testGroup "Ownership"
      [ testProperty "Ownership directive parsing" OwnershipQuickCheckSpec.prop_ownership_directive_parsing
      , testProperty "Move semantics parsing" OwnershipQuickCheckSpec.prop_move_semantics_parsing
      , testProperty "Immutable borrow parsing" OwnershipQuickCheckSpec.prop_immutable_borrow_parsing
      , testProperty "Mutable borrow parsing" OwnershipQuickCheckSpec.prop_mutable_borrow_parsing
      ]
  , testGroup "Parser"
      [ testProperty "Identifier parsing" ParserQuickCheckSpec.prop_identifier_parsing
      , testProperty "Number literal parsing" ParserQuickCheckSpec.prop_number_literal_parsing
      , testProperty "Binary expression parsing" ParserQuickCheckSpec.prop_binary_expression_parsing
      , testProperty "Function definition parsing" ParserQuickCheckSpec.prop_function_definition_parsing
      ]
  , testGroup "Compiler"
      [ testProperty "Basic compilation" CompilerQuickCheckSpec.prop_basic_compilation
      , testProperty "IR generation consistency" CompilerQuickCheckSpec.prop_ir_generation_consistency
      , testProperty "Type check correct" CompilerQuickCheckSpec.prop_type_check_correct
      , testProperty "Code generation idempotent" CompilerQuickCheckSpec.prop_code_generation_idempotent
      ]
  , testGroup "Error Handling"
      [ testProperty "Error handler basic" ErrorHandlingQuickCheckSpec.prop_error_handler_basic
      , testProperty "Error message completeness" ErrorHandlingQuickCheckSpec.prop_error_message_completeness
      , testProperty "Error recovery" ErrorHandlingQuickCheckSpec.prop_error_recovery
      , testProperty "Enhanced error handling" ErrorHandlingQuickCheckSpec.prop_enhanced_error_handling
      ]
  , testGroup "Source Location"
      [ testProperty "Source location basic" SourceLocationQuickCheckSpec.prop_source_location_basic
      , testProperty "Source location line accuracy" SourceLocationQuickCheckSpec.prop_source_location_line_accuracy
      , testProperty "Source location column accuracy" SourceLocationQuickCheckSpec.prop_source_location_column_accuracy
      , testProperty "Source location range validity" SourceLocationQuickCheckSpec.prop_source_location_range_validity
      ]
  , testGroup "Go Toolchain"
      [ testProperty "Go toolchain basic" GoToolchainQuickCheckSpec.prop_go_toolchain_basic
      , testProperty "Go code valid" GoToolchainQuickCheckSpec.prop_go_code_valid
      , testProperty "Go code syntax" GoToolchainQuickCheckSpec.prop_go_code_syntax
      , testProperty "Go code imports" GoToolchainQuickCheckSpec.prop_go_code_imports
      ]
  ]

-- | 端到端集成测试套件
endToEndTestSuite :: TestTree
endToEndTestSuite = testGroupWithStrategicCleanup "New End-to-End Integration Tests"
  [ testGroup "Typus Language Features"
      [ testCase "Dependent Types Edge Cases" DependentTypesQuickCheckSpec.test_dependent_types_edge_cases
      , testCase "Dependent Types Complex Expressions" DependentTypesQuickCheckSpec.test_dependent_types_complex_expressions
      , testCase "Ownership Edge Cases" OwnershipQuickCheckSpec.test_ownership_edge_cases
      , testCase "Ownership Complex Expressions" OwnershipQuickCheckSpec.test_ownership_complex_expressions
      ]
  , testGroup "Compiler Pipeline"
      [ testCase "Parser Edge Cases" ParserQuickCheckSpec.test_parser_edge_cases
      , testCase "Parser Complex Expressions" ParserQuickCheckSpec.test_parser_complex_expressions
      , testCase "Compiler Edge Cases" CompilerQuickCheckSpec.test_compiler_edge_cases
      , testCase "Compiler Complex Expressions" CompilerQuickCheckSpec.test_compiler_complex_expressions
      ]
  , testGroup "Error Handling & Reporting"
      [ testCase "Error Handling Edge Cases" ErrorHandlingQuickCheckSpec.test_error_handling_edge_cases
      , testCase "Error Handling Complex Expressions" ErrorHandlingQuickCheckSpec.test_error_handling_complex_expressions
      , testCase "Source Location Edge Cases" SourceLocationQuickCheckSpec.test_source_location_edge_cases
      , testCase "Source Location Complex Expressions" SourceLocationQuickCheckSpec.test_source_location_complex_expressions
      ]
  , testGroup "Go Toolchain Integration"
      [ testCase "Go Toolchain Edge Cases" GoToolchainQuickCheckSpec.test_go_toolchain_edge_cases
      , testCase "Go Toolchain Complex Expressions" GoToolchainQuickCheckSpec.test_go_toolchain_complex_expressions
      ]
  ]

-- | 性能测试套件
performanceTestSuite :: TestTree
performanceTestSuite = memoryLevelTestGroup Minimal "New Performance Tests"
  [ testGroup "Parsing Performance"
      [ testProperty "Identifier parsing performance" ParserQuickCheckSpec.prop_identifier_parsing
      , testProperty "Number literal parsing performance" ParserQuickCheckSpec.prop_number_literal_parsing
      ]
  , testGroup "Compilation Performance"
      [ testProperty "Basic compilation performance" CompilerQuickCheckSpec.prop_basic_compilation
      , testProperty "IR generation consistency performance" CompilerQuickCheckSpec.prop_ir_generation_consistency
      ]
  , testGroup "Error Handling Performance"
      [ testProperty "Error handling performance" ErrorHandlingQuickCheckSpec.prop_error_handling_performance
      ]
  ]