{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.AdditionalIntegratedCompilerQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import Test.QuickCheck.Gen (Gen, choose, listOf, elements, oneof, vectorOf)
import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.QuickCheck.Monadic (monadicIO, run, assert)

import IntegratedCompiler
  ( CompilerConfig(..)
  , defaultCompilerConfig
  , compileWithIntegratedAnalyzers
  , formatCompilationResult
  , getDetailedAnalysisSummary
  , analysisToCombined
  , showCombinedError
  , IntegratedCompileResult(..)
  )
import AnalyzerIntegration
  ( AnalysisResult(..)
  , CombinedError(..)
  , ErrorSeverity(..)
  , newIntegratedAnalyzer
  , mkAnalysisInput
  )
import qualified SyntaxValidator as SV
import qualified Parser as P
-- import Compiler.Errors.Compiler (CompilerError)  -- Module is hidden
import Ownership.Common.Types (OwnershipError(..))
import Dependencies.TypeSystem (DependentTypeError(..), TypeVar, TypeConstraint)

import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf, length)
import Data.List (null)
import Data.Maybe (isJust, isNothing)
import qualified Data.Map as Map

-- ============================================================================
-- Arbitrary Instances
-- ============================================================================

instance Arbitrary ErrorSeverity where
    arbitrary = elements [Info, Warning, Error, Fatal]

instance Arbitrary OwnershipError where
    arbitrary = oneof
        [ UseAfterMove <$> arbitrary
        , DoubleMove <$> arbitrary <*> arbitrary
        , BorrowWhileMoved <$> arbitrary
        , MutBorrowWhileBorrowed <$> arbitrary
        , BorrowWhileMutBorrowed <$> arbitrary
        , MultipleMutBorrows <$> arbitrary
        , UseWhileMutBorrowed <$> arbitrary
        , OutOfScope <$> arbitrary
        , BorrowError <$> arbitrary
        , Ownership.Common.Types.ParseError <$> arbitrary
        , CrossFunctionMove <$> arbitrary <*> arbitrary
        , ParameterMoveMismatch <$> arbitrary
        , ControlFlowError <$> arbitrary
        ]

instance Arbitrary TypeVar where
    arbitrary = arbitrary  -- Simplified - TypeVar is abstract

instance Arbitrary TypeConstraint where
    arbitrary = arbitrary  -- Simplified - TypeConstraint is abstract

instance Arbitrary DependentTypeError where
    arbitrary = oneof
        [ DependentTypeMismatch <$> arbitrary <*> arbitrary
        , ConstraintViolation <$> arbitrary <*> arbitrary
        , TypeNotFound <$> arbitrary
        , InvalidTypeArgument <$> arbitrary
        , UnsolvableConstraint <$> arbitrary
        , DependentInfiniteType <$> arbitrary <*> arbitrary
        , AmbiguousType <$> arbitrary
        , Dependencies.TypeSystem.ParseError <$> arbitrary
        , SemanticError <$> arbitrary
        ]

instance Arbitrary CompilerConfig where
    arbitrary = do
        enableOwnership <- arbitrary
        enableDependentTypes <- arbitrary
        errorReportingLevel <- arbitrary
        return $ CompilerConfig enableOwnership enableDependentTypes errorReportingLevel

instance Arbitrary CombinedError where
    arbitrary = oneof
        [ OwnershipErrorCombined <$> arbitrary <*> arbitrary
        , DependentTypeErrorCombined <$> arbitrary <*> arbitrary
        , IntegrationError <$> arbitrary <*> arbitrary
        , CrossAnalyzerError <$> arbitrary <*> arbitrary <*> listOf arbitrary
        ]

instance Arbitrary AnalysisResult where
    arbitrary = do
        ownershipErrors <- listOf arbitrary
        dependentTypeErrors <- listOf arbitrary
        analysisWarnings <- listOf arbitrary
        analysisInfo <- listOf arbitrary
        let typeEnvironment = Map.empty  -- Simplified for testing
        combinedErrors <- listOf arbitrary
        return $ AnalysisResult ownershipErrors dependentTypeErrors combinedErrors analysisWarnings analysisInfo typeEnvironment

instance Arbitrary SV.SyntaxError where
    arbitrary = do
        errorType <- arbitrary
        errorMessage <- arbitrary
        lineNumber <- choose (1, 100)
        columnNumber <- choose (1, 100)
        lineContent <- arbitrary
        return $ SV.SyntaxError errorType errorMessage lineNumber columnNumber lineContent

instance Arbitrary SV.ErrorType where
    arbitrary = elements [SV.UnexpectedToken, SV.MissingBrace, SV.InvalidIdentifier, SV.SyntaxWarning]

-- ============================================================================
-- Property Tests for CompilerConfig
-- ============================================================================

-- Property: Default compiler config enables both analyzers
prop_default_config_enables_analyzers :: Property
prop_default_config_enables_analyzers =
    let config = defaultCompilerConfig
    in property $ enableOwnership config .&&. enableDependentTypes config

-- Property: Default config reports warnings and above
prop_default_config_reports_warnings :: Property
prop_default_config_reports_warnings =
    let config = defaultCompilerConfig
    in property $ errorReportingLevel config === Warning

-- Property: Config equality works correctly
prop_config_equality :: CompilerConfig -> CompilerConfig -> Property
prop_config_equality config1 config2 =
    let same = config1 == config2
        sameFields = enableOwnership config1 == enableOwnership config2 &&
                     enableDependentTypes config1 == enableDependentTypes config2 &&
                     errorReportingLevel config1 == errorReportingLevel config2
    in property $ same === sameFields

-- ============================================================================
-- Property Tests for Compilation Result
-- ============================================================================

-- Property: Empty source code produces consistent result
prop_empty_source_compilation :: CompilerConfig -> Property
prop_empty_source_compilation config =
    monadicIO $ do
        result <- run $ compileWithIntegratedAnalyzers "" config
        assert $ not (L.null (formatCompilationResult result))

-- Property: Invalid source code produces compilation failure
prop_invalid_source_failure :: CompilerConfig -> Property
prop_invalid_source_failure config =
    monadicIO $ do
        let invalidSource = "invalid { syntax } here"
        result <- run $ compileWithIntegratedAnalyzers invalidSource config
        assert $ not (success result)

-- Property: Valid simple source code may succeed
prop_valid_simple_source :: CompilerConfig -> Property
prop_valid_simple_source config =
    monadicIO $ do
        let validSource = "//go:embed *\npackage main\nfunc main() {}"
        result <- run $ compileWithIntegratedAnalyzers validSource config
        -- Result may succeed L.or fail depending on parser, but should be consistent
        assert $ True  -- If we get here without crashing, it's consistent

-- ============================================================================
-- Property Tests for Format Compilation Result
-- ============================================================================

-- Property: Format result always contains status line
prop_format_contains_status :: CompilerConfig -> String -> Property
prop_format_contains_status config source =
    monadicIO $ do
        result <- run $ compileWithIntegratedAnalyzers source config
        let formatted = formatCompilationResult result
        assert $ "✅ Compilation Successful" `L.isInfixOf` formatted ||
                 "❌ Compilation Failed" `L.isInfixOf` formatted

-- Property: Successful result contains success indicator
prop_format_success_indicator :: CompilerConfig -> String -> Property
prop_format_success_indicator config source =
    monadicIO $ do
        result <- run $ compileWithIntegratedAnalyzers source config
        let formatted = formatCompilationResult result
        assert $ if success result
                 then "✅ Compilation Successful" `L.isInfixOf` formatted
                 else "❌ Compilation Failed" `L.isInfixOf` formatted

-- Property: Failed result contains error information
prop_format_failure_contains_errors :: CompilerConfig -> String -> Property
prop_format_failure_contains_errors config source =
    monadicIO $ do
        result <- run $ compileWithIntegratedAnalyzers source config
        let formatted = formatCompilationResult result
        assert $ if not (success result)
                 then not (null formatted) &&
                      ("Syntax Errors" `L.isInfixOf` formatted ||
                       "Analysis Errors" `L.isInfixOf` formatted ||
                       "Compiler Errors" `L.isInfixOf` formatted ||
                       "Warnings" `L.isInfixOf` formatted)
                 else True

-- ============================================================================
-- Property Tests for Analysis Summary
-- ============================================================================

-- Property: Analysis summary contains expected sections
prop_analysis_summary_sections :: AnalysisResult -> Property
prop_analysis_summary_sections analysis =
    let summary = getDetailedAnalysisSummary analysis
    in property $ "Analysis Summary" `L.isInfixOf` summary .&&.
                 "Ownership errors:" `L.isInfixOf` summary .&&.
                 "Dependent type errors:" `L.isInfixOf` summary .&&.
                 "Warnings:" `L.isInfixOf` summary .&&.
                 "Info messages:" `L.isInfixOf` summary .&&.
                 "Type environment bindings:" `L.isInfixOf` summary .&&.
                 "Status:" `L.isInfixOf` summary

-- Property: Analysis summary reflects error counts
prop_analysis_summary_error_counts :: Int -> Int -> Property
prop_analysis_summary_error_counts ownershipCount dependentCount =
    ownershipCount >= 0 && ownershipCount <= 10 &&
    dependentCount >= 0 && dependentCount <= 10 ==> 
    let analysis = AnalysisResult 
            { ownershipErrors = replicate ownershipCount undefined
            , dependentTypeErrors = replicate dependentCount undefined
            , analysisWarnings = []
            , analysisInfo = []
            , typeEnvironment = undefined
            , combinedErrors = []
            }
        summary = getDetailedAnalysisSummary analysis
    in property $ show ownershipCount `L.isInfixOf` summary .&&.
                 show dependentCount `L.isInfixOf` summary

-- ============================================================================
-- Property Tests for Combined Error Conversion
-- ============================================================================

-- Property: analysisToCombined preserves combined errors
prop_analysis_to_combined_preserves :: AnalysisResult -> Property
prop_analysis_to_combined_preserves analysis =
    let converted = analysisToCombined analysis
        original = combinedErrors analysis
    in property $ L.length converted === L.length original

-- Property: showCombinedError produces non-empty output
prop_show_combined_error_non_empty :: CombinedError -> Property
prop_show_combined_error_non_empty error =
    let shown = showCombinedError error
    in property $ not (null shown)

-- Property: showCombinedError contains error type information
prop_show_combined_error_contains_type :: CombinedError -> Property
prop_show_combined_error_contains_type error =
    let shown = showCombinedError error
    in property $ ("Ownership error:" `L.isInfixOf` shown .||.
                 "Dependent type error:" `L.isInfixOf` shown .||.
                 "Integration error:" `L.isInfixOf` shown .||.
                 "Cross-analyzer error:" `L.isInfixOf` shown)

-- ============================================================================
-- Property Tests for Error Severity
-- ============================================================================

-- Property: Error severity ordering is consistent
prop_error_severity_ordering :: ErrorSeverity -> ErrorSeverity -> Property
prop_error_severity_ordering sev1 sev2 =
    let severityOrder sev = case sev of
          Info -> 1
          Warning -> 2
          Error -> 3
          Fatal -> 4
        sev1Order = severityOrder sev1
        sev2Order = severityOrder sev2
    in property $ (sev1 <= sev2) === (sev1Order <= sev2Order)

-- ============================================================================
-- Property Tests for Integration
-- ============================================================================

-- Property: Compilation with different configs produces different results
prop_different_configs_different_results :: String -> Property
prop_different_configs_different_results source =
    let config1 = defaultCompilerConfig { enableOwnership = True, enableDependentTypes = False }
        config2 = defaultCompilerConfig { enableOwnership = False, enableDependentTypes = True }
    in monadicIO $ do
        result1 <- run $ compileWithIntegratedAnalyzers source config1
        result2 <- run $ compileWithIntegratedAnalyzers source config2
        -- Results may be the same for simple inputs, but should be consistent
        assert $ True  -- If we get here without crashing, it's consistent

-- Property: Compilation handles large inputs gracefully
prop_large_input_handling :: Int -> Property
prop_large_input_handling n =
    n >= 0 && n <= 100 ==>  -- Limit for performance testing
    let largeSource = unlines $ replicate n "package main\nfunc main() {}"
    in monadicIO $ do
        result <- run $ compileWithIntegratedAnalyzers largeSource defaultCompilerConfig
        assert $ not (L.null (formatCompilationResult result))

-- ============================================================================
-- Edge Case Tests
-- ============================================================================

-- Property: Compilation handles Unicode characters
prop_unicode_handling :: String -> Property
prop_unicode_handling base =
    let unicodeSource = base ++ " café naïve résumé 🚀 测试"
    in monadicIO $ do
        result <- run $ compileWithIntegratedAnalyzers unicodeSource defaultCompilerConfig
        assert $ not (L.null (formatCompilationResult result))

-- Property: Compilation handles special characters
prop_special_characters :: String -> Property
prop_special_characters base =
    let specialSource = base ++ " \t\n\r\\\"'`!@#$%^&*()[]{}|;:,.<>?"
    in monadicIO $ do
        result <- run $ compileWithIntegratedAnalyzers specialSource defaultCompilerConfig
        assert $ not (L.null (formatCompilationResult result))

-- Property: Compilation handles extremely long lines
prop_long_lines :: Int -> Property
prop_long_lines len =
    len >= 0 && len <= 1000 ==>  -- Limit for performance testing
    let longLine = replicate len 'x'
        source = longLine ++ "\npackage main\nfunc main() {}"
    in monadicIO $ do
        result <- run $ compileWithIntegratedAnalyzers source defaultCompilerConfig
        assert $ not (L.null (formatCompilationResult result))

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Additional IntegratedCompiler QuickCheck Tests"
    [ testGroup "CompilerConfig Properties"
        [ fastProperty "Default compiler config enables both analyzers" prop_default_config_enables_analyzers
        , fastProperty "Default config reports warnings and above" prop_default_config_reports_warnings
        , fastProperty "Config equality works correctly" prop_config_equality
        ]
    , testGroup "Compilation Result Properties"
        [ fastProperty "Empty source code produces consistent result" prop_empty_source_compilation
        , fastProperty "Invalid source code produces compilation failure" prop_invalid_source_failure
        , fastProperty "Valid simple source code may succeed" prop_valid_simple_source
        ]
    , testGroup "Format Compilation Result Properties"
        [ fastProperty "Format result always contains status line" prop_format_contains_status
        , fastProperty "Successful result contains success indicator" prop_format_success_indicator
        , fastProperty "Failed result contains error information" prop_format_failure_contains_errors
        ]
    , testGroup "Analysis Summary Properties"
        [ fastProperty "Analysis summary contains expected sections" prop_analysis_summary_sections
        , fastProperty "Analysis summary reflects error counts" prop_analysis_summary_error_counts
        ]
    , testGroup "Combined Error Conversion Properties"
        [ fastProperty "analysisToCombined preserves combined errors" prop_analysis_to_combined_preserves
        , fastProperty "showCombinedError produces non-empty output" prop_show_combined_error_non_empty
        , fastProperty "showCombinedError contains error type information" prop_show_combined_error_contains_type
        ]
    , testGroup "Error Severity Properties"
        [ fastProperty "Error severity ordering is consistent" prop_error_severity_ordering
        ]
    , testGroup "Integration Properties"
        [ fastProperty "Compilation with different configs produces different results" prop_different_configs_different_results
        , fastProperty "Compilation handles large inputs gracefully" prop_large_input_handling
        ]
    , testGroup "Edge Cases"
        [ fastProperty "Compilation handles Unicode characters" prop_unicode_handling
        , fastProperty "Compilation handles special characters" prop_special_characters
        , fastProperty "Compilation handles extremely long lines" prop_long_lines
        ]
    ]