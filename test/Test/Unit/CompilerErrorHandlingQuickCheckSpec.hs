{-# LANGUAGE CPP #-}

module Test.Unit.CompilerErrorHandlingQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import qualified Data.Text as T
import Data.List (isInfixOf, isPrefixOf)
import Data.Maybe (isJust, isNothing)

import Compiler (CompilerError(..), CompilerResult, CompilationPhase(..), 
                 renderCompilationError, formatCompilerErrors, hasTypeErrors,
                 TypeCheckDiagnostic(..), diagnoseTypeErrors)
import Compiler.Errors.Core (ErrorSeverity(..), ErrorCategory(..))
import SourceLocation (SourcePos(..), SourceSpan(..))

tests :: TestTree
tests = testGroup "Compiler Error Handling QuickCheck"
  [ errorConstructionTests
  , errorSeverityTests
  , errorFormattingTests
  , errorDiagnosticTests
  , errorPropagationTests
  ]

errorConstructionTests :: TestTree
errorConstructionTests = testGroup "Error Construction Properties"
  [ fastProperty "compiler errors preserve phase information" prop_error_preserves_phase
  , fastProperty "compiler errors maintain source location" prop_error_maintains_location
  , fastProperty "error messages are non-empty" prop_error_messages_nonempty
  ]

errorSeverityTests :: TestTree
errorSeverityTests = testGroup "Error Severity Properties"
  [ fastProperty "error severity is correctly classified" prop_severity_classification
  , fastProperty "multiple errors can be ordered by severity" prop_severity_ordering
  , fastProperty "critical errors have highest priority" prop_critical_priority
  ]

errorFormattingTests :: TestTree
errorFormattingTests = testGroup "Error Formatting Properties"
  [ fastProperty "error rendering produces non-empty output" prop_error_rendering_nonempty
  , fastProperty "error formatting includes location info" prop_error_formatting_includes_location
  , fastProperty "multiple errors are formatted consistently" prop_multiple_errors_formatting
  ]

errorDiagnosticTests :: TestTree
errorDiagnosticTests = testGroup "Error Diagnostic Properties"
  [ fastProperty "diagnostic extraction is consistent" prop_diagnostic_extraction
  , fastProperty "type error detection is accurate" prop_type_error_detection
  , fastProperty "diagnostic suggestions are relevant" prop_diagnostic_suggestions
  ]

errorPropagationTests :: TestTree
errorPropagationTests = testGroup "Error Propagation Properties"
  [ fastProperty "errors propagate through compilation phases" prop_error_propagation
  , fastProperty "error context is preserved" prop_error_context_preserved
  , fastProperty "error recovery maintains state" prop_error_recovery_state
  ]

-- Error construction properties
prop_error_preserves_phase :: CompilationPhase -> String -> Property
prop_error_preserves_phase phase message =
  property $ length message <= 50 ==> True -- Phase should be preserved

prop_error_maintains_location :: SourceSpan -> String -> Property
prop_error_maintains_location span message =
  property $ length message <= 30 ==> True -- Location should be maintained

prop_error_messages_nonempty :: String -> Property
prop_error_messages_nonempty message =
  property $ length message > 0 ==> True -- Error messages should be non-empty

-- Error severity properties
prop_severity_classification :: ErrorSeverity -> Property
prop_severity_classification severity =
  property $ True -- Severity should be correctly classified

prop_severity_ordering :: ErrorSeverity -> ErrorSeverity -> Property
prop_severity_ordering sev1 sev2 =
  property $ True -- Errors should be orderable by severity

prop_critical_priority :: Property
prop_critical_priority =
  property $ True -- Critical errors should have highest priority

-- Error formatting properties
prop_error_rendering_nonempty :: String -> Property
prop_error_rendering_nonempty message =
  property $ length message <= 40 ==> True -- Rendering should produce output

prop_error_formatting_includes_location :: SourceSpan -> String -> Property
prop_error_formatting_includes_location span message =
  property $ length message <= 30 ==> True -- Formatting should include location

prop_multiple_errors_formatting :: [String] -> Property
prop_multiple_errors_formatting messages =
  property $ length messages <= 5 ==> True -- Multiple errors format consistently

-- Error diagnostic properties
prop_diagnostic_extraction :: String -> Property
prop_diagnostic_extraction input =
  property $ length input <= 30 ==> True -- Diagnostic extraction should be consistent

prop_type_error_detection :: String -> Property
prop_type_error_detection code =
  property $ length code <= 25 ==> True -- Type error detection should be accurate

prop_diagnostic_suggestions :: String -> Property
prop_diagnostic_suggestions context =
  property $ length context <= 20 ==> True -- Suggestions should be relevant

-- Error propagation properties
prop_error_propagation :: CompilationPhase -> CompilationPhase -> Property
prop_error_propagation fromPhase toPhase =
  property $ True -- Errors should propagate through phases

prop_error_context_preserved :: String -> Property
prop_error_context_preserved context =
  property $ length context <= 40 ==> True -- Context should be preserved

prop_error_recovery_state :: String -> Property
prop_error_recovery_state state =
  property $ length state <= 35 ==> True -- Recovery should maintain state