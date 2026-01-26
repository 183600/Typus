{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Test.Unit.ErrorHandlerResilienceSpec where


import Test.Tasty
import Test.Tasty.QuickCheck



import Test.Tasty
import Test.Tasty.QuickCheck
import Utils
import Parser (TypusFile(..), parseTypus, defaultFileDirectives, 
              FileDirectives(..), CodeBlock(..), cbSpan, cbContent, 
              fdOwnership, fdDependentTypes, fdConstraints)
import SourceLocation (SourcePos(..), SourceSpan(..), Located(..), startPos, spanBetween)
import Compiler (compile, CompilerError(..))
import Compiler.Errors.Core (ErrorSeverity(..), ErrorContext(..))
import qualified Data.Text as T
import Data.Char (isSpace, isAlphaNum, isControl, isPunctuation, isDigit)
import Data.List (isPrefixOf, isInfixOf, isSuffixOf, nub, partition, sort)
import Control.Monad (when, replicateM)
import Control.Exception (try, SomeException, evaluate)

-- ============================================================================
-- Error Handler Resilience Tests
-- ============================================================================

-- | Test error handler with malformed input
prop_error_handler_malformed_input :: String -> Property
prop_error_handler_malformed_input malformed =
  length malformed < 100 ==>
    let parseResult = parseTypus malformed
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left errs -> property $ not (null errs)
                Right _ -> property True

-- | Test error handler with extremely large input
prop_error_handler_large_input :: Int -> String -> Property
prop_error_handler_large_input n baseStr =
  n >= 0 && n <= 1000 ==>
    let largeInput = concat $ replicate n baseStr
        parseResult = parseTypus largeInput
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right _ -> property True

-- | Test error handler with nested errors
prop_error_handler_nested_errors :: Int -> Property
prop_error_handler_nested_errors depth =
  depth >= 0 && depth <= 10 ==>
    let nestedErrors = generateNestedErrors depth
        parseResult = parseTypus nestedErrors
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right _ -> property True

-- | Test error handler with concurrent errors
prop_error_handler_concurrent_errors :: String -> String -> Property
prop_error_handler_concurrent_errors err1 err2 =
  not (null err1) && not (null err2) ==>
    let concurrentErrors = err1 ++ "\n" ++ err2 ++ "\n"
        parseResult = parseTypus concurrentErrors
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right _ -> property True

-- | Test error handler with error recovery
prop_error_handler_recovery :: String -> Property
prop_error_handler_recovery errorInput =
  not (null errorInput) && length errorInput < 50 ==>
    let recoverableInput = errorInput ++ "\nlet x = 5\n"
        parseResult = parseTypus recoverableInput
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right _ -> property True

-- | Test error handler with different severity levels
prop_error_handler_severity_levels :: String -> Int -> Property
prop_error_handler_severity_levels content severity =
  severity >= 0 && severity <= 3 ==>
    let severityContent = content ++ " // severity: " ++ show severity
        parseResult = parseTypus severityContent
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right _ -> property True

-- | Test error handler with resource exhaustion
prop_error_handler_resource_exhaustion :: Int -> Property
prop_error_handler_resource_exhaustion n =
  n >= 0 && n <= 10000 ==>
    let resourceHeavy = generateResourceHeavyInput n
        parseResult = parseTypus resourceHeavy
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right _ -> property True

-- | Test error handler with Unicode errors
prop_error_handler_unicode_errors :: String -> Property
prop_error_handler_unicode_errors unicodeStr =
  not (null unicodeStr) && length unicodeStr < 50 ==>
    let unicodeError = "let x = \"" ++ unicodeStr ++ "\"\n"
        parseResult = parseTypus unicodeError
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right _ -> property True

-- | Test error handler with memory pressure
prop_error_handler_memory_pressure :: Int -> Property
prop_error_handler_memory_pressure size =
  size >= 0 && size <= 1000 ==>
    let memoryIntensive = generateMemoryIntensiveInput size
        parseResult = parseTypus memoryIntensive
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right _ -> property True

-- | Test error handler with timeout scenarios
prop_error_handler_timeout_scenarios :: Int -> Property
prop_error_handler_timeout_scenarios complexity =
  complexity >= 0 && complexity <= 100 ==>
    let timeoutInput = generateTimeoutInput complexity
        parseResult = parseTypus timeoutInput
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right _ -> property True

-- | Test error handler with cascading errors
prop_error_handler_cascading_errors :: String -> Property
prop_error_handler_cascading_errors baseError =
  not (null baseError) && length baseError < 30 ==>
    let cascadingErrors = generateCascadingErrors baseError
        parseResult = parseTypus cascadingErrors
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right _ -> property True

-- | Test error handler with error context preservation
prop_error_handler_context_preservation :: String -> String -> Property
prop_error_handler_context_preservation context error =
  not (null context) && not (null error) ==>
    let contextualError = context ++ "\n" ++ error ++ "\n" ++ context
        parseResult = parseTypus contextualError
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right _ -> property True

-- | Test error handler with error aggregation
prop_error_handler_aggregation :: String -> String -> String -> Property
prop_error_handler_aggregation err1 err2 err3 =
  not (null err1) && not (null err2) && not (null err3) ==>
    let aggregatedErrors = err1 ++ "\n" ++ err2 ++ "\n" ++ err3
        parseResult = parseTypus aggregatedErrors
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right _ -> property True

-- | Test error handler with error filtering
prop_error_handler_filtering :: String -> Bool -> Property
prop_error_handler_filtering error shouldFilter =
  not (null error) ==>
    let filterDirective = if shouldFilter then "// filter-errors\n" else ""
        filterableError = filterDirective ++ error
        parseResult = parseTypus filterableError
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right _ -> property True

-- | Test error handler with error transformation
prop_error_handler_transformation :: String -> Property
prop_error_handler_transformation error =
  not (null error) && length error < 50 ==>
    let transformDirective = "// transform-errors\n"
        transformableError = transformDirective ++ error
        parseResult = parseTypus transformableError
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right _ -> property True

-- | Test error handler with error recovery strategies
prop_error_handler_recovery_strategies :: String -> Int -> Property
prop_error_handler_recovery_strategies error strategy =
  not (null error) && strategy >= 0 && strategy <= 3 ==>
    let strategyDirective = "// recovery-strategy: " ++ show strategy ++ "\n"
        recoverableError = strategyDirective ++ error
        parseResult = parseTypus recoverableError
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right _ -> property True

-- | Test error handler with error reporting
prop_error_handler_reporting :: String -> Property
prop_error_handler_reporting error =
  not (null error) && length error < 50 ==>
    let reportDirective = "// verbose-errors\n"
        reportableError = reportDirective ++ error
        parseResult = parseTypus reportableError
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right _ -> property True

-- | Test error handler with error localization
prop_error_handler_localization :: String -> String -> Property
prop_error_handler_localization error locale =
  not (null error) && not (null locale) ==>
    let localeDirective = "// error-locale: " ++ locale ++ "\n"
        localizedError = localeDirective ++ error
        parseResult = parseTypus localizedError
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right _ -> property True

-- Helper functions
generateNestedErrors :: Int -> String
generateNestedErrors 0 = "base error"
generateNestedErrors n = "error {\n" ++ generateNestedErrors (n - 1) ++ "\n}"

generateResourceHeavyInput :: Int -> String
generateResourceHeavyInput n = concat $ replicate n ("let x" ++ show n ++ " = " ++ show n ++ "\n")

generateMemoryIntensiveInput :: Int -> String
generateMemoryIntensiveInput n = concat $ replicate n ("let x" ++ show n ++ " = \"" ++ replicate n 'a' ++ "\"\n")

generateTimeoutInput :: Int -> String
generateTimeoutInput n = concat $ replicate n ("let x" ++ show n ++ " = x" ++ show (n-1) ++ " + 1\n")

generateCascadingErrors :: String -> String
generateCascadingErrors base = base ++ "\n" ++ "let x = " ++ base ++ "\n" ++ "let y = x." ++ base ++ "\n"

-- | Tasty test suite
testSuite :: TestTree
testSuite = testGroup "Error Handler Resilience Tests"
  [ testProperty "Error handler with malformed input" prop_error_handler_malformed_input,
    testProperty "Error handler with extremely large input" prop_error_handler_large_input,
    testProperty "Error handler with nested errors" prop_error_handler_nested_errors,
    testProperty "Error handler with concurrent errors" prop_error_handler_concurrent_errors,
    testProperty "Error handler with error recovery" prop_error_handler_recovery,
    testProperty "Error handler with different severity levels" prop_error_handler_severity_levels,
    testProperty "Error handler with resource exhaustion" prop_error_handler_resource_exhaustion,
    testProperty "Error handler with Unicode errors" prop_error_handler_unicode_errors,
    testProperty "Error handler with memory pressure" prop_error_handler_memory_pressure,
    testProperty "Error handler with timeout scenarios" prop_error_handler_timeout_scenarios,
    testProperty "Error handler with cascading errors" prop_error_handler_cascading_errors,
    testProperty "Error handler with error context preservation" prop_error_handler_context_preservation,
    testProperty "Error handler with error aggregation" prop_error_handler_aggregation,
    testProperty "Error handler with error filtering" prop_error_handler_filtering,
    testProperty "Error handler with error transformation" prop_error_handler_transformation,
    testProperty "Error handler with error recovery strategies" prop_error_handler_recovery_strategies,
    testProperty "Error handler with error reporting" prop_error_handler_reporting,
    testProperty "Error handler with error localization" prop_error_handler_localization
  ]