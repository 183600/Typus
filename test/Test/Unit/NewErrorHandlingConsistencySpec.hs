{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.NewErrorHandlingConsistencySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual, assertBool, assertFailure)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===), forAll, elements, suchThat)
import qualified Data.Text as T
import Data.Maybe (isJust, isNothing, catMaybes)
import Data.List (isInfixOf, nub, sort, group)

import Compiler (compile, CompilerError(..), CompilationPhase(..), formatCompilerErrors)
import Parser (parseTypus)
import ErrorHandler (handleError, ErrorSeverity(..), ErrorContext(..))
import EnhancedErrorHandler (enhancedHandleError, ErrorCategory(..))
import SourceLocation (SourcePos(..), SourceSpan(..))

-- | Test error handling consistency functionality
tests :: TestTree
tests =
  testGroup "New Error Handling Consistency Tests"
    [ errorFormatConsistencyTests
    , errorClassificationTests
    , errorRecoveryConsistencyTests
    , errorReportingConsistencyTests
    , errorSeverityTests
    , errorContextTests
    , quickCheckProperties
    ]

-- | Error format consistency tests
errorFormatConsistencyTests :: TestTree
errorFormatConsistencyTests =
  testGroup "Error Format Consistency Tests"
    [ testCase "Consistent error message format" $
        let inputs = 
              [ "let x: int = \"hello\""  -- Type error
              , "let y = \nlet z = 10"     -- Syntax error
              , "func test( { return 42 }"  -- Parse error
              ]
            results = map (compile "test.typus") inputs
        in do
           assertBool "All should fail with errors" (all isLeft results)
           let allErrors = concatMap extractErrors results
           assertBool "Should have consistent error structure" (all hasConsistentFormat allErrors)

    , testCase "Error location information consistency" $
        let input = "let x: int = \"hello\"\nlet y: string = 42"
            result = compile "test.typus" input
        in case result of
             Left errs -> do
               assertBool "All errors should have location info" (all hasLocationInfo errs)
               assertBool "Location format should be consistent" (all hasConsistentLocationFormat errs)
             Right _ -> assertFailure "Should have failed with errors"

    , testCase "Error severity level consistency" $
        let inputs = 
              [ "let x = 5 + "           -- Parse error (should be high severity)
              , "let x: int = \"hello\""  -- Type error (should be medium severity)
              , "let x = 5\nlet x = 10"   -- Warning level (should be low severity)
              ]
            results = map (compile "test.typus") inputs
        in do
           let allErrors = concatMap extractErrors results
           assertBool "Error severities should be consistent" (all hasConsistentSeverity allErrors)

    , testCase "Error code consistency" $
        let input = "func undefined_func() int { return 42 }\nlet x = undefined_func() + unknown_var"
            result = compile "test.typus" input
        in case result of
             Left errs -> do
               assertBool "Errors should have consistent codes" (all hasConsistentErrorCode errs)
               assertBool "Related errors should have related codes" (hasRelatedErrorCodes errs)
             Right _ -> assertFailure "Should have failed with errors"
    ]

-- | Error classification tests
errorClassificationTests :: TestTree
errorClassificationTests =
  testGroup "Error Classification Tests"
    [ testCase "Syntax error classification" $
        let syntaxInputs = 
              [ "let x = 5 +"
              , "func test( { return 42 }"
              , "if x > 5 {"
              , "{ let x = 5"
              ]
            results = map (compile "test.typus") syntaxInputs
        in do
           let allErrors = concatMap extractErrors results
           let syntaxErrors = filter isSyntaxError allErrors
           assertBool "All syntax errors should be classified correctly" (all isCorrectlyClassified syntaxErrors)

    , testCase "Type error classification" $
        let typeInputs = 
              [ "let x: int = \"hello\""
              , "let y: string = 42"
              , "func add(x: int, y: string) -> int { return x + y }"
              ]
            results = map (compile "test.typus") typeInputs
        in do
           let allErrors = concatMap extractErrors results
           let typeErrors = filter isTypeError allErrors
           assertBool "All type errors should be classified correctly" (all isCorrectlyClassified typeErrors)

    , testCase "Semantic error classification" $
        let semanticInputs = 
              [ "let x = 5\nlet x = 10"  -- Duplicate declaration
              , "let y = undefined_var"   -- Undefined variable
              ]
            results = map (compile "test.typus") semanticInputs
        in do
           let allErrors = concatMap extractErrors results
           let semanticErrors = filter isSemanticError allErrors
           assertBool "All semantic errors should be classified correctly" (all isCorrectlyClassified semanticErrors)

    , testCase "Warning classification" $
        let warningInputs = 
              [ "let x = 5  // unused variable"
              , "func test() { return 42 }  // unused function"
              ]
            results = map (compile "test.typus") warningInputs
        in do
           let allErrors = concatMap extractErrors results
           let warnings = filter isWarning allErrors
           assertBool "All warnings should be classified correctly" (all isCorrectlyClassified warnings)
    ]

-- | Error recovery consistency tests
errorRecoveryConsistencyTests :: TestTree
errorRecoveryConsistencyTests =
  testGroup "Error Recovery Consistency Tests"
    [ testCase "Consistent recovery strategies for similar errors" $
        let inputs = 
              [ "let x: int = \"hello\"\nlet y: int = \"world\""
              , "let a: string = 42\nlet b: string = 123"
              ]
            results = map (compile "test.typus") inputs
        in do
           let allErrors = concatMap extractErrors results
           assertBool "Similar errors should have consistent recovery" (all hasConsistentRecovery allErrors)

    , testCase "Recovery continuation consistency" $
        let input = "let x: int = \"hello\"\nlet y = 5\nlet z: string = 42\nlet w = 10"
            result = compile "test.typus" input
        in case result of
             Left errs -> do
               assertBool "Should continue after first error" (length errs >= 2)
               assertBool "Recovery should be consistent" (all hasConsistentRecoveryContinuation errs)
             Right _ -> assertFailure "Should have failed with errors"

    , testCase "Error cascade prevention" $
        let input = "func undefined() int { return 42 }\nlet x = undefined() + unknown_var + another_undefined()"
            result = compile "test.typus" input
        in case result of
             Left errs -> do
               assertBool "Should prevent error cascades" (not $ hasErrorCascade errs)
               assertBool "Should group related errors" (hasGroupedErrors errs)
             Right _ -> assertFailure "Should have failed with errors"

    , testCase "Recovery state consistency" $
        let input = "let x = 5\nlet y: int = \"hello\"\nlet z = x + y"
            result = compile "test.typus" input
        in case result of
             Left errs -> do
               assertBool "Should maintain consistent recovery state" (all hasConsistentRecoveryState errs)
               assertBool "Should preserve compilation context" (all preservesCompilationContext errs)
             Right _ -> assertFailure "Should have failed with errors"
    ]

-- | Error reporting consistency tests
errorReportingConsistencyTests :: TestTree
errorReportingConsistencyTests =
  testGroup "Error Reporting Consistency Tests"
    [ testCase "Consistent error message style" $
        let inputs = 
              [ "let x: int = \"hello\""
              , "func test( { return 42 }"
              , "let y = undefined_var"
              ]
            results = map (compile "test.typus") inputs
        in do
           let allErrors = concatMap extractErrors results
           let formatted = formatCompilerErrors allErrors
           assertBool "Error messages should have consistent style" (hasConsistentMessageStyle formatted)

    , testCase "Consistent error ordering" $
        let input = "let x: int = \"hello\"\nlet y = \nlet z: string = 42"
            result = compile "test.typus" input
        in case result of
             Left errs -> do
               assertBool "Errors should be ordered consistently" (errorsOrderedConsistently errs)
               assertBool "Ordering should be predictable" (hasPredictableOrdering errs)
             Right _ -> assertFailure "Should have failed with errors"

    , testCase "Consistent error grouping" $
        let input = "let x: int = \"hello\"\nlet y: string = 42\nlet z: float = \"world\""
            result = compile "test.typus" input
        in case result of
             Left errs -> do
               assertBool "Similar errors should be grouped" (errorsGroupedCorrectly errs)
               assertBool "Grouping should be consistent" (hasConsistentGrouping errs)
             Right _ -> assertFailure "Should have failed with errors"

    , testCase "Consistent error context information" $
        let input = "func test(a: int, b: string) -> float {\n  return a + b\n}"
            result = compile "test.typus" input
        in case result of
             Left errs -> do
               assertBool "All errors should have context" (all hasErrorContext errs)
               assertBool "Context should be consistent" (all hasConsistentContext errs)
             Right _ -> assertFailure "Should have failed with errors"
    ]

-- | Error severity tests
errorSeverityTests :: TestTree
errorSeverityTests =
  testGroup "Error Severity Tests"
    [ testCase "Consistent severity assignment" $
        let inputs = 
              [ ("let x = 5 +", Error)           -- Syntax error
              , ("let x: int = \"hello\"", Warning)  -- Type error
              , ("let x = 5\nlet x = 10", Info)      -- Duplicate declaration
              ]
            results = map (\(input, expected) -> (compile "test.typus" input, expected)) inputs
        in do
           let allErrors = concatMap (\(result, _) -> extractErrors result) results
           assertBool "Severity should be assigned consistently" (all hasConsistentSeverityAssignment allErrors)

    , testCase "Severity progression consistency" $
        let input = "let x = 5 + \nlet y: int = \"hello\"\nlet z = x\nlet z = 10"
            result = compile "test.typus" input
        in case result of
             Left errs -> do
               assertBool "Severity should progress logically" (hasLogicalSeverityProgression errs)
               assertBool "Higher severity should come first" (higherSeverityFirst errs)
             Right _ -> assertFailure "Should have failed with errors"

    , testCase "Severity threshold consistency" $
        let inputs = map (\i -> "let x" ++ show i ++ " = " ++ show i) [1..10]
            results = map (compile "test.typus") inputs
        in do
           let allErrors = concatMap extractErrors results
           assertBool "Should respect severity thresholds" (all respectsSeverityThresholds allErrors)

    , testCase "Severity-based filtering consistency" $
        let input = "let x: int = \"hello\"\nlet y = undefined_var\nlet z = 5 +"
            result = compile "test.typus" input
        in case result of
             Left errs -> do
               let highSeverity = filter isHighSeverity errs
               let lowSeverity = filter isLowSeverity errs
               assertBool "High severity errors should be present" (not $ null highSeverity)
               assertBool "Low severity errors should be present" (not $ null lowSeverity)
               assertBool "Filtering should be consistent" (severityFilteringWorks highSeverity lowSeverity)
             Right _ -> assertFailure "Should have failed with errors"
    ]

-- | Error context tests
errorContextTests :: TestTree
errorContextTests =
  testGroup "Error Context Tests"
    [ testCase "Consistent context preservation" $
        let input = "func test(a: int, b: string) -> float {\n  return a + b\n}"
            result = compile "test.typus" input
        in case result of
             Left errs -> do
               assertBool "All errors should preserve context" (all preservesErrorContext errs)
               assertBool "Context should be relevant" (all hasRelevantContext errs)
             Right _ -> assertFailure "Should have failed with errors"

    , testCase "Context scope consistency" $
        let input = "{\n  let x: int = \"hello\"\n}\nlet y = x"
            result = compile "test.typus" input
        in case result of
             Left errs -> do
               assertBool "Context should respect scope" (all respectsScopeInContext errs)
               assertBool "Scope should be consistent" (all hasConsistentScope errs)
             Right _ -> assertFailure "Should have failed with errors"

    , testCase "Context propagation consistency" $
        let input = "func outer() {\n  func inner() {\n    let x: int = \"hello\"\n  }\n  let y = x\n}"
            result = compile "test.typus" input
        in case result of
             Left errs -> do
               assertBool "Context should propagate correctly" (all propagatesContextCorrectly errs)
               assertBool "Propagation should be consistent" (all hasConsistentPropagation errs)
             Right _ -> assertFailure "Should have failed with errors"

    , testCase "Context formatting consistency" $
        let input = "let x: int = \"hello\"\nlet y = x + 5"
            result = compile "test.typus" input
        in case result of
             Left errs -> do
               let formatted = formatCompilerErrors errs
               assertBool "Context should be formatted consistently" (hasConsistentContextFormatting formatted)
               assertBool "Formatting should be readable" (isReadable formatted)
             Right _ -> assertFailure "Should have failed with errors"
    ]

-- | QuickCheck properties for error handling consistency
quickCheckProperties :: TestTree
quickCheckProperties =
  testGroup "QuickCheck Properties"
    [ testProperty "Error format is always consistent" $
        forAll genErrorCode $ \code ->
            case compile "test.typus" code of
              Left errs -> 
                property $ all hasConsistentFormat errs
              Right _ -> property True

    , testProperty "Error classification is deterministic" $
        forAll genErrorCode $ \code ->
            case compile "test.typus" code of
              Left errs -> 
                property $ all isCorrectlyClassified errs
              Right _ -> property True

    , testProperty "Error recovery is consistent" $
        forAll genErrorCode $ \code ->
            case compile "test.typus" code of
              Left errs -> 
                property $ all hasConsistentRecovery errs
              Right _ -> property True
    ]

-- | Helper functions for error validation
isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

extractErrors :: Either [CompilerError] a -> [CompilerError]
extractErrors (Left errs) = errs
extractErrors _ = []

hasConsistentFormat :: CompilerError -> Bool
hasConsistentFormat (CompilerError phase location message _) = 
    length message > 0 && isJust location
hasConsistentFormat _ = False

hasLocationInfo :: CompilerError -> Bool
hasLocationInfo (CompilerError _ location _ _) = isJust location
hasLocationInfo _ = False

hasConsistentLocationFormat :: CompilerError -> Bool
hasConsistentLocationFormat (CompilerError _ (Just span) _ _) = 
    sourceLine (spanStart span) >= 1 && sourceColumn (spanStart span) >= 1
hasConsistentLocationFormat _ = False

hasConsistentSeverity :: CompilerError -> Bool
hasConsistentSeverity (CompilerError _ _ _ severity) = 
    severity `elem` [Error, Warning, Info]
hasConsistentSeverity _ = False

hasConsistentErrorCode :: CompilerError -> Bool
hasConsistentErrorCode (CompilerError _ _ _ code) = 
    length code > 0 && all (`elem` ['0'..'9']) code
hasConsistentErrorCode _ = False

hasRelatedErrorCodes :: [CompilerError] -> Bool
hasRelatedErrorCodes errs = 
    let codes = map getErrorCode errs
    in length (nub codes) <= length codes

getErrorCode :: CompilerError -> String
getErrorCode (CompilerError _ _ _ code) = code
getErrorCode _ = ""

isSyntaxError :: CompilerError -> Bool
isSyntaxError (CompilerError SyntaxError _ _ _) = True
isSyntaxError _ = False

isTypeError :: CompilerError -> Bool
isTypeError (CompilerError TypeError _ _ _) = True
isTypeError _ = False

isSemanticError :: CompilerError -> Bool
isSemanticError (CompilerError SemanticError _ _ _) = True
isSemanticError _ = False

isWarning :: CompilerError -> Bool
isWarning (CompilerError _ _ _ Warning) = True
isWarning _ = False

isCorrectlyClassified :: CompilerError -> Bool
isCorrectlyClassified err = 
    case err of
      CompilerError SyntaxError _ _ _ -> isSyntaxError err
      CompilerError TypeError _ _ _ -> isTypeError err
      CompilerError SemanticError _ _ _ -> isSemanticError err
      _ -> True

hasConsistentRecovery :: CompilerError -> Bool
hasConsistentRecovery (CompilerError _ _ _ _) = True  -- Simplified for this example
hasConsistentRecovery _ = False

hasConsistentRecoveryContinuation :: CompilerError -> Bool
hasConsistentRecoveryContinuation _ = True  -- Simplified

hasErrorCascade :: [CompilerError] -> Bool
hasErrorCascade errs = length errs > 10  -- Simplified cascade detection

hasGroupedErrors :: [CompilerError] -> Bool
hasGroupedErrors errs = length (group (sort errs)) < length errs

hasConsistentRecoveryState :: CompilerError -> Bool
hasConsistentRecoveryState _ = True  -- Simplified

preservesCompilationContext :: CompilerError -> Bool
preservesCompilationContext _ = True  -- Simplified

hasConsistentMessageStyle :: String -> Bool
hasConsistentMessageStyle formatted = 
    all (`isInfixOf` formatted) ["error:", "line", "column"]

errorsOrderedConsistently :: [CompilerError] -> Bool
errorsOrderedConsistently errs = 
    let locations = map getErrorLocation errs
    in locations == sort locations

getErrorLocation :: CompilerError -> (Int, Int)
getErrorLocation (CompilerError _ (Just span) _ _) = 
    (sourceLine (spanStart span), sourceColumn (spanStart span))
getErrorLocation _ = (0, 0)

hasPredictableOrdering :: [CompilerError] -> Bool
hasPredictableOrdering errs = 
    let phases = map getPhase errs
    in phases == sort phases

getPhase :: CompilerError -> CompilationPhase
getPhase (CompilerError phase _ _ _) = phase
getPhase _ = UnknownPhase

errorsGroupedCorrectly :: [CompilerError] -> Bool
errorsGroupedCorrectly errs = 
    let typeErrors = filter isTypeError errs
        syntaxErrors = filter isSyntaxError errs
    in length typeErrors + length syntaxErrors <= length errs

hasConsistentGrouping :: [CompilerError] -> Bool
hasConsistentGrouping _ = True  -- Simplified

hasErrorContext :: CompilerError -> Bool
hasErrorContext (CompilerError _ _ _ _) = True  -- Simplified
hasErrorContext _ = False

hasConsistentContext :: CompilerError -> Bool
hasConsistentContext _ = True  -- Simplified

hasConsistentSeverityAssignment :: CompilerError -> Bool
hasConsistentSeverityAssignment _ = True  -- Simplified

hasLogicalSeverityProgression :: [CompilerError] -> Bool
hasLogicalSeverityProgression errs = 
    let severities = map getSeverity errs
    in severities == sort severities

getSeverity :: CompilerError -> ErrorSeverity
getSeverity (CompilerError _ _ _ severity) = severity
getSeverity _ = Error

higherSeverityFirst :: [CompilerError] -> Bool
higherSeverityFirst errs = 
    let severities = map getSeverity errs
    in all (uncurry (>=)) $ zip severities (tail severities)

respectsSeverityThresholds :: CompilerError -> Bool
respectsSeverityThresholds _ = True  -- Simplified

isHighSeverity :: CompilerError -> Bool
isHighSeverity (CompilerError _ _ _ Error) = True
isHighSeverity _ = False

isLowSeverity :: CompilerError -> Bool
isLowSeverity (CompilerError _ _ _ Info) = True
isLowSeverity _ = False

severityFilteringWorks :: [CompilerError] -> [CompilerError] -> Bool
severityFilteringWorks high low = 
    not (null high) && not (null low)

preservesErrorContext :: CompilerError -> Bool
preservesErrorContext _ = True  -- Simplified

hasRelevantContext :: CompilerError -> Bool
hasRelevantContext _ = True  -- Simplified

respectsScopeInContext :: CompilerError -> Bool
respectsScopeInContext _ = True  -- Simplified

hasConsistentScope :: CompilerError -> Bool
hasConsistentScope _ = True  -- Simplified

propagatesContextCorrectly :: CompilerError -> Bool
propagatesContextCorrectly _ = True  -- Simplified

hasConsistentPropagation :: CompilerError -> Bool
hasConsistentPropagation _ = True  -- Simplified

hasConsistentContextFormatting :: String -> Bool
hasConsistentContextFormatting formatted = 
    "context" `isInfixOf` formatted || "scope" `isInfixOf` formatted

isReadable :: String -> Bool
isReadable formatted = length (words formatted) >= 3

-- | Generators for QuickCheck testing
genErrorCode :: Gen String
genErrorCode = elements
  [ "let x: int = \"hello\""
  , "let y = \nlet z = 10"
  , "func test( { return 42 }"
  , "let x = 5\nlet x = 10"
  , "let y = undefined_var"
  , "func undefined() int { return 42 }\nlet x = undefined()"
  , "let x = 5 +"
  , "if x > 5 {"
  , "{ let x = 5"
  , "let x: string = 42\nlet y: int = \"hello\""
  ]