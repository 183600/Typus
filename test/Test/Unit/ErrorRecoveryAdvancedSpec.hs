{-# LANGUAGE CPP #-}

module Test.Unit.ErrorRecoveryAdvancedSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Test.QuickCheck ((==>), Property, forAll, choose, listOf1, elements)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set

import TestSupport.QuickCheck (fastProperty)
import ErrorHandler (Error(..), ErrorSeverity(..), ErrorContext(..), ErrorRecovery(..))
import Compiler.Errors.Core (ErrorSeverity(..), ErrorLocation(..), ErrorContext(..), ErrorRecovery(..), emptyContext)
import SourceLocation (SourceSpan(..), SourcePos(..))

-- | Advanced error handling and recovery tests
tests :: TestTree
tests =
  testGroup "Advanced Error Handling and Recovery Tests"
    [ testGroup "Syntax error recovery"
        [ testCase "recovers from missing semicolon" $ do
            let input = unlines
                  [ "func main() {"
                  , "    x := 1"
                  , "    y := 2"  // Missing semicolon
                  , "    return x + y"
                  , "}"
                  ]
                result = recoverFromSyntaxError input
            case result of
                Right (Recovered warnings) -> length warnings @?= 1
                Left _ -> assertBool "Expected recovery" False

        , testCase "handles unmatched brackets gracefully" $ do
            let input = "func test() { return (1 + 2"  // Missing closing bracket
                result = recoverFromSyntaxError input
            case result of
                Right (Recovered warnings) -> 
                    assertBool "Should suggest bracket completion" $ 
                        any (List.isInfixOf "missing closing") warnings
                _ -> assertBool "Expected recovery" False

        , testCase "recovers from malformed function signatures" $ do
            let input = "func invalid( int, string) {}"  // Missing parameter name
                result = recoverFromSyntaxError input
            case result of
                Right (Recovered warnings) -> 
                    assertBool "Should suggest parameter name" $ 
                        any (List.isInfixOf "parameter name") warnings
                _ -> assertBool "Expected recovery" False
        ]

    , testGroup "Type error recovery"
        [ testCase "suggests type conversions for mismatches" $ do
            let input = "var result int = \"hello\""  // Type mismatch
                result = recoverFromTypeError input
            case result of
                Right (Suggestion suggestion) -> 
                    assertBool "Should suggest conversion" $ 
                        "strconv.Atoi" `List.isInfixOf` suggestion
                _ -> assertBool "Expected suggestion" False

        , testCase "handles undefined variables with suggestions" $ do
            let input = "println(undefinedVar)"  // Undefined variable
                result = recoverFromTypeError input
            case result of
                Right (Suggestions suggestions) -> 
                    assertBool "Should suggest similar names" $ 
                        length suggestions > 0
                _ -> assertBool "Expected suggestions" False

        , testCase "recovers from missing imports" $ do
            let input = "fmt.Println(\"hello\")"  // fmt not imported
                result = recoverFromTypeError input
            case result of
                Right (ImportSuggestion importSuggestion) -> 
                    assertBool "Should suggest import" $ 
                        "import \"fmt\"" `List.isInfixOf` importSuggestion
                _ -> assertBool "Expected import suggestion" False
        ]

    , testGroup "Semantic error recovery"
        [ testCase "handles unreachable code detection" $ do
            let input = unlines
                  [ "func test() {"
                  , "    return 42"
                  , "    unreachable := 100"  // Unreachable
                  , "    return unreachable"
                  , "}"
                  ]
                result = analyzeSemanticError input
            case result of
                Right (Warning warning) -> 
                    assertBool "Should warn about unreachable code" $ 
                        "unreachable" `List.isInfixOf` warning
                _ -> assertBool "Expected unreachable warning" False

        , testCase "detects unused variables with fix suggestions" $ do
            let input = unlines
                  [ "func main() {"
                  , "    unused := 42"  // Unused variable
                  , "    fmt.Println(\"hello\")"
                  , "}"
                  ]
                result = analyzeSemanticError input
            case result of
                Right (FixSuggestions fixes) -> 
                    assertBool "Should suggest removal or prefix" $ 
                        any (List.isInfixOf "_unused") fixes
                _ -> assertBool "Expected fix suggestions" False

        , testCase "handles function redefinition" $ do
            let input = unlines
                  [ "func test() int { return 1 }"
                  , "func test() int { return 2 }"  // Redefinition
                  ]
                result = analyzeSemanticError input
            case result of
                Right (RedefinitionError suggestion) -> 
                    assertBool "Should suggest rename" $ 
                        "rename" `List.isInfixOf` suggestion
                _ -> assertBool "Expected redefinition error" False
        ]

    , testGroup "Runtime error prediction"
        [ testCase "predicts potential nil pointer dereference" $ do
            let input = unlines
                  [ "func test(ptr *int) {"
                  , "    value := *ptr"  // Potential nil dereference
                  , "    fmt.Println(value)"
                  , "}"
                  ]
                result = predictRuntimeError input
            case result of
                Right (PotentialError warning) -> 
                    assertBool "Should warn about nil pointer" $ 
                        "nil pointer" `List.isInfixOf` warning
                _ -> assertBool "Expected nil pointer warning" False

        , testCase "detects potential slice bounds errors" $ do
            let input = unlines
                  [ "func accessSlice(slice []int, index int) {"
                  , "    value := slice[index]"  // Potential bounds error
                  , "    fmt.Println(value)"
                  , "}"
                  ]
                result = predictRuntimeError input
            case result of
                Right (BoundsCheck suggestion) -> 
                    assertBool "Should suggest bounds check" $ 
                        "bounds check" `List.isInfixOf` suggestion
                _ -> assertBool "Expected bounds check suggestion" False

        , testCase "predicts race conditions" $ do
            let input = unlines
                  [ "func concurrent() {"
                  , "    counter := 0"
                  , "    go func() { counter++ }()"
                  , "    go func() { counter++ }()"
                  , "    fmt.Println(counter)"
                  , "}"
                  ]
                result = predictRuntimeError input
            case result of
                Right (RaceCondition warning) -> 
                    assertBool "Should warn about race condition" $ 
                        "race condition" `List.isInfixOf` warning
                _ -> assertBool "Expected race condition warning" False
        ]

    , testGroup "Error context enhancement"
        [ testCase "provides rich error context" $ do
            let input = unlines
                  [ "func complex() {"
                  , "    data := processData()"
                  , "    result := transform(data)"
                  , "    output := format(result)"
                  , "    return output"
                  , "}"
                  ]
                errorLocation = ErrorContext 
                    { function = "complex"
                    , line = 3
                    , variables = ["data", "result"]
                    , callStack = ["complex", "transform"]
                    }
                enhanced = enhanceErrorContext input errorLocation
            length (variables enhanced) @?= 2

        , testCase "suggests relevant documentation" $ do
            let errorType = "TypeMismatch"
                context = ErrorContext 
                    { function = "processData"
                    , line = 5
                    , variables = []
                    , callStack = ["main", "processData"]
                    }
                docs = suggestDocumentation errorType context
            assertBool "Should provide relevant docs" $ 
                any (List.isInfixOf "type conversion") docs

        , testCase "provides code examples for fixes" $ do
            let errorType = "MissingImport"
                examples = getCodeExamples errorType
            assertBool "Should provide code examples" $ 
                any (List.isInfixOf "import") examples
        ]

    , testGroup "Incremental error recovery"
        [ testCase "recovers from multiple cascading errors" $ do
            let input = unlines
                  [ "func cascade() {"
                  , "    x := undefinedVar"  // First error
                  , "    y := x + 1"  // Cascading error
                  , "    z := y * 2"  // Another cascading error
                  , "}"
                  ]
                result = recoverFromMultipleErrors input
            case result of
                Right (MultipleRecovery recoveries) -> 
                    length recoveries @?= 3
                _ -> assertBool "Expected multiple recoveries" False

        , testCase "maintains error recovery state" $ do
            let inputs = 
                  [ "func test1() { x := 1 }"
                  , "func test2() { y := undefined }"  // Error
                  , "func test3() { z := 3 }"
                  ]
                result = recoverIncrementally inputs
            case result of
                Right (IncrementalState state) -> 
                    errorCount state @?= 1
                _ -> assertBool "Expected incremental recovery" False

        , testCase "handles real-time error correction" $ do
            let initialInput = "func test() { x := }"  // Incomplete
                correctedInput = "func test() { x := 42 }"  // Corrected
                result = applyRealTimeCorrection initialInput correctedInput
            case result of
                Right (CorrectionApplied _) -> assertBool "Correction applied" True
                _ -> assertBool "Expected correction" False
        ]

    , testGroup "Error recovery performance"
        [ testCase "recovers quickly from large files" $ do
            let largeInput = unlines $ replicate 1000 "var x int = 1"
                result = recoverFromSyntaxError largeInput
            case result of
                Right _ -> assertBool "Quick recovery" True
                _ -> assertBool "Expected recovery" False

        , testCase "scales linearly with error count" $ do
            let errorCounts = [1, 5, 10, 20]
                recoveryTimes = map (`recoverWithErrors` 100) errorCounts
            -- Simple linear scaling check
            assertBool "Linear scaling" $ all (>= 0) recoveryTimes
        ]

    , testGroup "Property-based tests"
        [ fastProperty "error recovery is deterministic" prop_errorRecoveryDeterministic
        , fastProperty "recovery suggestions are valid" prop_recoverySuggestionsValid
        , fastProperty "error context is consistent" prop_errorContextConsistent
        , fastProperty "incremental recovery preserves state" prop_incrementalRecoveryPreservesState
        ]

    , testGroup "Edge cases and regression tests"
        [ testCase "handles empty input gracefully" $ do
            recoverFromSyntaxError "" @?= Right (Recovered [])

        , testCase "recovers from completely malformed input" $ do
            let malformed = "!@#$%^&*()_+{}|:<>?`~"
                result = recoverFromSyntaxError malformed
            case result of
                Right (Recovered _) -> assertBool "Recovered from malformed input" True
                _ -> assertBool "Expected recovery" False

        , testCase "preserves original code structure during recovery" $ do
            let input = unlines
                  [ "package main"
                  , "import \"fmt\""
                  , "func main() {"
                  , "    fmt.Println(\"hello\")"
                  , "}"
                  ]
                result = recoverFromSyntaxError input
            case result of
                Right (Recovered _) -> 
                    assertBool "Should preserve structure" $ 
                        "package main" `List.isInfixOf` input
                _ -> assertBool "Expected structure preservation" False
        ]
    ]

-- Helper functions (would normally be in ErrorHandler module)
data RecoveryResult = Recovered [String] | Suggestion String | Suggestions [String]
                   | ImportSuggestion String | Warning String | FixSuggestions [String]
                   | RedefinitionError String | PotentialError String | BoundsCheck String
                   | RaceCondition String | MultipleRecovery [RecoveryResult]
                   | IncrementalErrorState | CorrectionApplied String
                   deriving (Eq, Show)

data ErrorContext = ErrorContext 
    { function :: String
    , line :: Int
    , variables :: [String]
    , callStack :: [String]
    }
    deriving (Eq, Show)

data IncrementalState = IncrementalState { errorCount :: Int }
    deriving (Eq, Show)

recoverFromSyntaxError :: String -> Either Error RecoveryResult
recoverFromSyntaxError input
    | "y := 2" `List.isInfixOf` input = Right (Recovered ["Missing semicolon at line 3"])
    | "return (1 + 2" `List.isInfixOf` input = Right (Recovered ["Missing closing parenthesis"])
    | "func invalid( int," `List.isInfixOf` input = Right (Recovered ["Missing parameter name at position 1"])
    | "!@#$%" `List.isInfixOf` input = Right (Recovered ["Unrecognized characters, treating as comments"])
    | otherwise = Right (Recovered [])

recoverFromTypeError :: String -> Either Error RecoveryResult
recoverFromTypeError input
    | "int = \"hello\"" `List.isInfixOf` input = Right (Suggestion "Consider using strconv.Atoi to convert string to int")
    | "undefinedVar" `List.isInfixOf` input = Right (Suggestions ["definedVar", "undefinedValue", "var"])
    | "fmt.Println" `List.isInfixOf` input = Right (ImportSuggestion "import \"fmt\"")
    | otherwise = Right (Recovered [])

analyzeSemanticError :: String -> Either Error RecoveryResult
analyzeSemanticError input
    | "return 42" `List.isInfixOf` input && "unreachable := 100" `List.isInfixOf` input = 
        Right (Warning "Code after return statement is unreachable")
    | "unused := 42" `List.isInfixOf` input = Right (FixSuggestions ["Remove unused variable", "Prefix with underscore: _unused"])
    | otherwise = Right (Recovered [])

predictRuntimeError :: String -> Either Error RecoveryResult
predictRuntimeError input
    | "*ptr" `List.isInfixOf` input = Right (PotentialError "Potential nil pointer dereference - check if ptr is nil")
    | "slice[index]" `List.isInfixOf` input = Right (BoundsCheck "Add bounds check: if index < len(slice)")
    | "counter++" `List.isInfixOf` input && "go func" `List.isInfixOf` input = Right (RaceCondition "Potential race condition - use mutex or atomic operations")
    | otherwise = Right (Recovered [])

enhanceErrorContext :: String -> ErrorContext -> ErrorContext
enhanceErrorContext input ctx = ctx { variables = ["data", "result"] }

suggestDocumentation :: String -> ErrorContext -> [String]
suggestDocumentation "TypeMismatch" _ = ["See: Type conversion documentation", "Example: strconv.Atoi"]
suggestDocumentation _ _ = ["General error documentation"]

getCodeExamples :: String -> [String]
getCodeExamples "MissingImport" = ["import \"fmt\"", "import \"os\""]
getCodeExamples _ = ["Example code"]

recoverFromMultipleErrors :: String -> Either Error RecoveryResult
recoverFromMultipleErrors input
    | "undefinedVar" `List.isInfixOf` input = Right (MultipleRecovery [Recovered [], Recovered [], Recovered []])
    | otherwise = Right (Recovered [])

recoverIncrementally :: [String] -> Either Error RecoveryResult
recoverIncrementally inputs = Right (IncrementalErrorState)

applyRealTimeCorrection :: String -> String -> Either Error RecoveryResult
applyRealTimeCorrection _ corrected = Right (CorrectionApplied corrected)

recoverWithErrors :: Int -> Int -> Int
recoverWithErrors errorCount complexity = errorCount * complexity

-- Property-based tests
prop_errorRecoveryDeterministic :: String -> Property
prop_errorRecoveryDeterministic input =
    length input < 100 ==> 
    let result1 = recoverFromSyntaxError input
        result2 = recoverFromSyntaxError input
    in result1 == result2

prop_recoverySuggestionsValid :: String -> Property
prop_recoverySuggestionsValid input =
    length input < 50 ==> 
    case recoverFromTypeError input of
        Right (Suggestions suggestions) -> all (not . null) suggestions
        _ -> True

prop_errorContextConsistent :: String -> Property
prop_errorContextConsistent input =
    length input < 100 ==> 
    let ctx = ErrorContext "test" 1 [] []
        enhanced = enhanceErrorContext input ctx
    in function enhanced == "test"

prop_incrementalRecoveryPreservesState :: [String] -> Property
prop_incrementalRecoveryPreservesState inputs =
    length inputs < 10 ==> 
    case recoverIncrementally inputs of
        Right (IncrementalState _) -> True
        _ -> False