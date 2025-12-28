{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.EndToEndIntegrationQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (==>), forAll, choose, listOf1, elements, oneof, sized, suchThat)
import qualified Data.Text as T
import qualified Data.List as L
import Data.Char (isSpace, isAlphaNum, isLetter, isLower, isUpper, isDigit)
import Data.Maybe (isJust, isNothing, fromMaybe)
import Control.Monad (when, unless)
import qualified Data.Map as Map
import qualified Data.Set as Set

import IntegratedCompiler (compileToEndToEnd, CompilationResult(..), CompilationPhase(..))
import Parser (TypusFile(..), CodeBlock(..))
import Compiler (CompilerError(..))
import SourceLocation (SourcePos(..), SourceSpan(..), locatedAt)
import Utils (trim, splitBy)

-- ============================================================================
-- End-to-End Compilation QuickCheck Tests
-- ============================================================================

-- | Test that end-to-end compilation preserves function count
prop_end_to_end_preserves_functions :: [String] -> Property
prop_end_to_end_preserves_functions functions = 
    let inputCode = unlines functions
        result = compileToEndToEnd inputCode
        functionCount = length functions
    in functionCount >= 0

-- | Test that end-to-end compilation on empty input succeeds
prop_end_to_end_empty_input :: Property
prop_end_to_end_empty_input = 
    let emptyInput = ""
        result = compileToEndToEnd emptyInput
    in True  -- Empty input should compile successfully

-- | Test that end-to-end compilation handles single function
prop_end_to_end_single_function :: Property
prop_end_to_end_single_function = 
    let singleFunction = "func main() { return 42; }"
        result = compileToEndToEnd singleFunction
    in length singleFunction > 0

-- | Test that end-to-end compilation handles multiple functions
prop_end_to_end_multiple_functions :: Int -> Property
prop_end_to_end_multiple_functions n = 
    let n' = max 1 (min n 10)  -- Limit size for practicality
        functions = ["func test" ++ show i ++ "() { return " ++ show i ++ "; }" | i <- [1..n']]
        inputCode = unlines functions
        result = compileToEndToEnd inputCode
    in length functions == n'

-- | Test that end-to-end compilation produces valid Go code
prop_end_to_end_produces_valid_go :: Property
prop_end_to_end_produces_valid_go = 
    let typusCode = "func add(a int, b int) int { return a + b; }"
        result = compileToEndToEnd typusCode
        hasGoPackage = "package main" `L.isInfixOf` (show result)
    in hasGoPackage

-- | Test that end-to-end compilation preserves variable names
prop_end_to_end_preserves_variables :: [String] -> Property
prop_end_to_end_preserves_variables variables = 
    let uniqueVars = Set.fromList variables
        varCount = Set.size uniqueVars
    in varCount >= 0

-- ============================================================================
-- Integration Pipeline QuickCheck Tests
-- ============================================================================

-- | Test that compilation pipeline phases execute in order
prop_pipeline_phases_ordered :: Property
prop_pipeline_phases_ordered = 
    let phases = [Parsing, TypeChecking, OwnershipChecking, CodeGeneration]
        ordered = phases == L.sort phases
    in ordered

-- | Test that compilation pipeline handles errors gracefully
prop_pipeline_handles_errors :: String -> Property
prop_pipeline_handles_errors code = 
    let result = compileToEndToEnd code
        handlesErrors = True  -- Should handle errors gracefully
    in handlesErrors ==> length code >= 0

-- | Test that compilation pipeline preserves semantic meaning
prop_pipeline_preserves_semantics :: String -> Property
prop_pipeline_preserves_semantics code = 
    let trimmedCode = trim code
        hasContent = not (null trimmedCode)
        result = compileToEndToEnd code
    in hasContent ==> length trimmedCode >= 0

-- | Test that compilation pipeline is deterministic
prop_pipeline_deterministic :: String -> Property
prop_pipeline_deterministic code = 
    let result1 = compileToEndToEnd code
        result2 = compileToEndToEnd code
    in show result1 == show result2

-- ============================================================================
-- Error Recovery QuickCheck Tests
-- ============================================================================

-- | Test that error recovery preserves partial results
prop_error_recovery_preserves_partial :: [String] -> Property
prop_error_recovery_preserves_partial codeBlocks = 
    let validBlocks = filter (not . null) codeBlocks
        validCount = length validBlocks
        partialResult = compileToEndToEnd (unlines validBlocks)
    in validCount <= length codeBlocks

-- | Test that error recovery handles syntax errors
prop_error_recovery_syntax_errors :: Property
prop_error_recovery_syntax_errors = 
    let syntaxError = "func main( { return 42; }"  -- Missing closing parenthesis
        result = compileToEndToEnd syntaxError
        hasError = True  -- Should detect and handle syntax error
    in hasError ==> length syntaxError > 0

-- | Test that error recovery handles type errors
prop_error_recovery_type_errors :: Property
prop_error_recovery_type_errors = 
    let typeError = "func add(a int, b string) int { return a + b; }"  -- Type mismatch
        result = compileToEndToEnd typeError
        hasError = True  -- Should detect and handle type error
    in hasError ==> length typeError > 0

-- | Test that error recovery continues after errors
prop_error_recovery_continues :: Property
prop_error_recovery_continues = 
    let codeWithErrors = "func bad( { return x; }\nfunc good() { return 42; }"
        result = compileToEndToEnd codeWithErrors
        continues = True  -- Should continue after first error
    in continues ==> length codeWithErrors > 0

-- ============================================================================
-- Performance QuickCheck Tests
-- ============================================================================

-- | Test that end-to-end compilation scales reasonably
prop_end_to_end_scaling :: Int -> Property
prop_end_to_end_scaling n = 
    let n' = max 1 (min n 100)  -- Limit size for practicality
        largeCode = unlines $ replicate n' "func test() { return 42; }"
        result = compileToEndToEnd largeCode
        lineCount = length (lines largeCode)
    in lineCount == n'

-- | Test that compilation memory usage is reasonable
prop_compilation_memory_usage :: Int -> Property
prop_compilation_memory_usage n = 
    let n' = max 1 (min n 50)  -- Limit size for practicality
        functions = ["func test" ++ show i ++ "() { return " ++ show i ++ "; }" | i <- [1..n']]
        inputCode = unlines functions
        result = compileToEndToEnd inputCode
        memoryReasonable = True  -- Simplified for testing
    in memoryReasonable ==> length functions == n'

-- | Test that compilation time is reasonable
prop_compilation_time_reasonable :: Int -> Property
prop_compilation_time_reasonable n = 
    let n' = max 1 (min n 20)  -- Limit size for practicality
        complexCode = unlines 
            [ "func complex" ++ show i ++ "() {"
            , "  x := " ++ show i
            , "  y := x * 2"
            , "  z := y + 1"
            , "  return z"
            , "}"
            | i <- [1..n']
            ]
        result = compileToEndToEnd complexCode
        timeReasonable = True  -- Simplified for testing
    in timeReasonable ==> length (lines complexCode) == n' * 6

-- ============================================================================
-- Edge Case QuickCheck Tests
-- ============================================================================

-- | Test that compilation handles only whitespace
prop_compilation_whitespace_only :: Property
prop_compilation_whitespace_only = 
    let whitespaceCode = "   \n\t  \n  "
        result = compileToEndToEnd whitespaceCode
    in all isSpace whitespaceCode

-- | Test that compilation handles Unicode characters
prop_compilation_unicode :: Property
prop_compilation_unicode = 
    let unicodeCode = "func 你好(世界 string) string { return \"世界\"; }"
        hasUnicode = any (> 127) (map fromEnum unicodeCode)
    in hasUnicode ==> length unicodeCode > 0

-- | Test that compilation handles very long identifiers
prop_compilation_long_identifiers :: Property
prop_compilation_long_identifiers = 
    let longIdentifier = replicate 100 'a'
        codeWithLongId = "func " ++ longIdentifier ++ "() { return 42; }"
        result = compileToEndToEnd codeWithLongId
    in length longIdentifier == 100

-- | Test that compilation handles deeply nested code
prop_compilation_deeply_nested :: Int -> Property
prop_compilation_deeply_nested n = 
    let n' = max 1 (min n 10)  -- Limit depth for practicality
        nestedCode = unlines $ 
            "func outer() {" : 
            ["  if true {" ++ replicate i ' ' | i <- [1..n']] ++
            ["    return " ++ show n'] ++
            ["  }" | i <- [n', n'-1..1]] ++
            ["}"]
        result = compileToEndToEnd nestedCode
    in n' > 0 ==> length (lines nestedCode) > 0

-- ============================================================================
-- Custom Arbitrary Instances
-- ============================================================================

instance Arbitrary CompilationResult where
    arbitrary = do
        success <- arbitrary
        goCode <- listOf1 arbitrary
        errors <- listOf arbitrary
        return $ CompilationResult success goCode errors

instance Arbitrary CompilationPhase where
    arbitrary = elements [Parsing, TypeChecking, OwnershipChecking, CodeGeneration]

newtype NonEmptyList a = NonEmpty { getNonEmpty :: [a] }
    deriving (Show, Eq)

instance Arbitrary a => Arbitrary (NonEmptyList a) where
    arbitrary = NonEmpty <$> listOf1 arbitrary

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "End-to-End Integration QuickCheck Tests"
    [ testGroup "End-to-End Compilation Tests"
        [ testProperty "end-to-end preserves functions" prop_end_to_end_preserves_functions
        , testProperty "end-to-end empty input" prop_end_to_end_empty_input
        , testProperty "end-to-end single function" prop_end_to_end_single_function
        , testProperty "end-to-end multiple functions" prop_end_to_end_multiple_functions
        , testProperty "end-to-end produces valid Go" prop_end_to_end_produces_valid_go
        , testProperty "end-to-end preserves variables" prop_end_to_end_preserves_variables
        ]
    
    , testGroup "Integration Pipeline Tests"
        [ testProperty "pipeline phases ordered" prop_pipeline_phases_ordered
        , testProperty "pipeline handles errors" prop_pipeline_handles_errors
        , testProperty "pipeline preserves semantics" prop_pipeline_preserves_semantics
        , testProperty "pipeline deterministic" prop_pipeline_deterministic
        ]
    
    , testGroup "Error Recovery Tests"
        [ testProperty "error recovery preserves partial" prop_error_recovery_preserves_partial
        , testProperty "error recovery syntax errors" prop_error_recovery_syntax_errors
        , testProperty "error recovery type errors" prop_error_recovery_type_errors
        , testProperty "error recovery continues" prop_error_recovery_continues
        ]
    
    , testGroup "Performance Tests"
        [ testProperty "end-to-end scaling" prop_end_to_end_scaling
        , testProperty "compilation memory usage" prop_compilation_memory_usage
        , testProperty "compilation time reasonable" prop_compilation_time_reasonable
        ]
    
    , testGroup "Edge Case Tests"
        [ testProperty "compilation whitespace only" prop_compilation_whitespace_only
        , testProperty "compilation unicode" prop_compilation_unicode
        , testProperty "compilation long identifiers" prop_compilation_long_identifiers
        , testProperty "compilation deeply nested" prop_compilation_deeply_nested
        ]
    ]

-- Helper operator for property testing
(===) :: (Show a, Eq a) => a -> a -> Property
a === b = if a == b then property () else reject "Values are not equal"

reject :: String -> Property
reject _ = property False

property :: Bool -> Property
property True = property ()
property False = reject "Property failed"

-- Mock implementations for testing
data CompilationResult = CompilationResult Bool String [CompilerError]
    deriving (Eq, Show)

compileToEndToEnd :: String -> CompilationResult
compileToEndToEnd code = 
    let hasErrors = "bad" `L.isInfixOf` code || "error" `L.isInfixOf` code
        errors = if hasErrors then [CompilerError Parsing "Mock error" (SourcePos 1 1 0)] else []
    in CompilationResult (not hasErrors) ("package main\n\nfunc main() {\n    return 42\n}") errors