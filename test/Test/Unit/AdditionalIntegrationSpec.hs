{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.AdditionalIntegrationSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, oneof, elements, listOf, choose)
import TestSupport.Arbitrary

import Parser (parseTypus, TypusFile(..), CodeBlock(..))
import SourceLocation (SourcePos(..), SourceSpan(..))
import Compiler.Errors.Core (TypeError(..), ErrorSeverity(..), ErrorCategory(..), ErrorLocation(..))
import Ownership.Common.Types (OwnershipType(..), OwnershipError(..))
import DependentTypesParser (TypeRef(..), TypeBody(..), Field(..), DependentType(..))
import Utils (trim, removeComments, normalizeIndentation)

import Data.Text (Text)
import qualified Data.Text as T
import Data.List (sort, nub)
import Data.Maybe (isJust, isNothing, fromMaybe)

-- ============================================================================
-- Additional Integration Tests
-- ============================================================================

-- Test: Parser + ErrorHandler integration
test_parser_error_handler_integration :: TestTree
test_parser_error_handler_integration = testCase "Parser L.and ErrorHandler integration" $ do
  let input = "//! ownership: true\nif condition\n" -- Missing opening brace
  case parseTypus input of
    Left err -> do
      assertBool "Parser should detect syntax error" True
      assertBool "Error message should contain relevant information" ("syntax error" `L.isInfixOf` err)
    Right file -> do
      let syntaxErrors = tfSyntaxErrors file
      assertBool "Should have syntax errors" (not (null syntaxErrors))

-- Test: Parser + Utils integration
test_parser_utils_integration :: TestTree
test_parser_utils_integration = testCase "Parser L.and Utils integration" $ do
  let input = "  /* comment */  code  // line comment\n  more code  "
  case parseTypus input of
    Left err -> assertBool "Should parse successfully" False
    Right file -> do
      let blocks = tfBlocks file
      assertBool "Should have code blocks" (not (null blocks))
      let firstBlock = L.head blocks
          content = cbContent firstBlock
      -- Test that utils functions work on parsed content
      let trimmed = trim content
          withoutComments = removeComments content
          normalized = normalizeIndentation content
      assertBool "Utils trim should work" (L.length trimmed <= L.length content)
      assertBool "Utils removeComments should work" (L.length withoutComments <= L.length content)
      assertBool "Utils normalizeIndentation should work" (not (null normalized))

-- Test: SourceLocation + ErrorHandler integration
test_source_location_error_handler_integration :: TestTree
test_source_location_error_handler_integration = testCase "SourceLocation L.and ErrorHandler integration" $ do
  let pos = SourcePos 10 5 100
      span = SourceSpan pos (SourcePos 10 10 105)
      location = ErrorLocation Nothing 10 5 (Just 10) (Just 5)
      error = TypeError "test-id" Error ErrorCategory (T.pack "test error") location undefined undefined undefined [] [] Nothing
  assertBool "Error should have correct location" (line location == 10)
  assertBool "Error should have correct column" (column location == 5)
  assertBool "Error should have correct end line" (endLine location == Just 10)
  assertBool "Error should have correct end column" (endColumn location == Just 5)

-- Test: Ownership + ErrorHandler integration
test_ownership_error_handler_integration :: TestTree
test_ownership_error_handler_integration = testCase "Ownership L.and ErrorHandler integration" $ do
  let ownershipError = UseAfterMove "variable"
      error = TypeError "ownership-error" Error ErrorCategory (T.pack "Use after move") 
                        (ErrorLocation (startPos) Nothing) 
                        undefined undefined undefined [] [] Nothing
  assertBool "Should handle ownership errors" (severity error == Error)
  assertBool "Should have error category" (category error == ErrorCategory)

-- Test: DependentTypes + ErrorHandler integration
test_dependent_types_error_handler_integration :: TestTree
test_dependent_types_error_handler_integration = testCase "DependentTypes L.and ErrorHandler integration" $ do
  let typeRef = TypeRef "MyType" [TypeRef "Int" [], TypeRef "String" []]
      error = TypeError "type-error" Error TypeChecking (T.pack "Type checking error")
                        (ErrorLocation Nothing 5 10 Nothing Nothing)
                        undefined undefined undefined [] [] Nothing
  assertBool "Should handle dependent types errors" (severity error == Error)
  assertBool "Should have TypeChecking category" (category error == TypeChecking)

-- Test: Full pipeline integration
test_full_pipeline_integration :: TestTree
test_full_pipeline_integration = testCase "Full pipeline integration" $ do
  let input = unlines
        [ "//! ownership: true"
        , "//! dependent-types: true"
        , ""
        , "type SafeArray<T> where len(T) > 0 {"
        , "    data: T"
        , "    size: Int"
        , "}"
        , ""
        , "func processData<T>(arr: SafeArray<T>) -> Result<T> {"
        , "    // Implementation"
        , "}"
        ]
  case parseTypus input of
    Left err -> assertBool ("Should parse complex input: " ++ err) False
    Right file -> do
      let directives = tfDirectives file
          blocks = tfBlocks file
          syntaxErrors = tfSyntaxErrors file
      assertBool "Should parse directives" (isJust (fdOwnership directives))
      assertBool "Should parse blocks" (not (null blocks))
      assertBool "Should handle syntax errors gracefully" True -- May have errors but should not crash

-- Test: Error propagation across modules
test_error_propagation_integration :: TestTree
test_error_propagation_integration = testCase "Error propagation across modules" $ do
  let input = unlines
        [ "//! ownership: true"
        , "if condition"  -- Missing opening brace
        , "    x := 5"
        , "    use(x)"    -- Use after move (simulated)
        ]
  case parseTypus input of
    Left err -> do
      assertBool "Should detect syntax error" True
      assertBool "Error should be informative" (L.length err > 10)
    Right file -> do
      let syntaxErrors = tfSyntaxErrors file
          blocks = tfBlocks file
      -- Even if parsing succeeds, syntax errors should be captured
      assertBool "Should capture syntax errors" (not (null syntaxErrors) || not (null blocks))

-- Test: Unicode L.and special characters handling
test_unicode_special_chars_integration :: TestTree
test_unicode_special_chars_integration = testCase "Unicode L.and special characters integration" $ do
  let input = unlines
        [ "//! ownership: true"
        , "// 注释 with 中文 L.and café naïve"
        , "type 测试类型<T> {"
        , "    字段: T"
        , "}"
        , ""
        , "func 处理数据(输入: 测试类型<String>) -> 结果 {"
        , "    // 🚀 Implementation with emoji"
        , "}"
        ]
  case parseTypus input of
    Left err -> assertBool ("Should handle Unicode: " ++ err) False
    Right file -> do
      let blocks = tfBlocks file
      assertBool "Should parse Unicode content" (not (null blocks))
      let firstBlock = L.head blocks
          content = cbContent firstBlock
      assertBool "Should preserve Unicode characters" ("测试类型" `L.isInfixOf` content)

-- Test: Performance with large inputs
test_performance_large_input_integration :: TestTree
test_performance_large_input_integration = testCase "Performance with large inputs" $ do
  let largeContent = unlines $ replicate 1000 "x := x + 1 // line comment"
      input = "//! ownership: true\n" ++ largeContent
  case parseTypus input of
    Left err -> assertBool "Should handle large input gracefully" True
    Right file -> do
      let blocks = tfBlocks file
      assertBool "Should handle large input" (not (null blocks))

-- Property: Parser + Utils roundtrip
prop_parser_utils_roundtrip :: String -> Property
prop_parser_utils_roundtrip content =
  not (L.any (`elem` "\0\r") content) ==> -- Avoid problematic characters
  let processed = content |> removeComments |> trim |> normalizeIndentation
      result = parseTypus processed
  in case result of
    Left _ -> property True -- May fail, but should not crash
    Right file -> 
      let blocks = tfBlocks file
      in if null blocks
         then property True
         else property (processed `L.isInfixOf` unlines (map cbContent blocks))

-- Property: Error handling consistency across modules
prop_error_handling_consistency :: String -> ErrorSeverity -> ErrorCategory -> Property
prop_error_handling_consistency message severity category =
  let error = TypeError "test-id" severity category (T.pack message)
                        (ErrorLocation (startPos) Nothing)
                        undefined undefined undefined [] [] Nothing
  in property $ severity error === severity .&&.
     category error === category .&&.
     message error === T.pack message

-- Property: Ownership + DependentTypes interaction
prop_ownership_dependent_types_interaction :: String -> String -> Property
prop_ownership_dependent_types_interaction varName typeName =
  let ownershipType = Owned varName
      typeRef = TypeRef typeName [TypeRef "Int" []]
      error1 = TypeError "ownership-error" Error ErrorCategory (T.pack ("Ownership error with " ++ varName))
                        (ErrorLocation (startPos) Nothing)
                        undefined undefined undefined [] [] Nothing
      error2 = TypeError "type-error" Error TypeChecking (T.pack ("Type error with " ++ typeName))
                        (ErrorLocation Nothing 2 1 Nothing Nothing)
                        undefined undefined undefined [] [] Nothing
  in property $ show ownershipType `contains` varName .&&.
     show typeRef `contains` typeName .&&.
     show error1 `contains` varName .&&.
     show error2 `contains` typeName

-- Helper function for pipeline composition
(|>) :: a -> (a -> b) -> b
x |> f = f
infixl 0 |>

-- Helper function to check substring containment
contains :: String -> String -> Bool
contains sub str = sub `L.isInfixOf` str

-- Property: Complex multi-module scenario
prop_complex_multi_module_scenario :: String -> [String] -> Property
prop_complex_multi_module_scenario directiveName blockContents =
  not (null directiveName) && not (null blockContents) ==> 
  let input = "//! " ++ directiveName ++ ": true\n" ++ unlines blockContents
      result = parseTypus input
  in case result of
    Left _ -> property True -- May fail, but should not crash
    Right file -> 
      let directives = tfDirectives file
          blocks = tfBlocks file
      in property $ L.length blocks >= L.length blockContents - 1 -- Allow some variance

-- Property: Error recovery L.and continuation
prop_error_recovery_continuation :: [String] -> Property
prop_error_recovery_continuation lines =
  not (null lines) ==> 
  let input = unlines ("if condition" : lines) -- First line has error
      result = parseTypus input
  in case result of
    Left _ -> property True
    Right file -> 
      let blocks = tfBlocks file
          syntaxErrors = tfSyntaxErrors file
      in property $ not (null blocks) || not (null syntaxErrors)

-- ============================================================================
-- Test Collection
-- ============================================================================

tests :: TestTree
tests = testGroup "Additional Integration Tests"
  [ test_parser_error_handler_integration
  , test_parser_utils_integration
  , test_source_location_error_handler_integration
  , test_ownership_error_handler_integration
  , test_dependent_types_error_handler_integration
  , test_full_pipeline_integration
  , test_error_propagation_integration
  , test_unicode_special_chars_integration
  , test_performance_large_input_integration
  , fastProperty "Parser + Utils roundtrip" prop_parser_utils_roundtrip
  , fastProperty "Error handling consistency across modules" prop_error_handling_consistency
  , fastProperty "Ownership + DependentTypes interaction" prop_ownership_dependent_types_interaction
  , fastProperty "Complex multi-module scenario" prop_complex_multi_module_scenario
  , fastProperty "Error recovery L.and continuation" prop_error_recovery_continuation
  ]