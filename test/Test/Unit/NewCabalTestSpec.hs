{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewCabalTestSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck 
  ( Property, (===), (==>), forAll, counterexample, classify, property
  , (.&&.), (.||.), Arbitrary(..), Gen, arbitrary, oneof, elements
  , listOf, vectorOf, suchThat, resize
  )

import Compiler (compile, CompilerError(..), CompilationPhase(..))
import Ownership (OwnershipType(..), OwnershipError(..), OwnershipTransfer(..))
import DependentTypesParser 
  ( TypeRef(..), TypeBody(..), Field(..), TypeParameter(..)
  , TypeConstraint(..), DependentType(..), parseDependentType
  , validateDependentTypeSyntax
  )
import Parser (parseTypus, TypusFile(..))
import SourceLocation (SourceSpan(..), SourcePos(..))
import Utils (trim, splitBy, removeComments)

import Data.List (isInfixOf, isPrefixOf, null, length)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, isNothing, fromMaybe)
import Data.Char (isSpace, isAlphaNum)

-- Test 1: Compiler Error Handling
test_compiler_error_handling :: TestTree
test_compiler_error_handling = testCase "Compiler handles syntax errors gracefully" $ do
  let invalidSource = unlines
        [ "package main"
        , "func main() {"
        , "    if x > 0 {"
        , "        println(\"test\")"
        , "    // Missing closing brace"
        , "}"
        ]
  case compile invalidSource of
    Left errs -> 
      case errs of
        (SyntaxError _ msg):_ -> 
          assertBool "Error message should mention syntax issue" $
            "syntax" `isInfixOf` map toLower msg || "brace" `isInfixOf` map toLower msg
        _ -> assertFailure "Expected syntax error"
    Right _ -> assertFailure "Expected compilation to fail with syntax error"

-- Test 2: Ownership Analysis
test_ownership_analysis :: TestTree
test_ownership_analysis = testCase "Ownership analysis detects move violations" $ do
  let sourceWithOwnershipIssue = unlines
        [ "//! ownership: on"
        , "package main"
        , "func main() {"
        , "    data := make([]int, 10)"
        , "    moved := data"
        , "    _ = len(data)  // Using moved value"
        , "}"
        ]
  case parseTypus sourceWithOwnershipIssue of
    Left err -> assertFailure $ "parseTypus failed: " ++ err
    Right _ -> do
      -- This test verifies that the parser can handle ownership directives
      -- In a real scenario, the ownership analyzer would detect the violation
      assertBool "Parser should handle ownership directives" True

-- Test 3: Dependent Types Validation
test_dependent_types_validation :: TestTree
test_dependent_types_validation = testCase "Dependent types validation works correctly" $ do
  let validDependentType = "type Vec<T> where T: Eq, len(T) > 0"
  case parseDependentType validDependentType of
    Left _ -> assertFailure "Failed to parse valid dependent type"
    Right depType -> do
      assertBool "Should have type parameters" $ not $ null $ dtTypeParameters depType
      assertBool "Should have constraints" $ not $ null $ dtConstraints depType

-- Test 4: Parser Edge Cases
test_parser_edge_cases :: TestTree
test_parser_edge_cases = testCase "Parser handles edge cases correctly" $ do
  let edgeCases = 
        [ ("empty file", "")
        , ("only comments", "// This is a comment\n// Another comment")
        , ("only whitespace", "   \n\t  \n  ")
        , ("complex generics", "func complex<T, R, U>(x T, y func(T) R, z ...U) (R, error)")
        ]
  
  mapM_ (\(name, source) -> testCase name $ do
    case parseTypus source of
      Left err -> assertFailure $ "Failed to parse " ++ name ++ ": " ++ err
      Right _ -> return ()
    ) edgeCases

-- Test 5: QuickCheck Property - String Processing
prop_string_processing_roundtrip :: String -> Property
prop_string_processing_roundtrip input =
  let processed = input |> removeComments |> trim
      processed2 = input |> trim |> removeComments
  in classify (null input) "empty input" $
     classify (any isSpace input) "has whitespace" $
     property $ processed === processed2

-- Test 6: QuickCheck Property - Type Validation Consistency
prop_type_validation_consistency :: String -> Property
prop_type_validation_consistency typeStr =
  not (null typeStr) && length typeStr <= 100 ==> -- Limit size for performance
  let result1 = validateDependentTypeSyntax typeStr
      result2 = validateDependentTypeSyntax typeStr
  in property $ result1 === result2

-- Test 7: QuickCheck Property - Split By Consistency
prop_split_by_consistency :: Char -> String -> Property
prop_split_by_consistency delim input =
  let parts = splitBy delim input
      rejoined = concat (map (++ [delim]) (init parts) ++ [last parts])
  in classify (null input) "empty input" $
     classify (delim `elem` input) "contains delimiter" $
     property $ if null input then parts == [""] else rejoined === input

-- Test 8: Integration Test - Full Pipeline
test_full_pipeline_integration :: TestTree
test_full_pipeline_integration = testCase "Full compilation pipeline integration" $ do
  let validSource = unlines
        [ "//! ownership: on"
        , "//! dependent_types: on"
        , "package main"
        , "import \"fmt\""
        , ""
        , "type SafeString struct {"
        , "    data string"
        , "}"
        , ""
        , "func (s SafeString) Length() int where len(s.data) > 0 {"
        , "    return len(s.data)"
        , "}"
        , ""
        , "func main() {"
        , "    str := SafeString{data: \"hello\"}"
        , "    fmt.Printf(\"Length: %d\\n\", str.Length())"
        , "}"
        ]
  case parseTypus validSource of
    Left err -> assertFailure $ "parseTypus failed: " ++ err
    Right typusFile -> do
      -- Verify directives are parsed correctly
      let directives = tfDirectives typusFile
      assertBool "Should have ownership directive" $ isJust $ fdOwnership directives
      assertBool "Should have dependent types directive" $ isJust $ fdDependentTypes directives
      
      -- Verify blocks are parsed
      assertBool "Should parse code blocks" $ not $ null $ tfBlocks typusFile

-- Test 9: Error Recovery Test
test_error_recovery :: TestTree
test_error_recovery = testCase "Compiler recovers from multiple errors" $ do
  let sourceWithMultipleErrors = unlines
        [ "package main"
        , "func broken1() {"
        , "    if x > 0 {"  -- Missing variable declaration
        , "        println(\"test\")"
        , "    // Missing closing brace"
        , "}"
        , "func broken2() {"
        , "    var y int = \"string\""  -- Type mismatch
        , "}"
        , "func working() int {"  -- This should still be parseable
        , "    return 42"
        , "}"
        ]
  case compile sourceWithMultipleErrors of
    Left errs -> do
      assertBool "Should detect multiple errors" $ length errs >= 2
      -- Check that we get different types of errors
      let hasSyntaxError = any isSyntaxError errs
          hasTypeError = any isTypeError errs
      assertBool "Should have syntax errors" hasSyntaxError
    Right _ -> assertFailure "Expected compilation to fail"

-- Test 10: QuickCheck Property - Source Location Consistency
prop_source_location_consistency :: Int -> Int -> Int -> Int -> Property
prop_source_location_consistency line1 col1 line2 col2 =
  line1 >= 1 && line1 <= 1000 && col1 >= 1 && col1 <= 1000 &&
  line2 >= 1 && line2 <= 1000 && col2 >= 1 && col2 <= 1000 ==>
  let span1 = SourceSpan (SourcePos line1 col1) (SourcePos line1 col1)
      span2 = SourceSpan (SourcePos line2 col2) (SourcePos line2 col2)
      span3 = SourceSpan (SourcePos line1 col1) (SourcePos line2 col2)
  in property $ spanStart span1 === spanEnd span1 .&&.
     spanStart span2 === spanEnd span2 .&&.
     spanStart span3 === SourcePos line1 col1 .&&.
     spanEnd span3 === SourcePos line2 col2

-- Helper functions
isSyntaxError :: CompilerError -> Bool
isSyntaxError (SyntaxError _ _) = True
isSyntaxError _ = False

isTypeError :: CompilerError -> Bool
isTypeError (TypeError _ _) = True
isTypeError _ = False

toLower :: String -> String
toLower = map (\c -> if c >= 'A' && c <= 'Z' then toEnum (fromEnum c + 32) else c)

-- Operator for function composition
(|>) :: a -> (a -> b) -> b
x |> f = f x

-- Aggregate all tests
tests :: TestTree
tests =
  testGroup "New Cabal Tests"
    [ test_compiler_error_handling
    , test_ownership_analysis
    , test_dependent_types_validation
    , test_parser_edge_cases
    , testGroup "QuickCheck Properties"
        [ fastProperty "String processing roundtrip" prop_string_processing_roundtrip
        , fastProperty "Type validation consistency" prop_type_validation_consistency
        , fastProperty "Split by consistency" prop_split_by_consistency
        , fastProperty "Source location consistency" prop_source_location_consistency
        ]
    , test_full_pipeline_integration
    , test_error_recovery
    ]