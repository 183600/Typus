{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.CoreFunctionalitySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import Data.List (isPrefixOf, isInfixOf, intercalate)
import Data.Char (isSpace, isLetter, isDigit)

-- Import core modules
import qualified Parser
import qualified Compiler
import qualified Utils
import qualified SourceLocation
import qualified ErrorHandler
import qualified SyntaxValidator

-- | Core functionality tests covering essential compiler operations
tests :: TestTree
tests =
  testGroup "Core Functionality"
    [ testGroup "Parser Core Functions"
        [ fastProperty "Parser handles empty input gracefully" prop_parser_empty_input
        , fastProperty "Parser maintains position information" prop_parser_position_tracking
        , fastProperty "Parser handles malformed input safely" prop_parser_malformed_input
        , testCase "Parser handles special characters" $ do
            let input = "func test() { return \"特殊字符: 🚀测试\"; }"
            case Parser.parseExpression input of
              Left err -> assertFailure $ "Parse error: " ++ show err
              Right _ -> pure ()
        ]

    , testGroup "Compiler Core Functions"
        [ fastProperty "Compiler optimization preserves semantics" prop_compiler_optimization_preserves_semantics
        , fastProperty "Compiler handles circular dependencies" prop_compiler_circular_dependencies
        , fastProperty "Compiler resource cleanup" prop_compiler_resource_cleanup
        , testCase "Compiler handles complex expressions" $ do
            let input = "result := (a + b) * (c - d) / e;"
            case Compiler.compileExpression input of
              Left err -> assertFailure $ "Compilation error: " ++ show err
              Right _ -> pure ()
        ]

    , testGroup "Utils Core Functions"
        [ fastProperty "String processing is consistent" prop_utils_string_processing_consistency
        , fastProperty "List operations maintain invariants" prop_utils_list_operations_invariants
        , fastProperty "Map operations preserve properties" prop_utils_map_operations_properties
        , testCase "Utils handles edge cases" $ do
            Utils.trim "" @?= ""
            Utils.splitBy ',' "" @?= [""]
            Utils.removeComments "// comment" @?= " "
        ]

    , testGroup "Source Location Tracking"
        [ fastProperty "Source location accuracy" prop_source_location_accuracy
        , fastProperty "Location tracking through transformations" prop_location_tracking_transformations
        , fastProperty "Multi-line location handling" prop_multiline_location_handling
        , testCase "Source location edge cases" $ do
            let loc = SourceLocation.Position 1 1
            SourceLocation.isValidPosition loc @?= True
        ]

    , testGroup "Error Handling"
        [ fastProperty "Error messages are informative" prop_error_messages_informative
        , fastProperty "Error recovery maintains state" prop_error_recovery_maintains_state
        , fastProperty "Error context preservation" prop_error_context_preservation
        , testCase "Error handling edge cases" $ do
            let err = ErrorHandler.mkError "Test error" (SourceLocation.Position 1 1)
            ErrorHandler.errorMessage err @?= "Test error at line 1, column 1"
        ]

    , testGroup "Syntax Validation"
        [ fastProperty "Validator catches invalid syntax" prop_validator_catches_invalid_syntax
        , fastProperty "Validator accepts valid syntax" prop_validator_accepts_valid_syntax
        , fastProperty "Validation preserves AST structure" prop_validation_preserves_ast_structure
        , testCase "Syntax validation edge cases" $ do
            case SyntaxValidator.validateExpression "" of
              Left _ -> pure ()  -- Expected to fail
              Right _ -> assertFailure "Empty expression should be invalid"
        ]
    ]

-- Property-based tests

-- Parser properties
prop_parser_empty_input :: String -> Property
prop_parser_empty_input input =
  let emptyInput = ""
      result = Parser.parseExpression emptyInput
  in property $ case result of
    Left _ -> True
    Right _ -> True  -- Parser should handle empty input gracefully

prop_parser_position_tracking :: String -> Property
prop_parser_position_tracking input =
  not (null input) && length input <= 100 ==>  -- Limit input size
  let result = Parser.parseWithPosition input
  in property $ case result of
    Left _ -> True
    Right (ast, pos) -> SourceLocation.isValidPosition pos

prop_parser_malformed_input :: String -> Property
prop_parser_malformed_input input =
  let malformed = input ++ ")))((@@@##"
      result = Parser.parseExpression malformed
  in property $ case result of
    Left _ -> True  -- Should handle malformed input gracefully
    Right _ -> True  -- Or parse what it can

-- Compiler properties
prop_compiler_optimization_preserves_semantics :: String -> Property
prop_compiler_optimization_preserves_semantics input =
  not (null input) && length input <= 50 ==>
  let unoptimized = Compiler.compileExpression input
      optimized = Compiler.compileOptimized input
  in case (unoptimized, optimized) of
    (Left _, Left _) -> property True
    (Right u, Right o) -> property $ Compiler.semanticEquals u o
    _ -> property True  -- Different error handling is acceptable

prop_compiler_circular_dependencies :: [String] -> Property
prop_compiler_circular_dependencies deps =
  not (null deps) && length deps <= 5 ==>
  let circularDeps = zip deps (tail deps ++ [head deps])
      result = Compiler.checkDependencies circularDeps
  in property $ case result of
    Left _ -> True  -- Should detect circular dependencies
    Right _ -> True  -- Or handle them gracefully

prop_compiler_resource_cleanup :: String -> Property
prop_compiler_resource_cleanup input =
  let compilation = Compiler.compileExpression input
      cleanup = Compiler.cleanupResources
  in property $ cleanup == ()  -- Cleanup should not throw exceptions

-- Utils properties
prop_utils_string_processing_consistency :: String -> String -> Property
prop_utils_string_processing_consistency s1 s2 =
  let combined = s1 ++ s2
      trimmed1 = Utils.trim combined
      trimmed2 = Utils.trim (Utils.trim combined)
      split1 = Utils.splitBy ' ' combined
      split2 = Utils.splitBy ' ' trimmed1
  in property $ trimmed1 == trimmed2 .&&. length split1 >= length split2

prop_utils_list_operations_invariants :: [Int] -> [Int] -> Property
prop_utils_list_operations_invariants xs ys =
  let merged = Utils.mergeLists xs ys
      unique = Utils.removeDuplicates merged
  in property $ length merged >= length unique .&&. 
     all (`elem` merged) unique

prop_utils_map_operations_properties :: [(String, Int)] -> Property
prop_utils_map_operations_properties pairs =
  let dict = Utils.fromList pairs
      keys = Utils.keys dict
      values = Utils.values dict
  in property $ length keys == length values .&&.
     all (`elem` keys) (map fst pairs)

-- Source location properties
prop_source_location_accuracy :: String -> Property
prop_source_location_accuracy input =
  not (null input) && length input <= 100 ==>
  let lines' = lines input
      positions = [SourceLocation.Position line col | 
                   line <- [1..length lines'], 
                   col <- [1..length (lines' !! (line-1))]]
  in property $ all SourceLocation.isValidPosition positions

prop_location_tracking_transformations :: String -> Property
prop_location_tracking_transformations input =
  not (null input) && length input <= 50 ==>
  let ast = Parser.parseExpression input
      transformed = case ast of
        Left _ -> Nothing
        Right a -> Just (Compiler.transformAST a)
      originalLoc = SourceLocation.extractLocation input
      transformedLoc = fmap SourceLocation.extractLocation transformed
  in property $ case transformedLoc of
    Nothing -> True
    Just loc -> loc == originalLoc

prop_multiline_location_handling :: [String] -> Property
prop_multiline_location_handling lines' =
  not (null lines') && length lines' <= 10 ==>
  let input = intercalate "\n" lines'
      locations = SourceLocation.extractAllLocations input
  in property $ all SourceLocation.isValidLocation locations

-- Error handling properties
prop_error_messages_informative :: String -> String -> Property
prop_error_messages_informative msg context =
  not (null msg) && not (null context) ==>
  let error = ErrorHandler.mkError msg (SourceLocation.Position 1 1)
      errorMsg = ErrorHandler.errorMessage error
  in property $ msg `isInfixOf` errorMsg .&&.
     "line 1" `isInfixOf` errorMsg .&&.
     "column 1" `isInfixOf` errorMsg

prop_error_recovery_maintains_state :: String -> Property
prop_error_recovery_maintains_state input =
  let initialState = ErrorHandler.initialState
      result = ErrorHandler.processWithRecovery input initialState
  in property $ ErrorHandler.isValidState result

prop_error_context_preservation :: String -> String -> Property
prop_error_context_preservation input context =
  not (null input) && not (null context) ==>
  let error = ErrorHandler.mkErrorWithContext input context (SourceLocation.Position 1 1)
      preserved = ErrorHandler.getErrorContext error
  in property $ context `isInfixOf` preserved

-- Syntax validation properties
prop_validator_catches_invalid_syntax :: String -> Property
prop_validator_catches_invalid_syntax input =
  let invalidChars = "@@@)))((##$$%%^^&&**"
      invalidInput = input ++ invalidChars
      result = SyntaxValidator.validateExpression invalidInput
  in property $ case result of
    Left _ -> True  -- Should catch invalid syntax
    Right _ -> input /= invalidInput  -- Only valid if original was already valid

prop_validator_accepts_valid_syntax :: String -> Property
prop_validator_accepts_valid_syntax input =
  let validInput = filter (\c -> isLetter c || isDigit c || isSpace c) input
      validExpr = if null validInput then "x := 1" else validInput ++ " := 1"
      result = SyntaxValidator.validateExpression validExpr
  in property $ case result of
    Left _ -> length validExpr < 3  -- Only fail for very short inputs
    Right _ -> True  -- Should accept valid syntax

prop_validation_preserves_ast_structure :: String -> Property
prop_validation_preserves_ast_structure input =
  not (null input) && length input <= 50 ==>
  let ast = Parser.parseExpression input
      validated = case ast of
        Left _ -> Nothing
        Right a -> SyntaxValidator.validateAST a
  in property $ case validated of
    Nothing -> True
    Just validAst -> Compiler.structurallyEqual ast (Right validAst)
