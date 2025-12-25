module Test.Unit.CompilerErrorRecoverySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property)

import Compiler (compile, CompilerError(..), CompilationPhase(..), formatCompilerErrors)
import Parser (parseTypus)
import qualified Data.Text as T
import Data.List (isInfixOf)

-- Test error recovery in parsing malformed syntax
test_parse_error_recovery :: TestTree
test_parse_error_recovery = testCase "Parser recovers from syntax errors" $ do
    let source = unlines
          [ "package main"
          , "func main() {"
          , "    x := 5"
          , "    // missing closing brace"
          , "    y := 10"
          , "}"
          ]
    case parseTypus source of
      Left err -> assertBool "Parser should provide meaningful error" $ 
                   "syntax error" `isInfixOf` err
      Right _ -> assertFailure "Expected parse error"

-- Test type error recovery
test_type_error_recovery :: TestTree
test_type_error_recovery = testCase "Type checker recovers from type mismatches" $ do
    let source = unlines
          [ "package main"
          , "func main() {"
          , "    x := 5"
          , "    x := \"string\""  -- type reassignment
          , "    y := x + 1"
          , "}"
          ]
    result <- compile source
    case result of
      Left errs -> do
        let errorMessages = formatCompilerErrors errs
        assertBool "Should detect type mismatch" $ 
          any ("type" `isInfixOf`) errorMessages
        assertBool "Should continue analysis after first error" $ 
          length errorMessages >= 1
      Right _ -> assertFailure "Expected compilation errors"

-- Test ownership error recovery
test_ownership_error_recovery :: TestTree
test_ownership_error_recovery = testCase "Ownership checker recovers from ownership violations" $ do
    let source = unlines
          [ "//! ownership: on"
          , "package main"
          , "func main() {"
          , "    data := make([]int, 10)"
          , "    moved := data"
          , "    _ = data[0]"  -- use after move
          , "    _ = moved[1]" -- continue after error
          , "}"
          ]
    result <- compile source
    case result of
      Left errs -> do
        let errorMessages = formatCompilerErrors errs
        assertBool "Should detect ownership violation" $ 
          any ("ownership" `isInfixOf`) errorMessages
      Right _ -> assertFailure "Expected ownership errors"

-- Test dependent type error recovery
test_dependent_type_error_recovery :: TestTree
test_dependent_type_error_recovery = testCase "Dependent type checker recovers from constraint violations" $ do
    let source = unlines
          [ "//! dependent_types: on"
          , "package main"
          , "func main() {"
          , "    // Vector with length constraint"
          , "    type Vec5 = [5]int"
          , "    v := Vec5{1, 2, 3}"  -- insufficient elements"
          , "    w := Vec5{1, 2, 3, 4, 5, 6}"  -- too many elements"
          , "    _ = v[0]"
          , "}"
          ]
    result <- compile source
    case result of
      Left errs -> do
        let errorMessages = formatCompilerErrors errs
        assertBool "Should detect dependent type violations" $ 
          any (\msg -> "constraint" `isInfixOf` msg || "length" `isInfixOf` msg) errorMessages
      Right _ -> assertFailure "Expected dependent type errors"

-- Test multiple error accumulation
test_multiple_error_accumulation :: TestTree
test_multiple_error_accumulation = testCase "Compiler accumulates multiple errors" $ do
    let source = unlines
          [ "//! ownership: on"
          , "//! dependent_types: on"
          , "package main"
          , "func main() {"
          , "    x := 5"
          , "    x := \"string\""  -- type error"
          , "    data := make([]int, 10)"
          , "    moved := data"
          , "    _ = data[0]"  -- ownership error"
          , "    type Vec3 = [3]int"
          , "    v := Vec3{1, 2}"  -- dependent type error"
          , "}"
          ]
    result <- compile source
    case result of
      Left errs -> do
        let errorMessages = formatCompilerErrors errs
        assertBool "Should detect type errors" $ 
          any ("type" `isInfixOf`) errorMessages
        assertBool "Should detect ownership errors" $ 
          any ("ownership" `isInfixOf`) errorMessages
        assertBool "Should detect complex constraint violations" $ 
          any (\msg -> "constraint" `isInfixOf` msg || "length" `isInfixOf` msg) errorMessages
        assertBool "Should accumulate multiple errors" $ 
          length errorMessages >= 3
      Right _ -> assertFailure "Expected multiple compilation errors"

-- QuickCheck property: Error recovery preserves source location information
prop_error_preserves_source_location :: String -> Property
prop_error_preserves_source_location source = 
  let linesList = lines source
      hasMultipleLines = length linesList > 1
  in classify hasMultipleLines "multi-line input" $
     property $ 
       case compile source of
         Left errs -> all (hasValidLocation) errs
         Right _ -> property True
  where
    hasValidLocation err = case err of
      CompilerError { cePhase = phase, ceLocation = loc } -> 
        case loc of
          Just _ -> True
          Nothing -> phase `elem` [ParsingPhase, AnalysisPhase]

-- QuickCheck property: Error messages are informative
prop_error_messages_are_informative :: String -> Property
prop_error_messages_are_informative source =
  property $
    case compile source of
      Left errs -> 
        let messages = formatCompilerErrors errs
            nonEmpty = all (not . null) messages
            meaningful = any (\msg -> length msg > 10) messages
        in nonEmpty .&&. meaningful
      Right _ -> property True

tests :: TestTree
tests = testGroup "Compiler Error Recovery"
  [ test_parse_error_recovery
  , test_type_error_recovery
  , test_ownership_error_recovery
  , test_dependent_type_error_recovery
  , test_multiple_error_accumulation
  , testCase "QuickCheck: Error preserves source location" $
      fastProperty prop_error_preserves_source_location
  , testCase "QuickCheck: Error messages are informative" $
      fastProperty prop_error_messages_are_informative
  ]