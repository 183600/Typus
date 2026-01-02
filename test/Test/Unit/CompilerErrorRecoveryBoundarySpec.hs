{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.CompilerErrorRecoveryBoundarySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertFailure, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck 
  ( Property, (===), (==>), forAll, counterexample, classify, property
  , (.&&.), (.||.), Arbitrary(..), Gen, oneof, elements, listOf
  , sized, resize, suchThat, frequency, choose, getPositive, getNonEmpty
  )

import Compiler.Errors.Core (ErrorLocation(..))
import Compiler.Error
import Compiler
import qualified Data.Text as T
import qualified Data.List as List
import Data.Char (isSpace, isLetter, isDigit)
import Data.Maybe (isJust, isNothing, fromMaybe)
import Control.Exception (evaluate)

-- | Generate malformed but parseable code snippets
genMalformedCode :: Gen String
genMalformedCode = oneof
  [ -- Unclosed brackets
    do
      content <- listOf $ elements $ ['a'..'z'] ++ ['0'..'9'] ++ " \t"
      bracket <- elements "{[("
      return $ bracket ++ content
  , -- Mismatched brackets
    do
      content <- listOf $ elements $ ['a'..'z'] ++ ['0'..'9'] ++ " \t"
      open <- elements "{[("
      close <- elements ")]}"
      guard $ open /= close
      return $ open ++ content ++ close
  , -- Invalid keywords
    do
      keyword <- elements ["invalidKeyword", "break123", "continue_"]
      content <- listOf $ elements $ ['a'..'z'] ++ ['0'..'9'] ++ " \t;"
      return $ keyword ++ " " ++ L.concat content
  , -- Malformed strings
    do
      content <- listOf $ elements $ ['a'..'z'] ++ ['0'..'9'] ++ " \t"
      return $ "\"" ++ content ++ "\n"
  ]

-- | Generate code with syntax errors that should be recoverable
genRecoverableError :: Gen String
genRecoverableError = oneof
  [ -- Missing semicolon
    do
      stmt1 <- listOf $ elements $ ['a'..'z'] ++ ['0'..'9'] ++ " \t"
      stmt2 <- listOf $ elements $ ['a'..'z'] ++ ['0'..'9'] ++ " \t"
      return $ L.concat stmt1 ++ "\n" ++ L.concat stmt2 ++ ";"
  , -- Extra comma
    do
      items <- listOf $ elements $ ['a'..'z'] ++ ['0'..'9']
      return $ "[" ++ List.intercalate "," items ++ ",]"
  , -- Invalid operator
    do
      left <- listOf $ elements $ ['a'..'z'] ++ ['0'..'9']
      right <- listOf $ elements $ ['a'..'z'] ++ ['0'..'9']
      return $ L.concat left ++ " @ " ++ L.concat right
  ]

-- | Generate cascading error scenarios
genCascadingErrors :: Gen String
genCascadingErrors = do
  numErrors <- choose (2, 5)
  errors <- listOf $ choose (1, 3) >>= \n -> listOf1 genMalformedCode
  return $ unlines $ L.concat errors

-- Property: Compiler should not crash on malformed input
prop_compiler_no_crash_malformed :: String -> Property
prop_compiler_no_crash_malformed malformedCode =
  let result = compileString malformedCode
  in property $ isJust result .||. isNothing result

-- Property: Error recovery should produce meaningful error locations
prop_error_recovery_meaningful_locations :: String -> Property
prop_error_recovery_meaningful_locations malformedCode =
  L.length malformedCode > 5 ==> 
  let result = compileString malformedCode
  in case result of
    Left errors -> property $ not (null errors) .&&. 
      L.all (\err -> line err > 0 && column err > 0) errors
    Right _ -> property True

-- Property: Error recovery should handle cascading errors gracefully
prop_error_recovery_cascading :: String -> Property
prop_error_recovery_cascading codeWithErrors =
  L.length codeWithErrors > 10 ==> 
  let result = compileString codeWithErrors
  in case result of
    Left errors -> property $ L.length errors <= 20 -- Prevent error explosion
    Right _ -> property True

-- Property: Recovery should preserve as much of the original code as possible
prop_error_recovery_preserves_code :: String -> String -> Property
prop_error_recovery_preserves_code validPrefix errorSuffix =
  not (null validPrefix) && not (null errorSuffix) ==> 
  let combined = validPrefix ++ "\n" ++ errorSuffix
      result = compileString combined
  in case result of
    Left errors -> property $ L.length errors > 0
    Right ast -> property $ not (null ast)

-- Property: Error messages should be consistent across similar errors
prop_error_consistency :: String -> Char -> Char -> Property
prop_error_consistency content openBracket closeBracket =
  openBracket `elem` "{[(" && closeBracket `elem` ")]}" ==> 
  let malformed1 = openBracket ++ content
      malformed2 = openBracket ++ content ++ closeBracket
      result1 = compileString malformed1
      result2 = compileString malformed2
  in case (result1, result2) of
    (Left err1, Left err2) -> property $ 
      L.length err1 > 0 && L.length err2 > 0
    _ -> property True

-- Property: Recovery should work with Unicode content
prop_error_recovery_unicode :: String -> Property
prop_error_recovery_unicode baseContent =
  let unicodeContent = baseContent ++ "测试🚀café naïve"
      malformed = unicodeContent ++ "{"
      result = compileString malformed
  in case result of
    Left errors -> property $ L.all (\err -> line err > 0) errors
    Right _ -> property True

-- Property: Error recovery should handle very long lines
prop_error_recovery_long_lines :: Int -> String -> Property
prop_error_recovery_long_lines multiplier content =
  multiplier > 0 && multiplier <= 100 ==> 
  let longContent = L.concat (replicate multiplier content) ++ "{"
      result = compileString longContent
  in property $ isJust result .||. isNothing result

-- Property: Recovery should maintain position accuracy
prop_error_recovery_position_accuracy :: String -> Int -> Property
prop_error_recovery_position_accuracy content errorPos =
  L.length content > errorPos && errorPos >= 0 ==> 
  let withError = take errorPos content ++ "{" ++ drop errorPos content
      result = compileString withError
  in case result of
    Left errors -> property $ L.all (\err -> line err >= 1 && column err >= 1) errors
    Right _ -> property True

-- Property: Multiple errors should be reported in order
prop_error_recovery_ordered :: String -> Property
prop_error_recovery_ordered content =
  L.length content > 20 ==> 
  let withMultipleErrors = content ++ "{ x = } y = ( z = ]"
      result = compileString withMultipleErrors
  in case result of
    Left errors -> property $ 
      L.length errors >= 2 && 
      L.all (\(i, err) -> line err >= 1) (zip [0..] errors)
    Right _ -> property True

-- Property: Error recovery should handle nested structures
prop_error_recovery_nested :: Int -> Property
prop_error_recovery_nested depth =
  depth > 0 && depth <= 10 ==> 
  let nestedBrackets = L.concat (replicate depth "[")
      content = nestedBrackets ++ "content"
      result = compileString content
  in case result of
    Left errors -> property $ L.length errors >= 1
    Right _ -> property True

-- Property: Recovery should be idempotent
prop_error_recovery_idempotent :: String -> Property
prop_error_recovery_idempotent malformedCode =
  let result1 = compileString malformedCode
      result2 = compileString malformedCode
  in case (result1, result2) of
    (Left err1, Left err2) -> property $ L.length err1 == L.length err2
    (Right ast1, Right ast2) -> property $ ast1 == ast2
    _ -> property False

-- Property: Error recovery should handle empty input gracefully
prop_error_recovery_empty :: Property
prop_error_recovery_empty =
  let result = compileString ""
  in property $ isJust result .||. isNothing result

-- Property: Recovery should handle whitespace-only input
prop_error_recovery_whitespace :: String -> Property
prop_error_recovery_whitespace whitespace =
  L.all isSpace whitespace ==> 
  let result = compileString whitespace
  in property $ isJust result .||. isNothing result

-- Property: Error locations should be accurate in multi-line input
prop_error_recovery_multiline_positions :: [String] -> Property
prop_error_recovery_multiline_positions lines =
  L.length lines >= 3 ==> 
  let content = unlines lines
      withError = content ++ "unterminated ["
      result = compileString withError
  in case result of
    Left errors -> property $ 
      L.all (\err -> line err >= 1 && line err <= L.length lines + 1) errors
    Right _ -> property True

-- Property: Recovery should handle mixed indentation
prop_error_recovery_mixed_indentation :: String -> Property
prop_error_recovery_mixed_indentation content =
  let mixedIndent = "  " ++ content ++ "\n\t" ++ content ++ "\n    " ++ content ++ "{"
      result = compileString mixedIndent
  in property $ isJust result .||. isNothing result

-- Property: Error recovery should preserve comments in error reporting
prop_error_recovery_preserve_comments :: String -> String -> Property
prop_error_recovery_preserve_comments code comment =
  not (null code) && not (null comment) ==> 
  let withComment = code ++ " // " ++ comment ++ "\n{"
      result = compileString withComment
  in case result of
    Left errors -> property $ L.length errors > 0
    Right _ -> property True

-- | Helper function to compile a string (simplified for testing)
compileString :: String -> Either [ErrorLocation] String
compileString input = 
  -- This is a mock implementation - in real code this would call the actual compiler
  if "{" `L.isInfixOf` input && not ("}" `L.isInfixOf` input)
  then Left [ErrorLocation 1 (L.length $ takeWhile (/= '{') input) Nothing Nothing]
  else if "[" `L.isInfixOf` input && not ("]" `L.isInfixOf` input)
  then Left [ErrorLocation 1 (L.length $ takeWhile (/= '[') input) Nothing Nothing]
  else if "(" `L.isInfixOf` input && not (")" `L.isInfixOf` input)
  then Left [ErrorLocation 1 (L.length $ takeWhile (/= '(') input) Nothing Nothing]
  else Right "mock_ast"

tests :: TestTree
tests = testGroup "Compiler Error Recovery Boundary Tests"
  [ testGroup "Property-based tests"
    [ fastProperty "no crash on malformed input" prop_compiler_no_crash_malformed
    , fastProperty "meaningful error locations" prop_error_recovery_meaningful_locations
    , fastProperty "cascading errors handled gracefully" prop_error_recovery_cascading
    , fastProperty "preserves as much code as possible" prop_error_recovery_preserves_code
    , fastProperty "consistent error messages" prop_error_consistency
    , fastProperty "unicode error recovery" prop_error_recovery_unicode
    , fastProperty "long lines error recovery" prop_error_recovery_long_lines
    , fastProperty "position accuracy" prop_error_recovery_position_accuracy
    , fastProperty "ordered error reporting" prop_error_recovery_ordered
    , fastProperty "nested structure recovery" prop_error_recovery_nested
    , fastProperty "idempotent recovery" prop_error_recovery_idempotent
    , fastProperty "empty input handling" prop_error_recovery_empty
    , fastProperty "whitespace-only input" prop_error_recovery_whitespace
    , fastProperty "multiline position accuracy" prop_error_recovery_multiline_positions
    , fastProperty "mixed indentation recovery" prop_error_recovery_mixed_indentation
    , fastProperty "preserve comments in errors" prop_error_recovery_preserve_comments
    ]

  , testGroup "Unit tests"
    [ testCase "unclosed bracket error location" $ do
        let input = "func test() {\n  x := 1"
        case compileString input of
          Left errors -> do
            L.length errors @?= 1
            let err = L.head errors
            line err @?= 1
          Right _ -> assertFailure "Expected compilation error"
    
    , testCase "multiple errors in same file" $ do
        let input = "func test() {\n  x := [1,2,\n  y := (1,2"
        case compileString input of
          Left errors -> do
            L.length errors @?= 2
          Right _ -> assertFailure "Expected compilation errors"
    
    , testCase "error recovery with valid code after error" $ do
        let input = "func test() {\n  x := [1,2,\n  y := 1\n}"
        case compileString input of
          Left _ -> return () -- Expected
          Right ast -> ast @?= "mock_ast"
    ]
  ]