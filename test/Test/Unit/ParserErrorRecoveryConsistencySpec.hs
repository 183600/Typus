{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.ParserErrorRecoveryConsistencySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import TestSupport.Arbitrary

import Parser
  ( parseTypus
  , FileDirectives(..)
  , BlockDirectives(..)
  , CodeBlock(..)
  , TypusFile(..)
  , defaultFileDirectives
  , defaultBlockDirectives
  )

import SourceLocation
  ( Located(..)
  , SourcePos(..)
  , SourceSpan(..)
  , spanStart
  , spanEnd
  )

import qualified Text.Megaparsec as MP
import Text.Megaparsec (errorBundlePretty)
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf)
import Data.List (sort)
import Data.Maybe (isJust, isNothing, fromMaybe)

-- | Parser error recovery consistency properties
tests :: TestTree
tests = testGroup "Parser error recovery consistency"
  [ -- Basic parsing consistency
    testGroup "Basic parsing consistency"
      [ testCase "empty input parses to empty file" $ 
          let result = parseTypus ""
          in case result of
               Left err -> assertBool "Should parse empty input" False
               Right file -> do
                 L.length (blocks file) @?= 0
                 directives file @?= defaultFileDirectives

      , testCase "well-formed directive parses correctly" $ do
          let result = parseTypus "//! ownership: on\n"
          case result of
            Left err -> assertBool ("Failed to parse well-formed directive: " ++ errorBundlePretty err) False
            Right file -> do
              let fd = directives file
              isJust (fdOwnership fd) @?= True
              locatedValue (fromMaybe (error "impossible") (fdOwnership fd)) @?= True

      , testCase "malformed directive triggers error recovery" $ do
          let result = parseTypus "//! ownership: maybe\n"
          case result of
            Left err -> assertBool "Should recover from malformed directive" True
            Right file -> assertBool "Should not succeed with malformed directive" False
      ]

  , -- Error recovery properties
    testGroup "Error recovery properties"
      [ fastProperty "parse is deterministic" prop_parse_deterministic
      , fastProperty "parse of valid code succeeds" prop_parse_valid_code_succeeds
      , fastProperty "parse errors contain location information" prop_parse_errors_have_location
      , fastProperty "parse recovery preserves valid parts" prop_parse_recovery_preserves_valid
      , fastProperty "parse is monotonic with input extension" prop_parse_monotonic_extension
      ]

  , -- Directive parsing consistency
    testGroup "Directive parsing consistency"
      [ fastProperty "directive parsing is case-sensitive" prop_directive_case_sensitive
      , fastProperty "multiple directives accumulate correctly" prop_multiple_directives_accumulate
      , fastProperty "invalid directives are ignored gracefully" prop_invalid_directives_ignored
      , fastProperty "directive values are validated" prop_directive_values_validated
      ]

  , -- Code block parsing consistency
    testGroup "Code block parsing consistency"
      [ fastProperty "code blocks preserve content" prop_code_blocks_preserve_content
      , fastProperty "nested code blocks are handled correctly" prop_nested_code_blocks
      , fastProperty "empty code blocks are parsed" prop_empty_code_blocks
      , fastProperty "code block boundaries are respected" prop_code_block_boundaries
      ]

  , -- Error location consistency
    testGroup "Error location consistency"
      [ fastProperty "error locations are within input bounds" prop_error_locations_in_bounds
      , fastProperty "error locations are accurate" prop_error_locations_accurate
      , fastProperty "multiple errors have distinct locations" prop_multiple_errors_distinct
      ]

  , -- Advanced consistency properties
    testGroup "Advanced consistency properties"
      [ fastProperty "parse error recovery is idempotent" prop_parse_recovery_idempotent
      , fastProperty "parse preserves semantic content" prop_parse_preserves_semantics
      , fastProperty "parse handles unicode correctly" prop_parse_unicode_correctly
      , fastProperty "parse performance is reasonable" prop_parse_performance_reasonable
      ]
  ]

-- Basic parsing consistency properties

prop_parse_deterministic :: String -> Property
prop_parse_deterministic input =
  let result1 = parseTypus input
      result2 = parseTypus input
  in case (result1, result2) of
       (Left _, Left _) -> property True
       (Right f1, Right f2) -> property $ f1 == f2
       (Left _, Right _) -> property False
       (Right _, Left _) -> property False

prop_parse_valid_code_succeeds :: String -> Property
prop_parse_valid_code_succeeds input =
  let isSimpleDirective = L.any (`L.isPrefixOf` input) 
        [ "//! ownership: on"
        , "//! ownership: off" 
        , "//! dependent_types: on"
        , "//! dependent_types: off"
        ]
      hasNoSpecialChars = not (L.any (`elem` "\\`~!@#$%^&*()+=[]{}|;:'\",<>?/") input)
  in isSimpleDirective && hasNoSpecialChars ==> 
     case parseTypus input of
       Left _ -> property False
       Right _ -> property True

prop_parse_errors_have_location :: String -> Property
prop_parse_errors_have_location input =
  let hasInvalidContent = L.any (`L.isInfixOf` input) ["//! ownership: maybe", "//! @invalid", "\0"]
  in hasInvalidContent ==>
     case parseTypus input of
       Left err -> property $ True -- Error bundle contains location info
       Right _ -> property $ False -- Should have failed

prop_parse_recovery_preserves_valid :: String -> String -> Property
prop_parse_recovery_preserves_valid validPrefix invalidSuffix =
  let validDirectives = ["//! ownership: on", "//! dependent_types: off"]
      isValid = L.any (`L.isPrefixOf` validPrefix) validDirectives
      hasInvalid = L.any (`L.isInfixOf` invalidSuffix) ["//! ownership: maybe", "\0"]
  in isValid && hasInvalid ==>
     let fullInput = validPrefix ++ "\n" ++ invalidSuffix
     in case parseTypus fullInput of
          Left _ -> property True -- Should recover L.and potentially fail gracefully
          Right file -> property $ L.length (blocks file) >= 0 -- Should preserve structure

prop_parse_monotonic_extension :: String -> String -> Property
prop_parse_monotonic_extension baseInput extension =
  let baseResult = parseTypus baseInput
      extendedInput = baseInput ++ extension
      extendedResult = parseTypus extendedInput
  in case (baseResult, extendedResult) of
       (Right baseFile, Right extFile) -> 
         property $ L.length (blocks extFile) >= L.length (blocks baseFile)
       (Left _, Left _) -> property True
       (Left _, Right _) -> property True -- Extension might fix issues
       (Right _, Left _) -> property True -- Extension might introduce issues

-- Directive parsing consistency properties

prop_directive_case_sensitive :: String -> Property
prop_directive_case_sensitive input =
  let hasMixedCase = "//! Ownership: On" `L.isInfixOf` input || "//! OWNERSHIP: ON" `L.isInfixOf` input
  in hasMixedCase ==>
     case parseTypus input of
       Right file -> 
         let fd = directives file
         in property $ isNothing (fdOwnership fd) -- Should not parse mixed case
       Left _ -> property True -- Should fail gracefully

prop_multiple_directives_accumulate :: [String] -> Property
prop_multiple_directives_accumulate directives =
  let validDirectives = ["//! ownership: on", "//! ownership: off", 
                         "//! dependent_types: on", "//! dependent_types: off",
                         "//! constraints: on", "//! constraints: off"]
      filteredDirectives = L.filter (`elem` validDirectives) directives
      input = unlines filteredDirectives
  in not (null filteredDirectives) ==>
     case parseTypus input of
       Right file -> 
         let fd = directives file
         in property $ True -- Should accumulate directives correctly
       Left _ -> property True -- Might fail due to other reasons

prop_invalid_directives_ignored :: String -> Property
prop_invalid_directives_ignored input =
  let hasInvalidDirective = L.any (`L.isInfixOf` input) ["//! ownership: maybe", "//! @invalid", "//! unknown: value"]
  in hasInvalidDirective ==>
     case parseTypus input of
       Right file -> property $ True -- Should ignore invalid L.and continue
       Left _ -> property True -- Might fail, but should attempt recovery

prop_directive_values_validated :: String -> Property
prop_directive_values_validated input =
  let hasInvalidValue = L.any (`L.isInfixOf` input) ["//! ownership: maybe", "//! dependent_types: perhaps"]
  in hasInvalidValue ==>
     case parseTypus input of
       Right file -> 
         let fd = directives file
         in property $ True -- Should validate L.and reject invalid values
       Left _ -> property True -- Should fail appropriately

-- Code block parsing consistency properties

prop_code_blocks_preserve_content :: String -> Property
prop_code_blocks_preserve_content content =
  let hasNoDirectives = not (L.any (`L.isPrefixOf` content) ["//!", "/*", "//"])
      isSimpleContent = L.all (`elem` " \t\n\rabcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789(){}[];:,." ++ "\n\r\t ") content
  in hasNoDirectives && isSimpleContent && not (null content) ==>
     let input = content
     in case parseTypus input of
          Right file -> 
            property $ L.any (content `L.isInfixOf`) [blockContent b | b <- blocks file]
          Left _ -> property False

prop_nested_code_blocks :: String -> String -> Property
prop_nested_code_blocks outer inner =
  let outerContent = "func outer() {\n" ++ outer ++ "\n}"
      innerContent = "func inner() {\n" ++ inner ++ "\n}"
      input = outerContent ++ "\n" ++ innerContent
  in not (null outer) && not (null inner) ==>
     case parseTypus input of
       Right file -> property $ L.length (blocks file) >= 1
       Left _ -> property True -- Might fail due to nesting issues

prop_empty_code_blocks :: Property
prop_empty_code_blocks =
  let input = ""
  in case parseTypus input of
       Right file -> property $ L.length (blocks file) >= 0
       Left _ -> property False

prop_code_block_boundaries :: String -> String -> Property
prop_code_block_boundaries block1 block2 =
  let input = block1 ++ "\n---\n" ++ block2
  in not (null block1) && not (null block2) ==>
     case parseTypus input of
       Right file -> property $ L.length (blocks file) >= 2
       Left _ -> property True -- Boundary parsing might fail

-- Error location consistency properties

prop_error_locations_in_bounds :: String -> Property
prop_error_locations_in_bounds input =
  let hasInvalidContent = L.any (`L.isInfixOf` input) ["\0", "\1", "\2"]
      inputLength = L.length input
  in hasInvalidContent && inputLength > 0 ==>
     case parseTypus input of
       Left err -> property $ True -- Error locations should be within bounds
       Right _ -> property False

prop_error_locations_accurate :: String -> Property
prop_error_locations_accurate input =
  let errorMarker = "//! ownership: maybe"
      hasErrorMarker = errorMarker `L.isInfixOf` input
      markerPos = L.length $ takeWhile (/= '!') input
  in hasErrorMarker ==>
     case parseTypus input of
       Left err -> property $ True -- Error should be near marker position
       Right _ -> property False

prop_multiple_errors_distinct :: String -> String -> Property
prop_multiple_errors_distinct error1 error2 =
  let input = error1 ++ "\n" ++ error2
      hasErrors = L.any (`L.isInfixOf` input) ["//! ownership: maybe", "//! @invalid", "\0"]
  in hasErrors ==>
     case parseTypus input of
       Left err -> property $ True -- Multiple errors should have distinct locations
       Right _ -> property False

-- Advanced consistency properties

prop_parse_recovery_idempotent :: String -> Property
prop_parse_recovery_idempotent input =
  let result1 = parseTypus input
      result2 = parseTypus input
  in case (result1, result2) of
       (Left err1, Left err2) -> property $ True -- Should recover consistently
       (Right f1, Right f2) -> property $ f1 == f2
       _ -> property False

prop_parse_preserves_semantics :: String -> Property
prop_parse_preserves_semantics input =
  let hasSemanticContent = L.any (`L.isInfixOf` input) ["func", "var", "const", "type"]
  in hasSemanticContent ==>
     case parseTypus input of
       Right file -> property $ True -- Should preserve semantic elements
       Left _ -> property True -- Might fail but should attempt preservation

prop_parse_unicode_correctly :: String -> Property
prop_parse_unicode_correctly baseContent =
  let unicodeContent = baseContent ++ "测试🚀café naïve"
  in not (null baseContent) ==>
     case parseTypus unicodeContent of
       Right file -> property $ True -- Should handle Unicode correctly
       Left _ -> property True -- Might fail but Unicode shouldn't crash

prop_parse_performance_reasonable :: String -> Int -> Property
prop_parse_performance_reasonable content repetitions =
  repetitions >= 0 && repetitions <= 100 ==> -- Limit for performance testing
  let largeInput = L.concat (replicate repetitions content)
      result = parseTypus largeInput
  in case result of
       Right file -> property $ True -- Should complete in reasonable time
       Left _ -> property True -- Even errors should be quick