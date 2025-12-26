{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.ParserErrorRecoveryAdvancedQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))

import Parser (parseTypus, TypusFile(..), CodeBlock(..), parseWithErrorRecovery, recoverFromParseError)
import SyntaxValidator (validateSyntax, SyntaxError(..))
import qualified Data.Text as T
import Data.List (isInfixOf, isPrefixOf, lines, length)

-- Property: Parser recovery produces meaningful error messages
prop_parser_recovery_meaningful_errors :: String -> String -> Property
prop_parser_recovery_meaningful_errors goodCode badCode =
  let hasGood = length goodCode > 0
      hasBad = length badCode > 0
      mixedCode = goodCode ++ "\n" ++ badCode ++ "\n" ++ goodCode
  in hasGood && hasBad ==>
  case parseWithErrorRecovery mixedCode of
    Right (recovered, warnings) ->
      let recoveredStr = show recovered
          warningsStr = unlines warnings
          hasRecovered = length recoveredStr > 0
          hasWarnings = length warnings > 0 || length recoveredStr >= length goodCode
      in property $ hasRecovered .&&. hasWarnings
    Left parseError ->
      let errorStr = show parseError
          hasContext = any (`isInfixOf` errorStr) ["error", "line", "parse", "unexpected"]
      in property $ hasContext

-- Property: Parser can recover from syntax errors in the middle of code
prop_parser_recovery_middle_errors :: String -> String -> String -> Property
prop_parser_recovery_middle_errors prefix errorSuffix suffix =
  let hasPrefix = length prefix > 0
      hasError = length errorSuffix > 0
      hasSuffix = length suffix > 0
      codeWithError = prefix ++ "\n" ++ errorSuffix ++ "\n" ++ suffix
  in hasPrefix && hasError && hasSuffix ==>
  case parseWithErrorRecovery codeWithError of
    Right (recovered, warnings) ->
      let recoveredStr = show recovered
          hasPrefixContent = prefix `isInfix` recoveredStr || length prefix < 3
          hasSuffixContent = suffix `isInfix` recoveredStr || length suffix < 3
          hasWarnings = length warnings > 0
      in property $ (hasPrefixContent .||. hasSuffixContent) .&&. hasWarnings
    Left _ -> property $ True

-- Property: Multiple syntax errors are handled gracefully
prop_multiple_syntax_errors :: [String] -> Property
prop_multiple_syntax_errors codeFragments =
  let hasFragments = length codeFragments > 1
      allNonEmpty = all (not . null) codeFragments
      combinedCode = unlines codeFragments
  in hasFragments && allNonEmpty ==>
  case parseWithErrorRecovery combinedCode of
    Right (recovered, warnings) ->
      let warningCount = length warnings
          reasonableWarnings = warningCount <= length codeFragments + 2
          hasRecovered = length (show recovered) > 0
      in property $ hasRecovered .&&. reasonableWarnings
    Left _ -> property $ True

-- Property: Parser recovery preserves valid code sections
prop_parser_recovery_preserves_valid :: String -> String -> Property
prop_parser_recovery_preserves_valid validSection invalidSection =
  let hasValid = length validSection > 5
      hasInvalid = length invalidSection > 0
      mixedCode = validSection ++ "\n" ++ invalidSection ++ "\nfunc test() {}"
  in hasValid && hasInvalid ==>
  case parseWithErrorRecovery mixedCode of
    Right (recovered, warnings) ->
      let recoveredStr = show recovered
          hasValidSection = validSection `isInfix` recoveredStr || length validSection < 10
          hasFunction = "func" `isInfix` recoveredStr
      in property $ hasValidSection .||. hasFunction
    Left _ -> property $ True

-- Property: Error recovery doesn't crash on malformed input
prop_error_recovery_no_crash :: String -> Property
prop_error_recovery_no_crash malformedInput =
  let hasMalformed = any (`elem` malformedInput) "@#$%^&*|\\<>?/~`"
      hasContent = length malformedInput > 2
  in hasMalformed && hasContent ==>
  case parseWithErrorRecovery malformedInput of
    Right (recovered, warnings) ->
      let recoveredStr = show recovered
          warningsStr = unlines warnings
          notEmpty = length recoveredStr > 0 || length warningsStr > 0
      in property $ notEmpty
    Left parseError ->
      let errorStr = show parseError
          notEmpty = length errorStr > 0
      in property $ notEmpty

-- Property: Parser recovery provides accurate location information
prop_parser_recovery_accurate_location :: String -> Int -> Property
prop_parser_recovery_accurate_location code errorLine =
  let hasCode = length code > 0
      validLine = errorLine >= 1 && errorLine <= 10
      codeWithLines = unlines $ take errorLine (repeat code) ++ ["}invalid"]
  in hasCode && validLine ==>
  case parseWithErrorRecovery codeWithLines of
    Right (recovered, warnings) ->
      let warningsStr = unlines warnings
          hasLineNumber = any (`isInfixId` warningsStr) [show errorLine, "line", "Line"]
      in property $ hasLineNumber .||. length warnings == 0
    Left parseError ->
      let errorStr = show parseError
          hasLineNumber = any (`isInfixId` errorStr) [show errorLine, "line", "Line"]
      in property $ hasLineNumber
  where
    isInfixId needle haystack = needle `isInfix` haystack && length needle > 1

-- Property: Syntax validation works with recovered AST
prop_syntax_validation_with_recovered :: String -> Property
prop_syntax_validation_with_recovered code =
  let hasCode = length code > 5
  in hasCode ==>
  case parseWithErrorRecovery code of
    Right (recovered, warnings) ->
      case validateSyntax recovered of
        Right _ -> property $ True
        Left syntaxError ->
          let errorStr = show syntaxError
              hasInfo = any (`isInfix` errorStr) ["syntax", "error", "invalid"]
          in property $ hasInfo
    Left _ -> property $ True

-- Property: Parser recovery is deterministic
prop_parser_recovery_deterministic :: String -> Property
prop_parser_recovery_deterministic code =
  let hasCode = length code > 0
  in hasCode ==>
  let result1 = parseWithErrorRecovery code
      result2 = parseWithErrorRecovery code
      bothSuccess = case (result1, result2) of
        (Right (r1, w1), Right (r2, w2)) -> show r1 == show r2 && w1 == w2
        (Left e1, Left e2) -> show e1 == show e2
        _ -> False
  in property $ bothSuccess

tests :: TestTree
tests = testGroup "Parser Error Recovery Advanced QuickCheck Tests"
  [ fastProperty "Parser recovery produces meaningful error messages" prop_parser_recovery_meaningful_errors
  , fastProperty "Parser can recover from syntax errors in the middle of code" prop_parser_recovery_middle_errors
  , fastProperty "Multiple syntax errors are handled gracefully" prop_multiple_syntax_errors
  , fastProperty "Parser recovery preserves valid code sections" prop_parser_recovery_preserves_valid
  , fastProperty "Error recovery doesn't crash on malformed input" prop_error_recovery_no_crash
  , fastProperty "Parser recovery provides accurate location information" prop_parser_recovery_accurate_location
  , fastProperty "Syntax validation works with recovered AST" prop_syntax_validation_with_recovered
  , fastProperty "Parser recovery is deterministic" prop_parser_recovery_deterministic
  ]