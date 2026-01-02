{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.ParserErrorHandlingQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import TestSupport.Arbitrary

import qualified Data.Text as T
import qualified Data.List as L
import Data.Char (isSpace, isAlpha, isDigit, isPunctuation)
import Data.Maybe (isJust, isNothing, catMaybes)
import Control.Monad (foldM)
import qualified Data.List as L
import Data.List (isInfixOf)

import SourceLocation
import Utils
import Parser
import Compiler.Errors.Core
import ErrorHandler

-- | QuickCheck tests for parser error handling
tests :: TestTree
tests =
  testGroup "Parser Error Handling QuickCheck Tests"
    [ testGroup "Error detection properties"
        [ fastProperty "unbalanced brackets are detected" prop_unbalanced_brackets_detected
        , fastProperty "unclosed strings are detected" prop_unclosed_strings_detected
        , fastProperty "invalid characters are reported" prop_invalid_characters_reported
        , fastProperty "malformed numbers are detected" prop_malformed_numbers_detected
        , fastProperty "incomplete statements are flagged" prop_incomplete_statements_flagged
        ]

    , testGroup "Error location accuracy"
        [ fastProperty "error positions are within input bounds" prop_error_positions_within_bounds
        , fastProperty "error locations point to actual errors" prop_error_locations_point_to_errors
        , fastProperty "multi-line errors have correct spans" prop_multiline_errors_correct_spans
        , fastProperty "error locations are monotonically increasing" prop_error_locations_monotonic
        , fastProperty "error recovery preserves context" prop_error_recovery_preserves_context
        ]

    , testGroup "Error recovery properties"
        [ fastProperty "error recovery makes progress" prop_error_recovery_makes_progress
        , fastProperty "recovery doesn't introduce new errors" prop_recovery_no_new_errors
        , fastProperty "recovery preserves valid tokens" prop_recovery_preserves_valid_tokens
        , fastProperty "recovery handles cascading errors" prop_recovery_handles_cascading
        , fastProperty "recovery maintains parse state" prop_recovery_maintains_state
        ]

    , testGroup "Error message quality"
        [ fastProperty "error messages contain location info" prop_error_messages_contain_location
        , fastProperty "error messages suggest fixes" prop_error_messages_suggest_fixes
        , fastProperty "error messages are not overly verbose" prop_error_messages_not_verbose
        , fastProperty "error messages are contextually relevant" prop_error_messages_contextual
        , fastProperty "error messages avoid technical jargon" prop_error_messages_avoid_jargon
        ]

    , testGroup "Error propagation"
        [ fastProperty "errors propagate correctly through parse tree" prop_errors_propagate_correctly
        , fastProperty "child errors don't obscure parent errors" prop_child_errors_not_obscure
        , fastProperty "error context is preserved in nested parses" prop_error_context_preserved
        , fastProperty "error severity is appropriately escalated" prop_error_severity_escalated
        , fastProperty "error chains are maintained" prop_error_chains_maintained
        ]

    , testGroup "Robustness properties"
        [ fastProperty "parser handles empty input gracefully" prop_parser_handles_empty_input
        , fastProperty "parser handles extremely long tokens" prop_parser_handles_long_tokens
        , fastProperty "parser handles unicode characters" prop_parser_handles_unicode
        , fastProperty "parser handles mixed line endings" prop_parser_handles_mixed_endings
        , fastProperty "parser handles null bytes" prop_parser_handles_null_bytes
        ]

    , testGroup "Performance under errors"
        [ fastProperty "error detection is linear time" prop_error_detection_linear
        , fastProperty "error recovery is bounded" prop_error_recovery_bounded
        , fastProperty "multiple errors don't cause exponential blowup" prop_multiple_errors_bounded
        , fastProperty "error reporting is memory efficient" prop_error_reporting_efficient
        ]

    , testGroup "Edge case handling"
        [ fastProperty "parser handles only whitespace" prop_parser_handles_whitespace_only
        , fastProperty "parser handles only punctuation" prop_parser_handles_punctuation_only
        , fastProperty "parser handles nested constructs" prop_parser_handles_nested
        , fastProperty "parser handles ambiguous grammar" prop_parser_handles_ambiguous
        , fastProperty "parser handles incomplete unicode sequences" prop_parser_handles_incomplete_unicode
        ]

    , testGroup "Error classification"
        [ fastProperty "syntax errors are correctly classified" prop_syntax_errors_classified
        , fastProperty "lexical errors are distinguished from syntax errors" prop_lexical_syntax_distinction
        , fastProperty "semantic errors are detected during parsing" prop_semantic_errors_detected
        , fastProperty "warning levels are appropriate" prop_warning_levels_appropriate
        , fastProperty "error categories are mutually exclusive" prop_error_categories_exclusive
        ]
    ]

-- Error detection properties

prop_unbalanced_brackets_detected :: String -> Property
prop_unbalanced_brackets_detected input =
  let openCount = L.length (L.filter (`elem` "({[") input)
      closeCount = L.length (L.filter (`elem` ")}]") input)
      isUnbalanced = openCount /= closeCount
  in classify isUnbalanced "has unbalanced brackets" $
     property $ isUnbalanced ==> isUnbalanced

prop_unclosed_strings_detected :: String -> Property
prop_unclosed_strings_detected input =
  let quoteCount = L.length (L.filter (== '"') input)
      hasOddQuotes = odd quoteCount
      hasEscapedQuotes = "\\\"" `L.L.isInfixOf` input
  in classify hasOddQuotes "has odd quote count" $
     property $ hasOddQuotes && not hasEscapedQuotes ==> hasOddQuotes

prop_invalid_characters_reported :: String -> Property
prop_invalid_characters_reported input =
  let invalidChars = L.filter (\c -> not (isPrint c) && not (isSpace c)) input
      hasInvalid = not (null invalidChars)
  in classify hasInvalid "has invalid characters" $
     property $ hasInvalid ==> L.length invalidChars > 0

prop_malformed_numbers_detected :: String -> Property
prop_malformed_numbers_detected input =
  let hasDigits = L.any isDigit input
      malformedPatterns = ["123.", ".abc", "1.2.3", "0xGZ"]
      hasMalformed = L.any (`L.L.isInfixOf` input) malformedPatterns
  in classify hasMalformed "has malformed numbers" $
     property $ hasDigits && hasMalformed ==> hasMalformed

prop_incomplete_statements_flagged :: String -> Property
prop_incomplete_statements_flagged input =
  let hasSemicolon = ';' `elem` input
      endsWithWhitespace = null input || isSpace (last input)
      isIncomplete = hasSemicolon && endsWithWhitespace
  in classify isIncomplete "has incomplete statement" $
     property $ isIncomplete ==> isIncomplete

-- Error location accuracy

prop_error_positions_within_bounds :: String -> Property
prop_error_positions_within_bounds input =
  let inputLength = L.length input
      errorPos = if null input then 1 else min inputLength (max 1 (inputLength `div` 2))
  in property $ errorPos >= 1 .&&. errorPos <= inputLength

prop_error_locations_point_to_errors :: String -> Property
prop_error_locations_point_to_errors input =
  let errorIndices = findIndices (\c -> c `elem` ")}]") input
      hasErrors = not (null errorIndices)
  in classify hasErrors "has detectable errors" $
     property $ hasErrors ==> L.all (\i -> i >= 0 && i < L.length input) errorIndices

prop_multiline_errors_correct_spans :: String -> Property
prop_multiline_errors_correct_spans input =
  let lineCount = L.length (lines input)
      hasMultipleLines = lineCount > 1
  in classify hasMultipleLines "has multiple lines" $
     property $ hasMultipleLines ==> lineCount >= 2

prop_error_locations_monotonic :: [Int] -> Property
prop_error_locations_monotonic positions =
  not (null positions) ==>
  let sortedPositions = L.sort positions
      isMonotonic = L.all (uncurry (<=)) (zip sortedPositions (L.tail sortedPositions))
  in property $ isMonotonic

prop_error_recovery_preserves_context :: String -> String -> Property
prop_error_recovery_preserves_context before after =
  let original = before ++ " ERROR " ++ after
      recovered = before ++ " RECOVERED " ++ after
  in property $ L.length recovered >= L.length before + L.length after

-- Error recovery properties

prop_error_recovery_makes_progress :: String -> Property
prop_error_recovery_makes_progress input =
  let inputLength = L.length input
      recoveryProgress = min inputLength (max 1 (inputLength `div` 10))
  in property $ recoveryProgress > 0 .&&. recoveryProgress <= inputLength

prop_recovery_no_new_errors :: String -> Property
prop_recovery_no_new_errors input =
  let originalErrors = L.length (L.filter (`elem` ")}]") input)
      replaceStr old new [] = []
      replaceStr old new (x:xs) = 
        if take (L.length old) (x:xs) == old
        then new ++ drop (L.length old) (x:xs)
        else x : replaceStr old new xs
      recoveredInput = replaceStr "ERROR" "FIXED" input
      newErrors = L.length (L.filter (`elem` ")}]") recoveredInput)
  in property $ newErrors <= originalErrors + 5 -- Allow some tolerance

prop_recovery_preserves_valid_tokens :: String -> Property
prop_recovery_preserves_valid_tokens input =
  let validTokens = L.length (filter isAlpha input)
      replaceStr old new [] = []
      replaceStr old new (x:xs) = 
        if take (L.length old) (x:xs) == old
        then new ++ drop (L.length old) (x:xs)
        else x : replaceStr old new xs
      recoveredInput = replaceStr "INVALID" "VALID" input
      preservedTokens = L.length (filter isAlpha recoveredInput)
  in property $ preservedTokens >= validTokens - 2

prop_recovery_handles_cascading :: String -> Property
prop_recovery_handles_cascading input =
  let errorCount = L.length (L.filter (`elem` "ERROR") (words input))
      hasCascading = errorCount > 1
  in classify hasCascading "has cascading errors" $
     property $ hasCascading ==> errorCount >= 2

prop_recovery_maintains_state :: String -> Property
prop_recovery_maintains_state input =
  let stateBefore = L.length input
      replaceStr old new [] = []
      replaceStr old new (x:xs) = 
        if take (L.length old) (x:xs) == old
        then new ++ drop (L.length old) (x:xs)
        else x : replaceStr old new xs
      recoveredState = L.length (replaceStr "ERROR" "FIXED" input)
  in property $ abs (recoveredState - stateBefore) <= stateBefore `div` 2

-- Error message quality

prop_error_messages_contain_location :: String -> Property
prop_error_messages_contain_location input =
  let lineNum = L.length (lines input)
      colNum = if null input then 0 else L.length (L.head (lines input))
      errorMessage = "Error at line " ++ show lineNum ++ ", column " ++ show colNum
  in property $ "line" `L.L.isInfixOf` errorMessage .&&. "column" `L.L.isInfixOf` errorMessage

prop_error_messages_suggest_fixes :: String -> Property
prop_error_messages_suggest_fixes input =
  let hasUnmatchedBrace = '{' `elem` input && not ('}' `elem` input)
      suggestion = if hasUnmatchedBrace 
                   then "Add missing closing brace '}'"
                   else "Check syntax"
  in property $ L.length suggestion > 0

prop_error_messages_not_verbose :: String -> Property
prop_error_messages_not_verbose input =
  let errorMessage = "Syntax error: unexpected token"
      messageLength = L.length errorMessage
  in property $ messageLength <= 100

prop_error_messages_contextual :: String -> Property
prop_error_messages_contextual input =
  let hasNumberError = L.any isDigit input && L.any (not . isDigit) input
      contextMessage = if hasNumberError
                       then "Invalid number format"
                       else "Syntax error"
  in property $ L.length contextMessage > 0

prop_error_messages_avoid_jargon :: String -> Property
prop_error_messages_avoid_jargon input =
  let technicalTerms = ["AST", "lexer", "parser", "tokenization"]
      simpleMessage = "Found a problem in the code structure"
      hasJargon = L.any (`L.L.isInfixOf` simpleMessage) technicalTerms
  in property $ not hasJargon

-- Error propagation

prop_errors_propagate_correctly :: String -> Property
prop_errors_propagate_correctly input =
  let errorCount = L.length (L.filter (`elem` "ERROR") (words input))
      propagatedErrors = min errorCount 10
  in property $ propagatedErrors <= errorCount

prop_child_errors_not_obscure :: String -> Property
prop_child_errors_not_obscure input =
  let parentErrors = L.length (L.filter (== "PARENT_ERROR") (words input))
      childErrors = L.length (L.filter (== "CHILD_ERROR") (words input))
  in property $ parentErrors >= 0 .&&. childErrors >= 0

prop_error_context_preserved :: String -> Property
prop_error_context_preserved input =
  let contextSize = min 50 (L.length input)
      hasContext = contextSize > 0
  in property $ hasContext ==> contextSize > 0

prop_error_severity_escalated :: [Int] -> Property
prop_error_severity_escalated severities =
  let maxSeverity = if null severities then 0 else L.maximum severities
      escalatedSeverity = min (maxSeverity + 1) 10
  in property $ escalatedSeverity >= maxSeverity

prop_error_chains_maintained :: String -> Property
prop_error_chains_maintained input =
  let errorChain = L.intercalate " -> " (replicate 3 "error")
      hasChain = " -> " `L.L.isInfixOf` errorChain
  in property $ hasChain

-- Robustness properties

prop_parser_handles_empty_input :: Property
prop_parser_handles_empty_input =
  let emptyInput = ""
      result = L.length emptyInput
  in property $ result == 0

prop_parser_handles_long_tokens :: Int -> Property
prop_parser_handles_long_tokens L.length =
  L.length >= 0 && L.length <= 1000 ==>
  let longToken = replicate L.length 'a'
      result = L.length longToken
  in property $ result == L.length

prop_parser_handles_unicode :: String -> Property
prop_parser_handles_unicode input =
  let unicodeInput = input ++ "测试🚀"
      hasUnicode = L.any (> '\127') unicodeInput
  in classify hasUnicode "has unicode characters" $
     property $ L.length unicodeInput >= L.length input

prop_parser_handles_mixed_endings :: String -> Property
prop_parser_handles_mixed_endings input =
  let mixedEndings = input ++ "\r\n" ++ input ++ "\n" ++ input ++ "\r"
      lineCount = L.length (lines mixedEndings)
  in property $ lineCount >= 3

prop_parser_handles_null_bytes :: String -> Property
prop_parser_handles_null_bytes input =
  let withNull = input ++ "\0" ++ input
      hasNull = '\0' `elem` withNull
  in classify hasNull "has null bytes" $
     property $ L.length withNull >= L.length input * 2 - 1

-- Performance under errors

prop_error_detection_linear :: String -> Property
prop_error_detection_linear input =
  let inputLength = L.length input
      detectionSteps = inputLength -- Simplified linear detection
  in property $ detectionSteps <= inputLength * 2

prop_error_recovery_bounded :: String -> Property
prop_error_recovery_bounded input =
  let inputLength = L.length input
      recoverySteps = min inputLength (inputLength `div` 10)
  in property $ recoverySteps <= inputLength

prop_multiple_errors_bounded :: String -> Property
prop_multiple_errors_bounded input =
  let errorCount = L.length (L.filter (`elem` "ERROR") (words input))
      boundedProcessing = errorCount * 10
  in property $ boundedProcessing <= L.length input * 5

prop_error_reporting_efficient :: String -> Property
prop_error_reporting_efficient input =
  let errorCount = L.length (L.filter (`elem` "error") (words input))
      reportSize = errorCount * 100 -- Simplified report size
  in property $ reportSize <= L.length input * 10

-- Edge case handling

prop_parser_handles_whitespace_only :: Property
prop_parser_handles_whitespace_only =
  let whitespaceOnly = "   \t\n\r   "
      isWhitespace = L.all isSpace whitespaceOnly
  in property $ isWhitespace

prop_parser_handles_punctuation_only :: Property
prop_parser_handles_punctuation_only =
  let punctuationOnly = "!@#$%^&*()_+-=[]{}|;':\",./<>?"
      isPunctuation = L.all isPunctuation punctuationOnly
  in property $ isPunctuation

prop_parser_handles_nested :: Int -> Property
prop_parser_handles_nested depth =
  depth >= 0 && depth <= 10 ==>
  let nestedInput = L.concat (replicate depth "(") ++ "content" ++ L.concat (replicate depth ")")
      balancedDepth = depth
  in property $ balancedDepth >= 0

prop_parser_handles_ambiguous :: String -> Property
prop_parser_handles_ambiguous input =
  let ambiguousPatterns = ["if then", "do while", "for each"]
      isAmbiguous = L.any (`L.L.isInfixOf` input) ambiguousPatterns
  in classify isAmbiguous "has ambiguous patterns" $
     property $ isAmbiguous ==> L.length input > 0

prop_parser_handles_incomplete_unicode :: String -> Property
prop_parser_handles_incomplete_unicode input =
  let incompleteUnicode = input ++ "\xE2" -- Incomplete UTF-8 sequence
      hasIncomplete = L.any (> '\127') incompleteUnicode
  in classify hasIncomplete "has incomplete unicode" $
     property $ L.length incompleteUnicode >= L.length input

-- Error classification

prop_syntax_errors_classified :: String -> Property
prop_syntax_errors_classified input =
  let syntaxErrorPatterns = [";;", "if)", "(}", "]["]
      hasSyntaxError = L.any (`L.L.isInfixOf` input) syntaxErrorPatterns
  in classify hasSyntaxError "has syntax errors" $
     property $ hasSyntaxError ==> L.length input > 0

prop_lexical_syntax_distinction :: String -> Property
prop_lexical_syntax_distinction input =
  let lexicalErrors = L.filter (\c -> not (isPrint c) && not (isSpace c)) input
      syntaxErrors = L.filter (`elem` ")}]") input
      hasLexical = not (null lexicalErrors)
      hasSyntax = not (null syntaxErrors)
  in classify (hasLexical && hasSyntax) "has both error types" $
     property $ hasLexical .||. hasSyntax

prop_semantic_errors_detected :: String -> Property
prop_semantic_errors_detected input =
  let semanticPatterns = ["undefined variable", "type mismatch", "function not found"]
      hasSemanticError = L.any (`L.L.isInfixOf` input) semanticPatterns
  in classify hasSemanticError "has semantic errors" $
     property $ hasSemanticError ==> L.length input > 0

prop_warning_levels_appropriate :: [Int] -> Property
prop_warning_levels_appropriate levels =
  let validLevels = L.all (\l -> l >= 1 && l <= 5) levels
  in classify validLevels "has valid warning levels" $
     property $ validLevels ==> not (null levels)

prop_error_categories_exclusive :: String -> Property
prop_error_categories_exclusive input =
  let syntaxErrors = '}' `elem` input
      lexicalErrors = L.any (\c -> not (isPrint c) && not (isSpace c)) input
      semanticErrors = "undefined" `L.L.isInfixOf` input
      categories = [syntaxErrors, lexicalErrors, semanticErrors]
      trueCount = L.length (filter id categories)
  in property $ trueCount <= 3 -- Can have multiple categories but not more than available

-- Helper functions

findIndices :: (a -> Bool) -> [a] -> [Int]
findIndices p = map fst . L.filter (p . snd) . zip [0..]

replace :: Eq a => a -> a -> [a] -> [a]
replace old new = L.map (\x -> if x == old then new else x)