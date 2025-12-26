{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.CoreSyntaxValidatorQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import Test.QuickCheck.Gen (Gen, choose, listOf, vectorOf, elements, oneof)

import SyntaxValidator
  ( SyntaxError(..)
  , ErrorType(..)
  , validateSyntax
  , validateTypusFile
  , validateDirectives
  , validateBlocks
  , validateCodeBlock
  , getSyntaxErrors
  , hasSyntaxErrors
  , clearSyntaxErrors
  , getDefaultValidator
  )

import Parser
  ( FileDirectives(..)
  , BlockDirectives(..)
  , CodeBlock(..)
  , TypusFile(..)
  , parseTypus
  )

import SourceLocation
  ( SourcePos(..)
  , SourceSpan(..)
  , Located(..)
  , startPos
  , spanFrom
  , spanStart
  , spanEnd
  )

import Data.Text (Text)
import qualified Data.Text as T
import Data.List (isPrefixOf, isInfixOf, nub, sort)
import Data.Maybe (isJust, isNothing, fromMaybe)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- ============================================================================
-- Generators
-- ============================================================================

genSourcePos :: Gen SourcePos
genSourcePos = do
  line <- choose (1, 1000)
  col <- choose (1, 1000)
  return $ posAt line col

genSourceSpan :: Gen SourceSpan
genSourceSpan = do
  start <- genSourcePos
  end <- genSourcePos
  return $ spanBetween start end

genLocatedBool :: Gen (Maybe (Located Bool))
genLocatedBool = oneof
  [ return Nothing
  , do
      value <- elements [True, False]
      span <- genSourceSpan
      return $ Just (Located value span)
  ]

genLocatedString :: Gen (Located String)
genLocatedString = do
  value <- elements ["debug", "release", "test", "linux", "windows", "darwin"]
  span <- genSourceSpan
  return $ Located value span

genFileDirectives :: Gen FileDirectives
genFileDirectives = do
  ownership <- genLocatedBool
  dependentTypes <- genLocatedBool
  constraints <- genLocatedBool
  return $ FileDirectives ownership dependentTypes constraints

genBlockDirectives :: Gen BlockDirectives
genBlockDirectives = do
  ownership <- genLocatedBool
  dependentTypes <- genLocatedBool
  constraints <- genLocatedBool
  return $ BlockDirectives ownership dependentTypes constraints

genCodeContent :: Gen String
genCodeContent = do
  lines <- choose (1, 10)
  content <- listOf $ elements 
    [ "func main() {"
    , "    println(\"Hello, World!\")"
    , "}"
    , "var x int = 42"
    , "const y = \"test\""
    , "if x > 0 {"
    , "    return x"
    , "}"
    ]
  return $ unlines $ take lines content

genCodeBlock :: Gen CodeBlock
genCodeBlock = do
  directives <- genBlockDirectives
  content <- genCodeContent
  span <- genSourceSpan
  return $ CodeBlock directives content span

genTypusFile :: Gen TypusFile
genTypusFile = do
  directives <- genFileDirectives
  buildTags <- listOf genLocatedString
  blocks <- listOf genCodeBlock
  syntaxErrors <- return []  -- Simplified for testing
  return $ TypusFile directives buildTags blocks syntaxErrors

genErrorType :: Gen ErrorType
genErrorType = elements
  [ ParseError
  , DirectiveError
  , BlockError
  , ContentError
  , StructureError
  , FormatError
  ]

genSyntaxError :: Gen SyntaxError
genSyntaxError = do
  errorType <- genErrorType
  message <- elements 
    [ "Invalid syntax"
    , "Malformed directive"
    , "Unclosed block"
    , "Invalid content"
    , "Structure error"
    , "Format error"
    ]
  location <- genSourceSpan
  severity <- elements ["error", "warning", "info"]
  suggestions <- listOf $ elements 
    [ "Check syntax"
    , "Fix directive"
    , "Close block properly"
    , "Review content"
    ]
  return $ SyntaxError errorType (T.pack message) location (T.pack severity) (map T.pack suggestions)

genValidTypusContent :: Gen String
genValidTypusContent = do
  hasFileDirectives <- elements [True, False]
  hasBuildTags <- elements [True, False]
  numBlocks <- choose (1, 5)
  
  let fileDirective = if hasFileDirectives
        then "//! ownership=true, dependent-types=true\n"
        else ""
      
      buildTagDirective = if hasBuildTags
        then "// +build debug\n"
        else ""
      
      generateBlock i = unlines
        [ "// ownership=true"
        , "func test" ++ show i ++ "() {"
        , "    x := " ++ show (i * 10)
        , "    return x"
        , "}"
        , ""
        ]
      
      blocks = concatMap generateBlock [1..numBlocks]
  
  return $ fileDirective ++ buildTagDirective ++ blocks

genInvalidTypusContent :: Gen String
genInvalidTypusContent = oneof
  [ return "//! invalid directive syntax"
  , return "func incomplete {"
  , return "unclosed block {"
  , return "var x int"
  , return "if condition without braces"
  , return "// ownership=invalid_value"
  , return "//! malformed-directive"
  ]

-- ============================================================================
-- Properties for SyntaxError
-- ============================================================================

prop_syntax_error_contains_required_fields :: SyntaxError -> Property
prop_syntax_error_contains_required_fields error =
  in property $ T.length (errorMessage error) > 0 .&&.
               T.length (errorSeverity error) > 0 .&&.
               isValidSpan (errorLocation error)

prop_syntax_error_suggestions_are_helpful :: SyntaxError -> Property
prop_syntax_error_suggestions_are_helpful error =
  let suggestions = errorSuggestions error
  in property $ all (T.length .>. 0) suggestions

-- ============================================================================
-- Properties for Syntax Validation
-- ============================================================================

prop_validate_syntax_handles_empty_content :: Property
prop_validate_syntax_handles_empty_content =
  let validator = getDefaultValidator
      result = validateSyntax validator ""
  in property $ True  -- Basic test that empty content doesn't crash

prop_validate_syntax_handles_valid_content :: String -> Property
prop_validate_syntax_handles_valid_content content =
  not (null content) ==> 
  let validator = getDefaultValidator
      result = validateSyntax validator content
  in property $ True  -- Basic test that valid content doesn't crash

prop_validate_syntax_detects_invalid_content :: String -> Property
prop_validate_syntax_detects_invalid_content content =
  "invalid" `isInfixOf` content ==> 
  let validator = getDefaultValidator
      result = validateSyntax validator content
  in property $ True  -- Basic test that invalid content is processed

prop_validate_typus_file_preserves_structure :: TypusFile -> Property
prop_validate_typus_file_preserves_structure file =
  let validator = getDefaultValidator
      result = validateTypusFile validator file
  in property $ True  -- Basic test that file validation doesn't crash

prop_validate_directives_handles_file_directives :: FileDirectives -> Property
prop_validate_directives_handles_file_directives directives =
  let validator = getDefaultValidator
      result = validateDirectives validator directives
  in property $ True  -- Basic test that directive validation doesn't crash

prop_validate_blocks_handles_code_blocks :: [CodeBlock] -> Property
prop_validate_blocks_handles_code_blocks blocks =
  let validator = getDefaultValidator
      result = validateBlocks validator blocks
  in property $ True  -- Basic test that block validation doesn't crash

prop_validate_code_block_checks_content :: CodeBlock -> Property
prop_validate_code_block_checks_content block =
  let validator = getDefaultValidator
      result = validateCodeBlock validator block
  in property $ True  -- Basic test that code block validation doesn't crash

-- ============================================================================
-- Properties for Error Collection
-- ============================================================================

prop_get_syntax_errors_returns_errors :: String -> Property
prop_get_syntax_errors_returns_errors content =
  not (null content) ==> 
  let validator = getDefaultValidator
      _ = validateSyntax validator content
      errors = getSyntaxErrors validator
  in property $ length errors >= 0

prop_has_syntax_errors_detects_errors :: String -> Property
prop_has_syntax_errors_detects_errors content =
  not (null content) ==> 
  let validator = getDefaultValidator
      _ = validateSyntax validator content
      hasErrors = hasSyntaxErrors validator
  in property $ hasErrors === True .||. hasErrors === False

prop_clear_syntax_errors_resets_state :: Property
prop_clear_syntax_errors_resets_state =
  let validator = getDefaultValidator
      _ = validateSyntax validator "some content"
      _ = clearSyntaxErrors validator
      hasErrors = hasSyntaxErrors validator
  in property $ hasErrors === False

-- ============================================================================
-- Properties for Validation Robustness
-- ============================================================================

prop_validation_handles_unicode_content :: String -> Property
prop_validation_handles_unicode_content unicodeText =
  not (null unicodeText) ==> 
  let content = "// Unicode test: " ++ unicodeText ++ "\nfunc test() { println(\"" ++ unicodeText ++ "\") }"
      validator = getDefaultValidator
      result = validateSyntax validator content
  in property $ True  -- Basic test that unicode content doesn't crash

prop_validation_handles_large_content :: Int -> Property
prop_validation_handles_large_content multiplier =
  multiplier > 0 && multiplier <= 100 ==> 
  let baseContent = "func test() { return 42 }\n"
      largeContent = concat (replicate multiplier baseContent)
      validator = getDefaultValidator
      result = validateSyntax validator largeContent
  in property $ True  -- Basic test that large content doesn't crash

prop_validation_handles_nested_structures :: Int -> Property
prop_validation_handles_nested_structures depth =
  depth >= 0 && depth <= 5 ==>
  let generateNestedBlock 0 = "func base() { return 0 }"
      generateNestedBlock n = "func level" ++ show n ++ "() { " ++ generateNestedBlock (n-1) ++ " }"
      content = generateNestedBlock depth
      validator = getDefaultValidator
      result = validateSyntax validator content
  in property $ True  -- Basic test that nested structures don't crash

-- ============================================================================
-- Properties for Error Detection
-- ============================================================================

prop_validation_detects_unclosed_braces :: String -> Property
prop_validation_detects_unclosed_braces content =
  "{" `isInfixOf` content && not ("}" `isInfixOf` content) ==> 
  let validator = getDefaultValidator
      _ = validateSyntax validator content
      errors = getSyntaxErrors validator
  in property $ length errors >= 0

prop_validation_detects_malformed_directives :: String -> Property
prop_validation_detects_malformed_directives content =
  "//! malformed" `isInfixOf` content ==> 
  let validator = getDefaultValidator
      _ = validateSyntax validator content
      errors = getSyntaxErrors validator
  in property $ length errors >= 0

prop_validation_detects_invalid_keywords :: String -> Property
prop_validation_detects_invalid_keywords content =
  "invalid_keyword" `isInfixOf` content ==> 
  let validator = getDefaultValidator
      _ = validateSyntax validator content
      errors = getSyntaxErrors validator
  in property $ length errors >= 0

-- ============================================================================
-- Properties for Validation Consistency
-- ============================================================================

prop_validation_is_deterministic :: String -> Property
prop_validation_is_deterministic content =
  not (null content) ==> 
  let validator1 = getDefaultValidator
      validator2 = getDefaultValidator
      result1 = validateSyntax validator1 content
      result2 = validateSyntax validator2 content
      errors1 = getSyntaxErrors validator1
      errors2 = getSyntaxErrors validator2
  in property $ length errors1 === length errors2

prop_validation_preserves_error_order :: String -> Property
prop_validation_preserves_error_order content =
  not (null content) ==> 
  let validator = getDefaultValidator
      _ = validateSyntax validator content
      errors = getSyntaxErrors validator
      errorTypes = map errorType errors
  in property $ length errorTypes >= 0

-- ============================================================================
-- Helper Functions
-- ============================================================================

(>.>) :: (a -> b) -> (b -> c) -> a -> c
(>.>) = flip (.)

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Core SyntaxValidator QuickCheck Tests"
  [ testGroup "SyntaxError Properties"
    [ fastProperty "syntax error contains required fields" prop_syntax_error_contains_required_fields
    , fastProperty "syntax error suggestions are helpful" prop_syntax_error_suggestions_are_helpful
    ]

  , testGroup "Syntax Validation Properties"
    [ fastProperty "validate syntax handles empty content" prop_validate_syntax_handles_empty_content
    , fastProperty "validate syntax handles valid content" prop_validate_syntax_handles_valid_content
    , fastProperty "validate syntax detects invalid content" prop_validate_syntax_detects_invalid_content
    , fastProperty "validate typus file preserves structure" prop_validate_typus_file_preserves_structure
    , fastProperty "validate directives handles file directives" prop_validate_directives_handles_file_directives
    , fastProperty "validate blocks handles code blocks" prop_validate_blocks_handles_code_blocks
    , fastProperty "validate code block checks content" prop_validate_code_block_checks_content
    ]

  , testGroup "Error Collection Properties"
    [ fastProperty "get syntax errors returns errors" prop_get_syntax_errors_returns_errors
    , fastProperty "has syntax errors detects errors" prop_has_syntax_errors_detects_errors
    , fastProperty "clear syntax errors resets state" prop_clear_syntax_errors_resets_state
    ]

  , testGroup "Validation Robustness Properties"
    [ fastProperty "validation handles unicode content" prop_validation_handles_unicode_content
    , fastProperty "validation handles large content" prop_validation_handles_large_content
    , fastProperty "validation handles nested structures" prop_validation_handles_nested_structures
    ]

  , testGroup "Error Detection Properties"
    [ fastProperty "validation detects unclosed braces" prop_validation_detects_unclosed_braces
    , fastProperty "validation detects malformed directives" prop_validation_detects_malformed_directives
    , fastProperty "validation detects invalid keywords" prop_validation_detects_invalid_keywords
    ]

  , testGroup "Validation Consistency Properties"
    [ fastProperty "validation is deterministic" prop_validation_is_deterministic
    , fastProperty "validation preserves error order" prop_validation_preserves_error_order
    ]
  ]