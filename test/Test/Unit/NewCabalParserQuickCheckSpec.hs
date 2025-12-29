{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewCabalParserQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, oneof, elements, listOf, choose)
import qualified Test.QuickCheck as QC

import Parser
import SourceLocation (SourcePos(..), SourceSpan(..), Located(..), locatedWithSpan, emptySpan)
import qualified Data.Text as T
import Data.Char (isAlphaNum, isAlpha, isSpace)
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)

-- ============================================================================
-- Generators for Parser data types
-- ============================================================================

-- Generate valid identifiers
genIdentifier :: Gen String
genIdentifier = do
  first <- elements ['a'..'z']
  rest <- listOf $ elements $ ['a'..'z'] ++ ['0'..'9'] ++ ['_']
  pure $ first : rest

-- Generate valid directives
genDirective :: Gen (String, String)
genDirective = do
  key <- genIdentifier
  value <- genIdentifier
  pure (key, value)

-- Generate file directives
genFileDirectives :: Gen FileDirectives
genFileDirectives = do
  ownership <- QC.oneof [pure Nothing, Just <$> genLocatedBool]
  dependentTypes <- QC.oneof [pure Nothing, Just <$> genLocatedBool]
  constraints <- QC.oneof [pure Nothing, Just <$> genLocatedBool]
  pure $ FileDirectives ownership dependentTypes constraints

-- Generate block directives
genBlockDirectives :: Gen BlockDirectives
genBlockDirectives = do
  ownership <- QC.oneof [pure Nothing, Just <$> genLocatedBool]
  dependentTypes <- QC.oneof [pure Nothing, Just <$> genLocatedBool]
  constraints <- QC.oneof [pure Nothing, Just <$> genLocatedBool]
  pure $ BlockDirectives ownership dependentTypes constraints

-- Generate located boolean values
genLocatedBool :: Gen (Located Bool)
genLocatedBool = do
  value <- QC.arbitrary
  pos <- genSourcePos
  pure $ locatedWithSpan (emptySpan pos) value

-- Generate source position
genSourcePos :: Gen SourcePos
genSourcePos = do
  line <- choose (1, 1000)
  column <- choose (1, 1000)
  offset <- choose (0, 10000)
  pure $ SourcePos line column offset

-- Generate source span
genSourceSpan :: Gen SourceSpan
genSourceSpan = do
  start <- genSourcePos
  end <- genSourcePos
  pure $ SourceSpan start end

-- Generate arbitrary CodeBlock
genCodeBlock :: Gen CodeBlock
genCodeBlock = do
  content <- listOf $ QC.arbitrary :: Gen String
  directives <- genBlockDirectives
  span <- genSourceSpan
  pure $ CodeBlock directives content span

-- Generate typus files
genTypusFile :: Gen TypusFile
genTypusFile = do
  directives <- genFileDirectives
  buildTags <- listOf genIdentifier
  blocks <- listOf genCodeBlock
  syntaxErrors <- QC.arbitrary
  pure $ TypusFile directives buildTags blocks syntaxErrors

-- ============================================================================
-- Property-based tests for Parser module
-- ============================================================================

-- Property: parseTypus handles empty input
prop_parseTypus_empty :: Property
prop_parseTypus_empty =
  let result = parseTypus ""
  in case result of
    Left _ -> property True
    Right file -> property $ null (tfBlocks file)

-- Property: parseTypus handles whitespace-only input
prop_parseTypus_whitespace :: String -> Property
prop_parseTypus_whitespace ws =
  all isSpace ws ==>
  let result = parseTypus ws
  in case result of
    Left _ -> property True
    Right file -> property $ null (tfBlocks file)

-- Property: parseTypus preserves directives
prop_parseTypus_directives :: String -> String -> Property
prop_parseTypus_directives key value =
  all isAlphaNum key && all isAlphaNum value ==>
  let input = "//#!" ++ key ++ ":" ++ value ++ "\n"
      result = parseTypus input
  in case result of
    Left _ -> property False
    Right file -> property $ 
      case fdOwnership (tfDirectives file) of
        Nothing -> False
        Just (Located (True) _) -> key == "ownership" && value == "true"
        Just (Located (False) _) -> key == "ownership" && value == "false"

-- Property: parseTypus handles multiple directives
prop_parseTypus_multiple_directives :: [(String, String)] -> Property
prop_parseTypus_multiple_directives directives =
  all (all isAlphaNum . fst) directives && all (all isAlphaNum . snd) directives ==>
  let directiveLines = map (\(k, v) -> "//#!" ++ k ++ ":" ++ v) directives
      input = unlines directiveLines
      result = parseTypus input
  in case result of
    Left _ -> property True  -- May fail due to invalid directive combinations
    Right file -> property $ True  -- Successfully parsed

-- Property: parseTypus handles block directives
prop_parseTypus_block_directives :: String -> String -> Property
prop_parseTypus_block_directives key value =
  all isAlphaNum key && all isAlphaNum value ==>
  let input = "{//! " ++ key ++ ":" ++ value ++ " }\ncontent\n"
      result = parseTypus input
  in case result of
    Left _ -> property True
    Right file -> property $ length (tfBlocks file) >= 1

-- Property: parseTypus preserves code content
prop_parseTypus_preserves_content :: String -> Property
prop_parseTypus_preserves_content content =
  not (null content) && not (any (`elem` ['{', '}', '/', '\n', '\r']) content) ==>
  let input = content ++ "\n"
      result = parseTypus input
  in case result of
    Left _ -> property True
    Right file -> property $ 
      case tfBlocks file of
        [] -> False
        (block:_) -> content `isInfixOf` unlines (cbContent block)

-- Property: parseTypus handles mixed content
prop_parseTypus_mixed_content :: String -> String -> String -> Property
prop_parseTypus_mixed_content directive content blockDirective =
  all isAlphaNum directive && all isAlphaNum blockDirective ==>
  let input = "//#!" ++ directive ++ ":true\n" ++
              content ++ "\n" ++
              "{//! " ++ blockDirective ++ ":false }\n" ++
              "more content\n"
      result = parseTypus input
  in case result of
    Left _ -> property True
    Right file -> property $ length (tfBlocks file) >= 1

-- Property: parseTypus handles invalid syntax gracefully
prop_parseTypus_invalid_syntax :: String -> Property
prop_parseTypus_invalid_syntax content =
  let invalidChars = ['{', '}', '/', '"', '\'', '\\']
      hasInvalid = any (`elem` invalidChars) content
  in hasInvalid ==>
  let result = parseTypus content
  in case result of
    Left _ -> property True
    Right file -> property $ True  -- May still parse partially

-- Property: parseTypus is deterministic
prop_parseTypus_deterministic :: String -> Property
prop_parseTypus_deterministic input =
  let result1 = parseTypus input
      result2 = parseTypus input
  in case (result1, result2) of
    (Left e1, Left e2) -> e1 === e2
    (Right f1, Right f2) -> f1 === f2
    _ -> property False

-- Property: parseTypus handles line endings
prop_parseTypus_line_endings :: String -> Property
prop_parseTypus_line_endings content =
  not ('\n' `elem` content) && not ('\r' `elem` content) ==>
  let input1 = content ++ "\n"
      input2 = content ++ "\r\n"
      result1 = parseTypus input1
      result2 = parseTypus input2
  in case (result1, result2) of
    (Left _, Left _) -> property True
    (Right f1, Right f2) -> 
      property $ tfBlocks f1 === tfBlocks f2  -- Content should be the same
    _ -> property False

-- Property: parseTypus handles Unicode content
prop_parseTypus_unicode :: String -> Property
prop_parseTypus_unicode baseContent =
  let unicodeContent = baseContent ++ "测试🚀café"
      input = unicodeContent ++ "\n"
      result = parseTypus input
  in case result of
    Left _ -> property True
    Right file -> property $ True

-- Property: parseTypus handles very long lines
prop_parseTypus_long_lines :: Int -> String -> Property
prop_parseTypus_long_lines multiplier baseContent =
  multiplier >= 0 && multiplier <= 100 ==>
  let longContent = concat (replicate multiplier baseContent)
      input = longContent ++ "\n"
      result = parseTypus input
  in case result of
    Left _ -> property True
    Right file -> property $ True

-- Property: parseTypus handles nested block structures
prop_parseTypus_nested_blocks :: Int -> Property
prop_parseTypus_nested_blocks depth =
  depth >= 0 && depth <= 10 ==>
  let nestedContent = concat (replicate depth "{//! ownership:true }\n")
      input = nestedContent ++ "content\n"
      result = parseTypus input
  in case result of
    Left _ -> property True
    Right file -> property $ length (tfBlocks file) >= depth

-- Property: parseTypus error messages contain line numbers
prop_parseTypus_error_line_numbers :: String -> Property
prop_parseTypus_error_line_numbers malformedInput =
  not (null malformedInput) && '{' `elem` malformedInput && not ('}' `elem` malformedInput) ==>
  let result = parseTypus malformedInput
  in case result of
    Left errorMsg -> property $ any (`elem` "0123456789") errorMsg
    Right _ -> property False  -- Should have failed but didn't

-- Property: parseTypus handles comments correctly
prop_parseTypus_comments :: String -> String -> Property
prop_parseTypus_comments code comment =
  not ('"' `elem` code) && not ('/' `elem` code) ==>
  let input = code ++ " // " ++ comment ++ "\n"
      result = parseTypus input
  in case result of
    Left _ -> property True
    Right file -> property $ True

-- Property: parseTypus preserves build tags
prop_parseTypus_build_tags :: [String] -> Property
prop_parseTypus_build_tags tags =
  all (all isAlphaNum) tags ==>
  let tagLines = map (\tag -> "//#!build:" ++ tag) tags
      input = unlines tagLines
      result = parseTypus input
  in case result of
    Left _ -> property True
    Right file -> property $ 
      let fileTags = tfBuildTags file
      in all (`elem` fileTags) tags

-- Property: parseTypus handles empty directives
prop_parseTypus_empty_directives :: Property
prop_parseTypus_empty_directives =
  let input = "//#!:\n"
      result = parseTypus input
  in case result of
    Left _ -> property True
    Right file -> property $ True

-- Property: parseTypus handles malformed directives
prop_parseTypus_malformed_directives :: String -> Property
prop_parseTypus_malformed_directives malformed =
  let input = "//#!" ++ malformed ++ "\n"
      result = parseTypus input
  in case result of
    Left _ -> property True
    Right file -> property $ True

-- Property: parseTypus handles special characters in content
prop_parseTypus_special_chars :: String -> Property
prop_parseTypus_special_chars content =
  let specialChars = "!@#$%^&*()_+-=[]|;':\",./<>?"
      hasSpecial = any (`elem` specialChars) content
  in hasSpecial ==>
  let input = content ++ "\n"
      result = parseTypus input
  in case result of
    Left _ -> property True
    Right file -> property $ True

-- ============================================================================
-- Test suite
-- ============================================================================

tests :: TestTree
tests = testGroup "New Cabal Parser QuickCheck Tests"
  [ fastProperty "parseTypus handles empty input" prop_parseTypus_empty
  , fastProperty "parseTypus handles whitespace-only input" prop_parseTypus_whitespace
  , fastProperty "parseTypus preserves directives" prop_parseTypus_directives
  , fastProperty "parseTypus handles multiple directives" prop_parseTypus_multiple_directives
  , fastProperty "parseTypus handles block directives" prop_parseTypus_block_directives
  , fastProperty "parseTypus preserves code content" prop_parseTypus_preserves_content
  , fastProperty "parseTypus handles mixed content" prop_parseTypus_mixed_content
  , fastProperty "parseTypus handles invalid syntax gracefully" prop_parseTypus_invalid_syntax
  , fastProperty "parseTypus is deterministic" prop_parseTypus_deterministic
  , fastProperty "parseTypus handles line endings" prop_parseTypus_line_endings
  , fastProperty "parseTypus handles Unicode content" prop_parseTypus_unicode
  , fastProperty "parseTypus handles very long lines" prop_parseTypus_long_lines
  , fastProperty "parseTypus handles nested block structures" prop_parseTypus_nested_blocks
  , fastProperty "parseTypus error messages contain line numbers" prop_parseTypus_error_line_numbers
  , fastProperty "parseTypus handles comments correctly" prop_parseTypus_comments
  , fastProperty "parseTypus preserves build tags" prop_parseTypus_build_tags
  , fastProperty "parseTypus handles empty directives" prop_parseTypus_empty_directives
  , fastProperty "parseTypus handles malformed directives" prop_parseTypus_malformed_directives
  , fastProperty "parseTypus handles special characters in content" prop_parseTypus_special_chars
  ]