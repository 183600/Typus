{-# LANGUAGE TemplateHaskell #-}

module Test.Unit.NewComprehensiveParserQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.TH
import Parser
  ( FileDirectives(..), BlockDirectives(..), CodeBlock(..), TypusFile(..)
  , defaultFileDirectives, defaultBlockDirectives, parseTypus
  )
import SourceLocation (SourcePos(..), SourceSpan(..), posAt, spanBetween)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Char (isSpace)
import Data.List (isPrefixOf, isInfixOf)
import Control.DeepSeq (NFData, rnf)

-- Test directive parsing properties
prop_file_directives_default_valid :: Bool
prop_file_directives_default_valid = 
  let defaults = defaultFileDirectives
  in isNothing (fdOwnership defaults) &&
     isNothing (fdDependentTypes defaults) &&
     isNothing (fdConstraints defaults)

prop_block_directives_default_valid :: Bool
prop_block_directives_default_valid = 
  let defaults = defaultBlockDirectives
  in isNothing (bdOwnership defaults) &&
     isNothing (bdDependentTypes defaults) &&
     isNothing (bdConstraints defaults)

-- Test parsing consistency properties
prop_parse_empty_string :: Bool
prop_parse_empty_string = 
  let result = parseTypus ""
      expected = TypusFile defaultFileDirectives [] [] []
  in case result of
    Left _ -> False
    Right parsed -> parsed == expected

prop_parse_preserves_non_directive_content :: String -> Property
prop_parse_preserves_non_directive_content content = 
  not (any (`isPrefixOf` content) ["//!", "//@", "//!@"]) ==> 
  let result = parseTypus content
  in case result of
    Left _ -> False
    Right parsed -> 
      let blocks = tfBlocks parsed
      in not (null blocks) && 
         all (\block -> cbContent block `isInfixOf` content) blocks

prop_parse_directive_extraction :: String -> String -> Bool
prop_parse_directive_extraction directive content = 
  let fullContent = "//! " ++ directive ++ "\n" ++ content
      result = parseTypus fullContent
  in case result of
    Left _ -> False
    Right parsed -> 
      let directives = tfDirectives parsed
      in directives /= defaultFileDirectives

-- Test code block properties
prop_code_block_span_valid :: String -> Property
prop_code_block_span_valid content = 
  not (null content) ==> 
  let result = parseTypus content
  in case result of
    Left _ -> False
    Right parsed -> 
      let blocks = tfBlocks parsed
      in all (\block -> isValidSpan (cbSpan block)) blocks

prop_code_block_content_preserved :: String -> Property
prop_code_block_content_preserved content = 
  not (null content) ==> 
  let result = parseTypus content
  in case result of
    Left _ -> False
    Right parsed -> 
      let blocks = tfBlocks parsed
      in all (\block -> not (null (cbContent block))) blocks

-- Test error handling properties
prop_parse_invalid_syntax_reports_errors :: String -> Property
prop_parse_invalid_syntax_reports_errors content = 
  content `isInfixOf` "INVALID_SYNTAX_TOKEN" ==> 
  let result = parseTypus content
  in case result of
    Left _ -> True
    Right parsed -> not (null (tfSyntaxErrors parsed))

prop_parse_with_errors_still_returns_structure :: String -> Property
prop_parse_with_errors_still_returns_structure content = 
  content `isInfixOf` "INVALID_SYNTAX_TOKEN" ==> 
  let result = parseTypus content
  in case result of
    Left _ -> False
    Right parsed -> tfDirectives parsed /= undefined &&
                   tfBlocks parsed /= undefined

-- Test build tag parsing properties
prop_build_tags_extracted_correctly :: String -> Bool
prop_build_tags_extracted_correctly tag = 
  let content = "//! build_tags: " ++ tag ++ "\n"
      result = parseTypus content
  in case result of
    Left _ -> False
    Right parsed -> 
      let buildTags = tfBuildTags parsed
      in not (null buildTags) && 
         any (\bt -> isInfixOf tag (locValue bt)) buildTags

prop_multiple_build_tags_preserved :: [String] -> Property
prop_multiple_build_tags_preserved tags = 
  not (null tags) && length tags <= 5 ==> 
  let tagStr = unwords tags
      content = "//! build_tags: " ++ tagStr ++ "\n"
      result = parseTypus content
  in case result of
    Left _ -> False
    Right parsed -> 
      let buildTags = tfBuildTags parsed
          tagValues = map locValue buildTags
      in all (`elem` tagValues) tags

-- Test directive override properties
prop_block_directives_override_file :: String -> String -> Property
prop_block_directives_override_file fileDirective blockDirective = 
  let content = "//! ownership: true\n//@ ownership: false\n" ++ blockDirective
      result = parseTypus content
  in case result of
    Left _ -> False
    Right parsed -> 
      let blocks = tfBlocks parsed
      in not (null blocks) &&
         all (\block -> bdOwnership (cbDirectives block) /= 
                         fdOwnership (tfDirectives parsed)) blocks

prop_directive_parsing_case_sensitive :: String -> Bool
prop_directive_parsing_case_sensitive directive = 
  let upperDirective = map toUpper directive
      content1 = "//! " ++ directive ++ ": true\n"
      content2 = "//! " ++ upperDirective ++ ": true\n"
      result1 = parseTypus content1
      result2 = parseTypus content2
  in case (result1, result2) of
    (Right parsed1, Right parsed2) -> parsed1 /= parsed2
    _ -> True

-- Test parsing round-trip properties
prop_parse_format_parse_roundtrip :: String -> Property
prop_parse_format_parse_roundtrip content = 
  not (null content) && not (any (`isInfixOf` content) ["INVALID_SYNTAX_TOKEN"]) ==> 
  let result1 = parseTypus content
  in case result1 of
    Left _ -> discard
    Right parsed1 -> 
      let formatted = formatTypusFile parsed1
          result2 = parseTypus formatted
      in case result2 of
        Left _ -> False
        Right parsed2 -> tfDirectives parsed2 == tfDirectives parsed1 &&
                         length (tfBlocks parsed2) == length (tfBlocks parsed1)

-- Test NFData instances
prop_typus_file_nfdata :: TypusFile -> Bool
prop_typus_file_nfdata file = rnf file == ()

prop_code_block_nfdata :: CodeBlock -> Bool
prop_code_block_nfdata block = rnf block == ()

prop_directives_nfdata :: FileDirectives -> Bool
prop_directives_nfdata directives = rnf directives == ()

-- Helper functions
isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False

isValidSpan :: SourceSpan -> Bool
isValidSpan span = spanStart span <= spanEnd span

formatTypusFile :: TypusFile -> String
formatTypusFile file = unlines $ concat [
    formatFileDirectives (tfDirectives file),
    formatBuildTags (tfBuildTags file),
    concatMap formatCodeBlock (tfBlocks file)
  ]

formatFileDirectives :: FileDirectives -> [String]
formatFileDirectives directives = 
  let ownership = case fdOwnership directives of
        Just (Located True _ _) -> ["//! ownership: true"]
        Just (Located False _ _) -> ["//! ownership: false"]
        Nothing -> []
      dependentTypes = case fdDependentTypes directives of
        Just (Located True _ _) -> ["//! dependent_types: true"]
        Just (Located False _ _) -> ["//! dependent_types: false"]
        Nothing -> []
      constraints = case fdConstraints directives of
        Just (Located True _ _) -> ["//! constraints: true"]
        Just (Located False _ _) -> ["//! constraints: false"]
        Nothing -> []
  in ownership ++ dependentTypes ++ constraints

formatBuildTags :: [Located String] -> [String]
formatBuildTags tags = 
  if null tags then []
  else ["//! build_tags: " ++ unwords (map locValue tags)]

formatCodeBlock :: CodeBlock -> String
formatCodeBlock block = cbContent block

-- Arbitrary instances
instance Arbitrary FileDirectives where
  arbitrary = do
    ownership <- arbitrary
    dependentTypes <- arbitrary
    constraints <- arbitrary
    return $ FileDirectives ownership dependentTypes constraints

instance Arbitrary BlockDirectives where
  arbitrary = do
    ownership <- arbitrary
    dependentTypes <- arbitrary
    constraints <- arbitrary
    return $ BlockDirectives ownership dependentTypes constraints

instance Arbitrary CodeBlock where
  arbitrary = do
    directives <- arbitrary
    content <- arbitrary
    span <- arbitrary
    return $ CodeBlock directives content span

instance Arbitrary TypusFile where
  arbitrary = do
    directives <- arbitrary
    buildTags <- arbitrary
    blocks <- arbitrary
    syntaxErrors <- arbitrary
    return $ TypusFile directives buildTags blocks syntaxErrors

instance Arbitrary a => Arbitrary (Located a) where
  arbitrary = do
    value <- arbitrary
    pos <- arbitrary
    span <- arbitrary
    return $ Located value pos span

tests :: TestTree
tests = $(testGroupGenerator)

main :: IO ()
main = defaultMain tests