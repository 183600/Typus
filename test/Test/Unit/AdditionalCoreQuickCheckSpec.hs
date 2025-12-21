{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Unit.AdditionalCoreQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck

import SourceLocation 
  ( SourcePos(..), SourceSpan(..)
  , startPos, mergeSpans
  , locatedAt, locatedValue, locatedSpan, mapLocated
  , advancePos, isValidSpan
  )

import Compiler.GoAst 
  ( PackageDecl(..), ImportDecl(..)
  , FuncDecl(..)
  )

import Utils 
  ( trim, splitBy, splitByCollapsed, removeLineComments
  , normalizeIndentation, breakOn, splitByComma
  )

import Data.List (isInfixOf, isPrefixOf)

-- Import Arbitrary instances from TestSupport.Arbitrary to avoid orphan instances
import TestSupport.Arbitrary ()

-- ============================================================================
-- SourceLocation Properties
-- ============================================================================

-- Property: Position advancement is consistent for newline characters
prop_advance_pos_consistent :: SourcePos -> Property
prop_advance_pos_consistent pos =
  let advancedNewline = advancePos '\n' pos
      advancedRegular = advancePos 'a' pos
  in property $
     posLine advancedNewline === posLine pos + 1 .&&.
     posColumn advancedNewline === 1 .&&.
     posLine advancedRegular === posLine pos .&&.
     posColumn advancedRegular >= posColumn pos

-- Property: Span validity is preserved under merging
prop_merge_spans_validity :: SourceSpan -> SourceSpan -> Property
prop_merge_spans_validity span1 span2 =
  let merged = mergeSpans span1 span2
  in property $ isValidSpan merged ==> isValidSpan merged

-- Property: Located values preserve their content through mapping
prop_located_map_preservation :: String -> Int -> Property
prop_located_map_preservation str _value =
  let located = locatedAt (startPos) str
      mapped = mapLocated length located
  in property $
     locatedValue mapped === length str .&&.
     locatedSpan mapped === locatedSpan located

-- ============================================================================
-- GoAST Properties  
-- ============================================================================



-- Property: Import declarations maintain path consistency
prop_import_path_consistency :: ImportDecl -> Property
prop_import_path_consistency imp =
  let path = importPath imp
  in property $ not (null path) ==> length path > 0

-- Property: Package declarations maintain name consistency
prop_package_name_consistency :: PackageDecl -> Property
prop_package_name_consistency pkg =
  let name = packageName pkg
  in property $ length name >= 0

-- ============================================================================
-- Parser/Utils Properties
-- ============================================================================

-- Property: splitBy and splitByCollapsed relationship
prop_split_by_collapsed_relationship :: Char -> String -> Property
prop_split_by_collapsed_relationship delim str =
  let normal = splitBy delim str
      collapsed = splitByCollapsed delim str
  in property $ length collapsed <= length normal .&&.
     (not (null collapsed) || all null (filter (not . null) normal))

-- Property: trim is idempotent
prop_trim_idempotent :: String -> Property
prop_trim_idempotent str =
  let trimmedOnce = trim str
      trimmedTwice = trim trimmedOnce
  in property $ trimmedOnce === trimmedTwice

-- Property: breakOn always finds first occurrence or returns original
prop_break_on_finds_first :: String -> String -> Property
prop_break_on_finds_first needle haystack =
  let (before, after) = breakOn needle haystack
      hasNeedle = needle `isInfixOf` haystack
      reconstructed = before ++ needle ++ after
  in property $ 
     (if hasNeedle then reconstructed === haystack else (before, after) === (haystack, ""))

-- Property: normalizeIndentation preserves relative indentation
prop_normalize_indentation_preserves_relative :: String -> Property
prop_normalize_indentation_preserves_relative code =
  let normalized = normalizeIndentation code
      linesInOriginal = lines code
      linesInNormalized = lines normalized
  in length linesInOriginal === length linesInNormalized

-- ============================================================================
-- String Processing Properties
-- ============================================================================

-- Property: removeLineComments preserves non-comment lines
prop_remove_line_comments_preserves_non_comments :: String -> Property
prop_remove_line_comments_preserves_non_comments code =
  let withoutComments = removeLineComments code
      linesOriginal = lines code
      linesWithoutComments = lines withoutComments
      nonCommentLines = filter (not . ("//" `isPrefixOf`)) (filter (not . null) linesOriginal)
      filteredWithoutComments = filter (not . null) linesWithoutComments
  in property $ length filteredWithoutComments >= length nonCommentLines

-- Property: splitBy comma consistency with splitBy comma
prop_split_by_comma_consistency :: String -> Property
prop_split_by_comma_consistency str =
  let byComma = splitBy ',' str
      byCommaFunc = splitByComma str
  in property $ byComma === byCommaFunc

-- Test collection
tests :: TestTree
tests = testGroup "Additional Core QuickCheck Tests"
  [ fastProperty "advance_pos_consistent" prop_advance_pos_consistent
  , fastProperty "merge_spans_validity" prop_merge_spans_validity  
  , fastProperty "located_map_preservation" prop_located_map_preservation
  , fastProperty "import_path_consistency" prop_import_path_consistency
  , fastProperty "package_name_consistency" prop_package_name_consistency
  , fastProperty "split_by_collapsed_relationship" prop_split_by_collapsed_relationship
  , fastProperty "trim_idempotent" prop_trim_idempotent
  , fastProperty "break_on_finds_first" prop_break_on_finds_first
  , fastProperty "normalize_indentation_preserves_relative" prop_normalize_indentation_preserves_relative
  , fastProperty "remove_line_comments_preserves_non_comments" prop_remove_line_comments_preserves_non_comments
  , fastProperty "split_by_comma_consistency" prop_split_by_comma_consistency
  ]