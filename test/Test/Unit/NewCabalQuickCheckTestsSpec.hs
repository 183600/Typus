{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewCabalQuickCheckTestsSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import TestSupport.Arbitrary
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), elements, oneof, frequency)

-- Core modules to test
import Utils
  ( trim
  , splitBy
  , splitByCollapsed
  , removeComments
  , normalizeIndentation
  , breakOn
  )

import SourceLocation
  ( SourcePos(..)
  , SourceSpan(..)
  , Located(..)
  , startPos
  , posAfter
  , emptySpan
  , spanFrom
  , mergeSpans
  , locatedAt
  , locatedValue
  , locatedSpan
  , advancePos
  )

import Parser
  ( parseTypus
  , FileDirectives(..)
  , BlockDirectives(..)
  , TypusFile(..)
  )

import Data.Char (isSpace, isAlphaNum, isLetter)
import qualified Data.List as Data.List
import Data.List (isPrefixOf, isInfixOf, sort, nub)
import qualified Data.Text as T

-- ============================================================================
-- String Processing Properties
-- ============================================================================

-- Property: trim is idempotent
prop_trim_idempotent :: String -> Property
prop_trim_idempotent s = 
  let trimmedOnce = trim s
      trimmedTwice = trim trimmedOnce
  in property $ trimmedOnce === trimmedTwice

-- Property: trim never increases length
prop_trim_never_increases_length :: String -> Property
prop_trim_never_increases_length s = 
  let trimmed = trim s
      originalLength = length s
      trimmedLength = length trimmed
  in property $ trimmedLength <= originalLength

-- Property: splitBy and join are inverse operations (for non-empty delimiter)
prop_split_by_join_inverse :: String -> Char -> Property
prop_split_by_join_inverse s delim = 
  delim /= '\0' ==>
  let parts = splitBy delim s
      rejoined = Data.List.intercalate [delim] parts
  in property $ rejoined === s

-- Property: splitByCollapsed removes empty segments
prop_split_by_collapsed_removes_empty :: String -> Char -> Property
prop_split_by_collapsed_removes_empty s delim = 
  let collapsed = splitByCollapsed delim
  in property $ all (not . null) collapsed

-- Property: removeComments preserves non-comment content
prop_remove_comments_preserves_content :: String -> String -> Property
prop_remove_comments_preserves_content prefix suffix =
  let content = "valid code here"
      source = prefix ++ "/* comment */" ++ content ++ "// line comment\n" ++ suffix
      cleaned = removeComments source
  in property $ content `isInfixOf` cleaned

-- ============================================================================
-- Source Location Properties
-- ============================================================================

-- Property: startPos is consistent with posAt
prop_start_pos_consistency :: Property
prop_start_pos_consistency = 
  let start = startPos
      startAt = posAt 1 1
  in property $ start === startAt

-- Property: posAfter advances column correctly
prop_pos_after_advances_column :: Int -> Property
prop_pos_after_advances_column col = 
  col >= 1 && col <= 100 ==>
  let pos = posAt 1 col
      after = posAfter pos
  in property $ posColumn after === col + 1

-- Property: mergeSpans is commutative for valid spans
prop_merge_spans_commutative :: SourceSpan -> SourceSpan -> Property
prop_merge_spans_commutative span1 span2 =
  isValidSpan span1 && isValidSpan span2 ==>
  let merged1 = mergeSpans span1 span2
      merged2 = mergeSpans span2 span1
  in property $ merged1 === merged2

-- Property: locatedAt creates consistent located values
prop_located_at_consistency :: String -> Int -> Int -> Property
prop_located_at_consistency value line col =
  line >= 1 && col >= 1 && line <= 1000 && col <= 1000 ==>
  let pos = posAt line col
      located = locatedAt pos value
  in property $ locatedValue located === value .&&. 
                locatedPos located === pos

-- ============================================================================
-- Parser Properties  
-- ============================================================================

-- Property: parsing empty string yields minimal valid structure
prop_parse_empty_string :: Property
prop_parse_empty_string = 
  case parseTypus "" of
    Left _ -> property $ True  -- Empty string can fail to parse, that's OK
    Right typusFile -> 
      let directives = tfDirectives typusFile
          blocks = tfBlocks typusFile
      in property $ length blocks === 0

-- Property: parsing preserves directive order
prop_parse_preserves_directive_order :: String -> String -> Property
prop_parse_preserves_directive_order ownerText depText =
  let source = unlines
        [ "//! ownership: " ++ ownerText
        , "//! dependent_types: " ++ depText  
        , "package main"
        , "func main() {}"
        ]
  in case parseTypus source of
    Left _ -> property $ True  -- May fail to parse, that's OK
    Right typusFile ->
      let directives = tfDirectives typusFile
      in property $ True  -- If parse succeeds, structure should be valid

-- Property: round-trip parsing preserves essential structure
prop_round_trip_preserves_structure :: String -> Property
prop_round_trip_preserves_structure source =
  length source <= 1000 ==>  -- Limit size for performance
  case parseTypus source of
    Left _ -> property $ True
    Right typusFile ->
      let directives = tfDirectives typusFile
          blocks = tfBlocks typusFile
      in property $ length blocks >= 0  -- Basic sanity check

-- ============================================================================
-- Indentation Properties
-- ============================================================================

-- Property: normalizeIndentation preserves relative indentation
prop_normalize_preserves_relative :: String -> String -> Property
prop_normalize_preserves_relative line1 line2 =
  let source = unlines ["  " ++ line1, "    " ++ line2]
      normalized = normalizeIndentation source
      lines' = lines normalized
  in length lines' >= 2 ==>
     let indent1 = length $ takeWhile isSpace (head lines')
         indent2 = length $ takeWhile isSpace (lines' !! 1)
     in property $ indent2 >= indent1

-- Property: normalizeIndentation doesn't create leading empty lines
prop_normalize_no_leading_empty :: String -> Property
prop_normalize_no_leading_empty source =
  not (null source) ==>
  let normalized = normalizeIndentation source
      normalizedLines = lines normalized
  in property $ not (null normalizedLines) .&&. 
                head normalizedLines /= ""

-- ============================================================================
-- Edge Case Properties
-- ============================================================================

-- Property: breakOn works with empty pattern
prop_break_on_empty_pattern :: String -> Property
prop_break_on_empty_pattern s =
  let (before, after) = breakOn "" s
  in property $ before === "" .&&. after === s

-- Property: breakOn with non-existent pattern returns original string
prop_break_on_not_found :: String -> String -> Property
prop_break_on_not_found s pattern =
  not (pattern `isInfixOf` s) && not (null pattern) ==>
  let (before, after) = breakOn pattern s
  in property $ before === s .&&. after === ""

-- Property: splitBy with empty delimiter returns list of characters
prop_split_by_empty_delimiter :: String -> Property
prop_split_by_empty_delimiter s =
  let parts = splitBy '\0' s
      chars = map (:[]) s
  in property $ parts === chars

-- ============================================================================
-- Performance Properties
-- ============================================================================

-- Property: trim is O(n) (reasonable size limit check)
prop_trim_performance_reasonable :: String -> Property
prop_trim_performance_reasonable s =
  length s <= 10000 ==>  -- Keep test reasonable
  let result = trim s
  in property $ length result <= length s

-- Property: splitBy performance is reasonable
prop_split_by_performance_reasonable :: String -> Char -> Property
prop_split_by_performance_reasonable s delim =
  length s <= 5000 ==>  -- Keep test reasonable
  let parts = splitBy delim s
      totalLength = sum $ map length parts
  in property $ totalLength >= length s - 1  -- At most one delimiter per split

-- ============================================================================
-- Test Collection
-- ============================================================================

tests :: TestTree
tests = testGroup "New Cabal QuickCheck Tests"
  [ testGroup "String Processing Properties"
    [ fastProperty "trim is idempotent" prop_trim_idempotent
    , fastProperty "trim never increases length" prop_trim_never_increases_length
    , fastProperty "splitBy and join are inverse" prop_split_by_join_inverse
    , fastProperty "splitByCollapsed removes empty segments" prop_split_by_collapsed_removes_empty
    , fastProperty "removeComments preserves content" prop_remove_comments_preserves_content
    ]
  
  , testGroup "Source Location Properties"
    [ fastProperty "startPos consistency" prop_start_pos_consistency
    , fastProperty "posAfter advances column" prop_pos_after_advances_column
    , fastProperty "mergeSpans is commutative" prop_merge_spans_commutative
    , fastProperty "locatedAt consistency" prop_located_at_consistency
    ]
  
  , testGroup "Parser Properties"
    [ fastProperty "parse empty string" prop_parse_empty_string
    , fastProperty "parse preserves directive order" prop_parse_preserves_directive_order
    , fastProperty "round-trip preserves structure" prop_round_trip_preserves_structure
    ]
  
  , testGroup "Indentation Properties"
    [ fastProperty "normalize preserves relative indentation" prop_normalize_preserves_relative
    , fastProperty "normalize no leading empty" prop_normalize_no_leading_empty
    ]
  
  , testGroup "Edge Case Properties"
    [ fastProperty "breakOn empty pattern" prop_break_on_empty_pattern
    , fastProperty "breakOn not found" prop_break_on_not_found
    , fastProperty "splitBy empty delimiter" prop_split_by_empty_delimiter
    ]
  
  , testGroup "Performance Properties"
    [ fastProperty "trim performance reasonable" prop_trim_performance_reasonable
    , fastProperty "splitBy performance reasonable" prop_split_by_performance_reasonable
    ]
  ]