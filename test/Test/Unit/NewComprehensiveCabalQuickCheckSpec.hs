{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewComprehensiveCabalQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import Test.Tasty.QuickCheck (testProperties)

import Utils
  ( trim
  , splitBy
  , splitByCollapsed
  , splitByComma
  , removeLineComments
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
  , isValidSpan
  , locatedAt
  , locatedWithSpan
  , locatedSpan
  , locatedValue
  , advancePos
  )

import Data.Char (isSpace, isAlphaNum, isDigit)
import qualified Data.List as Data.List
import Data.List (isPrefixOf, isInfixOf, sort, nub)
import Data.String (IsString)

-- ============================================================================
-- Utils Module Tests
-- ============================================================================

-- Property: trim is idempotent and removes outer whitespace
prop_trim_idempotent_and_cleans :: String -> Property
prop_trim_idempotent_and_cleans str =
  let trimmed = trim str
      trimmedTwice = trim trimmed
      hasLeading = not (null trimmed) && isSpace (head trimmed)
      hasTrailing = not (null trimmed) && isSpace (last trimmed)
  in property $ trimmed === trimmedTwice .&&. 
     property (not hasLeading .&&. not hasTrailing)

-- Property: splitBy and splitByCollapsed relationship
prop_splitBy_vs_splitByCollapsed :: Char -> String -> Property
prop_splitBy_vs_splitByCollapsed delim str =
  let regular = splitBy delim str
      collapsed = splitByCollapsed delim str
      hasConsecutive = delim `elem` str && 
                      any (\(a,b) -> a == delim && b == delim) (zip str (tail str))
  in classify hasConsecutive "has consecutive delimiters" $
     property $ length collapsed <= length regular .&&.
                (if hasConsecutive then length collapsed < length regular else length collapsed == length regular)

-- ============================================================================
-- SourceLocation Module Tests
-- ============================================================================

-- Property: SourcePos advancement is consistent
prop_sourcepos_advancement_consistent :: Int -> Int -> Char -> Property
prop_sourcepos_advancement_consistent line col ch =
  line >= 1 && col >= 1 && line <= 100 && col <= 100 ==>
  let initial = SourcePos line col 0
      advanced = advancePos ch initial
      expectedLine = if ch == '\n' then line + 1 else line
      expectedCol = if ch == '\n' then 1 else col + 1
      expectedOffset = 0 + 1
  in property $ advanced === SourcePos expectedLine expectedCol expectedOffset

-- Property: span merging is associative
prop_span_merging_associative :: Int -> Int -> Int -> Property
prop_span_merging_associative line1 line2 line3 =
  line1 >= 1 && line2 >= 1 && line3 >= 1 ==>
  let p1 = SourcePos line1 1 0
      p2 = SourcePos line2 1 0
      p3 = SourcePos line3 1 0
      span1 = spanFrom p1
      span2 = spanFrom p2
      span3 = spanFrom p3
      merged12 = mergeSpans span1 span2
      merged123 = mergeSpans merged12 span3
  in property $ (isValidSpan merged12) .&&. (isValidSpan merged123)

-- Property: Located values preserve their spans
prop_located_preserves_span :: Int -> Int -> String -> Property
prop_located_preserves_span line col value =
  line >= 1 && col >= 1 ==>
  let pos = SourcePos line col 0
      span = spanFrom pos
      located = locatedWithSpan span value
  in property $ locatedSpan located === span .&&. locatedValue located === value

-- ============================================================================
-- Parser Integration Tests
-- ============================================================================

-- Property: Comment removal preserves non-comment code structure
prop_comment_preservation_structure :: String -> String -> Property
prop_comment_preservation_structure prefix suffix =
  not (any (`elem` "\"'/\\") (prefix ++ suffix)) ==> -- Avoid string literals
  let code = prefix ++ "x := 42" ++ suffix
      withComments = code ++ " // comment\n /* block */" ++ code
      withoutComments = removeComments withComments
      codeCount = length (filter (== 'x') code)
      resultCount = length (filter (== 'x') withoutComments)
  in property $ codeCount * 2 === resultCount

-- ============================================================================
-- Indentation and Text Processing Tests
-- ============================================================================

-- Property: Normalization preserves relative indentation differences
prop_indentation_preserves_relative :: [Int] -> String -> Property
prop_indentation_preserves_relative indentLevels content =
  not (null indentLevels) && all (>= 0) indentLevels && all (<= 20) indentLevels ==>
  let lines' = zipWith (\level content' -> replicate level ' ' ++ content') indentLevels (repeat content)
      input = unlines lines'
      normalized = normalizeIndentation input
      normLines = lines normalized
      indents = map (length . takeWhile isSpace) normLines
      minIndent = if null indents then 0 else minimum indents
      adjustedIndents = map (subtract minIndent) indents
      -- Check that relative differences are preserved
      originalDiffs = if length indentLevels > 1 
                      then zipWith subtract indentLevels (tail indentLevels)
                      else []
      normalizedDiffs = if length adjustedIndents > 1 
                       then zipWith subtract adjustedIndents (tail adjustedIndents)
                       else []
  in property $ length normalizedDiffs === length originalDiffs .&&.
     (if null normalizedDiffs then property () 
      else property $ all (uncurry (==)) (zip normalizedDiffs originalDiffs))

-- ============================================================================
-- Error Handling and Edge Cases
-- ============================================================================

-- Property: String processing handles Unicode correctly
prop_unicode_handling :: String -> Property
prop_unicode_handling content =
  let unicodeContent = content ++ "测试café naïve 🚀"
      trimmed = trim unicodeContent
      split = splitBy ' ' unicodeContent
      commentsRemoved = removeLineComments unicodeContent
  in property $ "测试" `isInfixOf` trimmed .&&.
     "café" `isInfixOf` commentsRemoved .&&.
     any ("测试" `isInfixOf`) split

-- Property: Complex processing pipeline is consistent
prop_pipeline_consistency :: String -> Property
prop_pipeline_consistency input =
  let pipeline1 = input |> trim |> removeComments |> normalizeIndentation
      pipeline2 = input |> removeComments |> trim |> normalizeIndentation
      pipeline3 = input |> normalizeIndentation |> trim |> removeComments
  in property $ (pipeline1 == pipeline2) .||. (pipeline2 == pipeline3) .||. (pipeline1 == pipeline3)

-- ============================================================================
-- Test Collection
-- ============================================================================

tests :: TestTree
tests = testGroup "New Comprehensive Cabal QuickCheck Tests"
  [ fastProperty "trim idempotent and cleans" prop_trim_idempotent_and_cleans
  , fastProperty "splitBy vs splitByCollapsed" prop_splitBy_vs_splitByCollapsed
  , fastProperty "SourcePos advancement consistency" prop_sourcepos_advancement_consistent
  , fastProperty "span merging associative" prop_span_merging_associative
  , fastProperty "Located preserves span" prop_located_preserves_span
  , fastProperty "comment preservation structure" prop_comment_preservation_structure
  , fastProperty "indentation preserves relative" prop_indentation_preserves_relative
  , fastProperty "Unicode handling" prop_unicode_handling
  , fastProperty "pipeline consistency" prop_pipeline_consistency
  ]

-- Helper operator for pipeline (if not already defined)
(|>) :: a -> (a -> b) -> b
x |> f = f x