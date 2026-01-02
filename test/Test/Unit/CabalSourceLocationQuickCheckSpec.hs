{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.CabalSourceLocationQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import qualified Data.List as List
import Data.Char (isSpace, isAlphaNum, isLetter)
import Data.Maybe (isJust, isNothing)

import SourceLocation
  ( SourcePos(..), SourceSpan(..), Located(..)
  , posLine, posColumn, spanStart, spanEnd
  , mkSourceSpan, mkSourcePos, locatedAt
  )

-- Simple arbitrary instances for source location testing
instance Arbitrary SourcePos where
  arbitrary = do
    line <- arbitrary `suchThat` (> 0)
    column <- arbitrary `suchThat` (> 0)
    return $ SourcePos line column

instance Arbitrary SourceSpan where
  arbitrary = do
    startLine <- arbitrary `suchThat` (> 0)
    startCol <- arbitrary `suchThat` (> 0)
    endLine <- arbitrary `suchThat` (>= startLine)
    endCol <- if endLine == startLine 
                then arbitrary `suchThat` (>= startCol)
                else arbitrary `suchThat` (> 0)
    return $ SourceSpan (SourcePos startLine startCol) (SourcePos endLine endCol)

newtype LocatedString = LocatedString (Located String) deriving (Show)

instance Arbitrary LocatedString where
  arbitrary = do
    span <- arbitrary
    content <- listOf $ elements ['a'..'z']
    return $ LocatedString $ Located content span

-- Property: Source position line numbers are preserved
prop_source_pos_line_preserved :: Int -> Property
prop_source_pos_line_preserved lineNum =
  let line = abs lineNum `mod` 1000 + 1
      pos = mkSourcePos line 1
  in posLine pos === line

-- Property: Source position column numbers are preserved
prop_source_pos_column_preserved :: Int -> Property
prop_source_pos_column_preserved colNum =
  let col = abs colNum `mod` 100 + 1
      pos = mkSourcePos 1 col
  in posColumn pos === col

-- Property: Source span start position is preserved
prop_source_span_start_preserved :: SourcePos -> SourcePos -> Property
prop_source_span_start_preserved start end =
  let adjustedEnd = if posLine end < posLine start || 
                       (posLine end == posLine start && posColumn end < posColumn start)
                   then start
                   else end
      span = mkSourceSpan start adjustedEnd
  in spanStart span === start

-- Property: Source span end position is preserved
prop_source_span_end_preserved :: SourcePos -> SourcePos -> Property
prop_source_span_end_preserved start end =
  let adjustedEnd = if posLine end < posLine start || 
                       (posLine end == posLine start && posColumn end < posColumn start)
                   then start
                   else end
      span = mkSourceSpan start adjustedEnd
  in spanEnd span === adjustedEnd

-- Property: Located values preserve their content
prop_located_preserves_content :: String -> SourceSpan -> Property
prop_located_preserves_content content span =
  let located = locatedAt content span
  in locatedValue located === content

-- Property: Located values preserve their span
prop_located_preserves_span :: String -> SourceSpan -> Property
prop_located_preserves_span content span =
  let located = locatedAt content span
  in locSpan located === span

-- Property: Source span ordering is consistent
prop_source_span_ordering :: SourcePos -> SourcePos -> Property
prop_source_span_ordering pos1 pos2 =
  let span1 = mkSourceSpan pos1 pos1
      span2 = mkSourceSpan pos2 pos2
      start1 = spanStart span1
      start2 = spanStart span2
      sameLine = posLine start1 == posLine start2
      sameCol = posColumn start1 == posColumn start2
  in property $ (sameLine .&&. sameCol) ==> (span1 == span2)

tests :: TestTree
tests = testGroup "Cabal Source Location QuickCheck Tests"
  [ fastProperty "Source pos line preserved" prop_source_pos_line_preserved
  , fastProperty "Source pos column preserved" prop_source_pos_column_preserved
  , fastProperty "Source span start preserved" prop_source_span_start_preserved
  , fastProperty "Source span end preserved" prop_source_span_end_preserved
  , fastProperty "Located preserves content" prop_located_preserves_content
  , fastProperty "Located preserves span" prop_located_preserves_span
  , fastProperty "Source span ordering consistent" prop_source_span_ordering
  , testCase "Source location handles multi-line spans" $ do
      let start = mkSourcePos 1 5
          end = mkSourcePos 3 10
          span = mkSourceSpan start end
      posLine (spanStart span) @?= 1
      posColumn (spanStart span) @?= 5
      posLine (spanEnd span) @?= 3
      posColumn (spanEnd span) @?= 10
  , testCase "Source location handles single-character spans" $ do
      let pos = mkSourcePos 2 8
          span = mkSourceSpan pos pos
      spanStart span @?= spanEnd span
  ]