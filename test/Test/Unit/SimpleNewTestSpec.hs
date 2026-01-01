{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.SimpleNewTestSpec where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual, assertBool)
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck (Arbitrary(..), Gen, Property, (==>), forAll, choose, listOf1, elements, resize)
import TestSupport.QuickCheck (fastProperty)
import qualified Data.List as L
import Data.Char (isSpace)
import Data.Maybe (isJust, isNothing)

import Utils (trim, splitBy, splitByCollapsed, splitByComma)
import SourceLocation (SourcePos(..), SourceSpan(..), Located(..), startPos, posAfter, emptySpan, spanFrom, isValidSpan)

-- ============================================================================
-- Test Suite Definition
-- ============================================================================

tests :: TestTree
tests = testGroup "Simple New Test Suite"
  [ textProcessingProperties
  , sourceLocationProperties
  , basicProperties
  ]

-- ============================================================================
-- Text Processing Properties
-- ============================================================================

textProcessingProperties :: TestTree
textProcessingProperties = testGroup "Text Processing Properties"
  [ testProperty "trim is idempotent" $
      \s -> trim (trim s) == trim s

  , testProperty "splitBy preserves concatenation with delimiter" $
      \c s -> splitBy c s `L.intercalate` [c] == s

  , testProperty "splitByCollapsed removes empty segments" $
      \c s -> L.all (not . null) (splitByCollapsed c s)

  , testCase "trim handles empty strings" $
      assertEqual "trim empty" "" (trim "")

  , testCase "trim handles whitespace-only strings" $
      assertEqual "trim whitespace" "" (trim "   \t\n  ")
  ]

-- ============================================================================
-- Source Location Properties
-- ============================================================================

sourceLocationProperties :: TestTree
sourceLocationProperties = testGroup "Source Location Properties"
  [ testProperty "posAfter advances column by 1 for same line" $
      \pos -> posAfter pos (SourcePos 0 1) == SourcePos (sourceLine pos) (sourceColumn pos + 1)

  , testProperty "spanFrom creates valid spans" $
      \pos -> isValidSpan (spanFrom pos 5)

  , testProperty "emptySpan has zero L.length" $
      \pos -> let span = emptySpan pos
              in spanStart span == spanEnd span

  , testCase "startPos is at line 1, column 1" $
      assertEqual "startPos" (SourcePos 1 1) startPos
  ]

-- ============================================================================
-- Basic Properties
-- ============================================================================

basicProperties :: TestTree
basicProperties = testGroup "Basic Properties"
  [ testProperty "located values preserve their content" $
      \pos val -> locatedValue (Located pos val) == val

  , testProperty "located values track their position" $
      \pos val -> locatedPos (Located pos val) == pos

  , testProperty "splitByComma handles consecutive commas" $
      \s -> splitByComma (",," ++ s ++ ",,") `L.L.isPrefixOf` ["", "", ""]

  , testCase "splitBy handles single character strings" $
      assertEqual "single char" ["", ""] (splitBy ',' [','])
  ]