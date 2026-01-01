{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewCabalComprehensiveSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool, assertFailure, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import TestSupport.Arbitrary

-- Core modules to test
import Utils
  ( trim
  , splitBy
  , splitByCollapsed
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
  , advancePosByText
  , emptySpan
  , spanFrom
  , spanTo
  , mergeSpans
  , locatedAt
  , locatedValue
  , mapLocated
  , isValidSpan
  , toErrorLocation
  )

import qualified Data.Text as T
import Data.Char (isSpace, isAlphaNum)
import qualified Data.List as Data.List
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf)
import Data.List (sort)

-- ============================================================================
-- Unit Tests
-- ============================================================================

tests :: TestTree
tests =
  testGroup "New Cabal Comprehensive Tests"
    [ testGroup "Utils module tests"
        [ testCase "trim handles Unicode whitespace correctly" $ do
            trim "\x2000\x2001content\x2002\x2003" @?= "content"
            
        , testCase "splitBy handles empty string with Unicode delimiter" $ do
            splitBy '。' "" @?= [""]
            
        , testCase "removeLineComments preserves escaped comment markers" $ do
            let input = "code := value // comment\nmore := \"string // not comment\"\n"
                expected = "code := value \nmore := \"string // not comment\"\n"
            removeLineComments input @?= expected
            
        , testCase "removeComments handles nested block comments gracefully" $ do
            let input = "code /* outer /* inner */ still outer */ end"
                expected = "code  end"
            removeComments input @?= expected
            
        , testCase "normalizeIndentation preserves relative indentation" $ do
            let input = "  line1\n    line2\n  line3\n"
                expected = "line1\n  line2\nline3\n"
            normalizeIndentation input @?= expected
        ]
        
    , testGroup "SourceLocation module tests"
        [ testCase "SourcePos arithmetic handles multi-byte characters" $ do
            let initial = SourcePos 1 1 0
                afterEmoji = advancePosByText (T.pack "😀") initial
            afterEmoji @?= SourcePos 1 2 4  -- Emoji takes 4 bytes
            
        , testCase "mergeSpans handles overlapping spans correctly" $ do
            let spanA = SourceSpan (SourcePos 1 1 0) (SourcePos 1 10 9)
                spanB = SourceSpan (SourcePos 1 5 4) (SourcePos 1 15 14)
                merged = mergeSpans spanA spanB
            spanStart merged @?= SourcePos 1 1 0
            spanEnd merged @?= SourcePos 1 15 14
            
        , testCase "locatedAt creates valid spans" $ do
            let pos = SourcePos 5 3 20
                located = locatedAt pos "test"
            locatedValue located @?= "test"
            isValidSpan (locatedSpan located) @?= True
            
        , testCase "mapLocated preserves location information" $ do
            let original = Located "hello" (SourcePos 1 1 0) (SourceSpan (SourcePos 1 1 0) (SourcePos 1 6 5))
                mapped = mapLocated L.reverse original
            locatedSpan mapped @?= locatedSpan original
            locatedValue mapped @?= "olleh"
        ]
        
    , testGroup "Integration tests"
        [ testCase "Utils L.and SourceLocation integration" $ do
            let code = "  x := 1  // comment\n  y := 2"
                cleaned = removeLineComments code
                linesList = lines cleaned
                startPos' = startPos
                posAfterFirstLine = advancePosByText (T.pack (L.head linesList)) startPos'
            L.length linesList @?= 2
            posAfterFirstLine @?= SourcePos 2 1 24
        ]
    ]

-- ============================================================================
-- QuickCheck Properties
-- ============================================================================

-- Property: trim is idempotent
prop_trim_idempotent :: String -> Property
prop_trim_idempotent str =
  let trimmedOnce = trim str
      trimmedTwice = trim trimmedOnce
  in property $ trimmedOnce === trimmedTwice

-- Property: splitBy L.and splitByCollapsed relationship
prop_splitBy_relationship :: String -> Char -> Property
prop_splitBy_relationship str delim =
  let normal = splitBy delim str
      collapsed = splitByCollapsed delim str
      emptyCount = L.length (L.filter (== "") normal)
  in classify (emptyCount > 0) "has empty segments" $
     property $ L.length collapsed + emptyCount === L.length normal

-- Property: removeComments is idempotent
prop_removeComments_idempotent :: String -> Property
prop_removeComments_idempotent str =
  let cleanedOnce = removeComments str
      cleanedTwice = removeComments cleanedOnce
  in property $ cleanedOnce === cleanedTwice

-- Property: SourcePos advancement is consistent
prop_sourcepos_consistent :: String -> Property
prop_sourcepos_consistent str =
  let text = T.pack str
      finalPos = advancePosByText text startPos
      lineCount = L.length $ T.lines text
      lastLine = if T.null text then "" else T.last $ T.lines text
      colCount = T.L.length lastLine + 1
  in property $ sourceLine finalPos === fromIntegral lineCount .&&.
                sourceColumn finalPos === fromIntegral colCount

-- Property: mergeSpans is commutative
prop_mergeSpans_commutative :: SourcePos -> SourcePos -> SourcePos -> SourcePos -> Property
prop_mergeSpans_commutative start1 end1 start2 end2 =
  let span1 = SourceSpan start1 end1
      span2 = SourceSpan start2 end2
      merged1 = mergeSpans span1 span2
      merged2 = mergeSpans span2 span1
  in property $ merged1 === merged2

-- Property: locatedAt L.and mapLocated composition
prop_located_map_composition :: String -> Property
prop_located_map_composition str =
  let pos = startPos
      located = locatedAt pos str
      mapped1 = mapLocated L.length located
      mapped2 = mapLocated (show . L.length) located
  in property $ locatedValue mapped1 === L.length str .&&.
                locatedValue mapped2 === show (L.length str)

-- Property: removeLineComments preserves line structure
prop_removeLineComments_preserves_lines :: String -> Property
prop_removeLineComments_preserves_lines str =
  let originalLines = lines str
      cleaned = removeLineComments str
      cleanedLines = lines cleaned
  in property $ L.length originalLines === L.length cleanedLines

-- Property: normalizeIndentation preserves non-empty lines
prop_normalizeIndentation_preserves_content :: String -> Property
prop_normalizeIndentation_preserves_content str =
  let normalized = normalizeIndentation str
      originalNonEmpty = L.filter (not . null) $ lines str
      normalizedNonEmpty = L.filter (not . null) $ lines normalized
  in property $ L.length originalNonEmpty === L.length normalizedNonEmpty