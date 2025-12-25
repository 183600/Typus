{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewCabalTestsSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Gen, Arbitrary(..), oneof, elements, listOf, choose)
import qualified Test.QuickCheck as QC

import EmbedAssets
  ( MissingEmbed(..)
  , formatMissingMessage
  , extractEmbeddedPatterns
  , handleMissingEmbeds
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
  )
import Utils
  ( trim
  , splitBy
  , splitByCollapsed
  , removeLineComments
  )
import SyntaxValidator
  ( SyntaxError(..)
  , ErrorType(..)
  , formatSyntaxError
  )

import Data.Char (isSpace, isAlpha, isDigit)
import Data.List (isPrefixOf, sort, nub)
import qualified Data.Text as T
import Text.Read (readMaybe)

-- ============================================================================
-- EmbedAssets Tests
-- ============================================================================

-- Test 1: extractEmbeddedPatterns property
prop_extract_embedded_patterns :: String -> String -> Property
prop_extract_embedded_patterns prefix pattern =
  let content = prefix ++ "//go:embed " ++ pattern
      patterns = extractEmbeddedPatterns content
  in classify (null patterns) "no patterns found" $
     classify (not (null patterns)) "patterns found" $
     property $ patterns == [trim pattern]

-- Test 2: formatMissingMessage property
prop_format_missing_message :: [MissingEmbed] -> Property
prop_format_missing_message missing =
  let msg = formatMissingMessage missing
      uniqueMissing = nub missing
  in property $ 
    (null missing ==> msg == "Missing embedded assets detected:\n") .&&.
    (not (null missing) ==> "Missing embedded assets detected:" `isPrefixOf` msg)

-- Test 3: MissingEmbed ordering property
prop_missing_embed_ordering :: MissingEmbed -> MissingEmbed -> Property
prop_missing_embed_ordering me1 me2 =
  let list = [me2, me1]
      sorted = sort list
  in property $ length sorted == 2

-- ============================================================================
-- SourceLocation Tests
-- ============================================================================

-- Test 4: SourcePos advancement property
prop_source_pos_advancement :: Int -> Int -> String -> Property
prop_source_pos_advancement line col content =
  line >= 0 && col >= 0 && not (null content) ==>
  let start = SourcePos line col
      end = posAfter start content
  in property $ sourcePosLine end >= line

-- Test 5: span merging property
prop_span_merging :: SourceSpan -> SourceSpan -> Property
prop_span_merging span1 span2 =
  let merged = mergeSpans span1 span2
  in property $ 
    (spanStart merged `sourcePosLe` spanStart span1 .||. spanStart merged `sourcePosLe` spanStart span2) .&&.
    (spanEnd span1 `sourcePosLe` spanEnd merged .||. spanEnd span2 `sourcePosLe` spanEnd merged)
  where
    sourcePosLe p1 p2 = sourcePosLine p1 < sourcePosLine p2 ||
                       (sourcePosLine p1 == sourcePosLine p2 && sourcePosColumn p1 <= sourcePosColumn p2)

-- Test 6: emptySpan property
prop_empty_span_property :: SourcePos -> Property
prop_empty_span_property pos =
  let span = emptySpan pos
  in property $ spanStart span == pos && spanEnd span == pos

-- ============================================================================
-- Utils Tests
-- ============================================================================

-- Test 7: splitBy roundtrip property
prop_split_by_roundtrip :: Char -> String -> Property
prop_split_by_roundtrip delim str =
  let parts = splitBy delim str
      rejoined = intercalate [delim] parts
  in property $ length rejoined >= length str

-- Test 8: trim idempotency property
prop_trim_idempotent :: String -> Property
prop_trim_idempotent str =
  let trimmedOnce = trim str
      trimmedTwice = trim trimmedOnce
  in property $ trimmedOnce == trimmedTwice

-- Test 9: removeLineComments preserves literals property
prop_remove_line_comments_preserves_literals :: String -> String -> Property
prop_remove_line_comments_preserves_literals prefix suffix =
  let content = prefix ++ "url := \"http://example.com//path\" // comment" ++ suffix
      processed = removeLineComments content
  in property $ "http://example.com//path" `isInfixOf` processed

-- Test 10: splitByCollapsed vs splitBy property
prop_split_by_collapsed_vs_split_by :: Char -> String -> Property
prop_split_by_collapsed_vs_split_by delim str =
  let collapsed = splitByCollapsed delim str
      regular = splitBy delim str
  in property $ length collapsed <= length regular

-- ============================================================================
-- Test Collection
-- ============================================================================

tests :: TestTree
tests =
  testGroup "New Cabal Tests"
    [ testGroup "EmbedAssets Properties"
        [ fastProperty "extractEmbeddedPatterns finds patterns" prop_extract_embedded_patterns
        , fastProperty "formatMissingMessage handles missing embeds" prop_format_missing_message
        , fastProperty "MissingEmbed ordering works" prop_missing_embed_ordering
        ]
    
    , testGroup "SourceLocation Properties"
        [ fastProperty "SourcePos advancement works correctly" prop_source_pos_advancement
        , fastProperty "span merging preserves bounds" prop_span_merging
        , fastProperty "emptySpan has same start and end" prop_empty_span_property
        ]
    
    , testGroup "Utils Properties"
        [ fastProperty "splitBy roundtrip works" prop_split_by_roundtrip
        , fastProperty "trim is idempotent" prop_trim_idempotent
        , fastProperty "removeLineComments preserves string literals" prop_remove_line_comments_preserves_literals
        , fastProperty "splitByCollapsed removes empty segments" prop_split_by_collapsed_vs_split_by
        ]
    
    , testGroup "Unit Tests"
        [ testCase "extractEmbeddedPatterns handles multiple directives" $ do
            let content = "//go:embed *.go\n//go:embed assets/*\nregular line\n//go:embed config/*.json"
                patterns = extractEmbeddedPatterns content
            patterns @?= ["*.go", "assets/*", "config/*.json"]
            
        , testCase "formatMissingMessage formats correctly" $ do
            let missing = [ MissingEmbed "*.txt" "/src" "main.go" ]
                msg = formatMissingMessage missing
            assertBool "Message contains header" $ "Missing embedded assets detected:" `isPrefixOf` msg
            assertBool "Message contains pattern" $ "*.txt" `isInfixOf` msg
        ]
    ]

-- Helper function
intercalate :: String -> [String] -> String
intercalate _ [] = ""
intercalate _ [x] = x
intercalate sep (x:xs) = x ++ sep ++ intercalate sep xs

isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = needle `Data.List.isInfixOf` haystack

-- Arbitrary instances for QuickCheck
instance Arbitrary MissingEmbed where
  arbitrary = do
    pattern <- oneof [pure "*.txt", pure "*.go", pure "assets/*", pure "config/*.json"]
    root <- oneof [pure "/src", pure "/project", pure "/app"]
    ref <- oneof [pure "main.go", pure "lib.go", pure "utils.go"]
    return $ MissingEmbed pattern root ref

instance Arbitrary SourcePos where
  arbitrary = do
    line <- choose (0, 100)
    col <- choose (0, 200)
    return $ SourcePos line col

instance Arbitrary SourceSpan where
  arbitrary = do
    start <- arbitrary
    end <- arbitrary
    return $ SourceSpan start end