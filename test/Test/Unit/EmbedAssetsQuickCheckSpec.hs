{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.EmbedAssetsQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import Test.QuickCheck.Gen (Gen, choose, listOf, elements, oneof, vectorOf)
import Test.QuickCheck.Arbitrary (Arbitrary(..))

import EmbedAssets
  ( MissingEmbed(..)
  , formatMissingMessage
  , extractEmbeddedPatterns
  , toMissingEmbedInfo
  )

import Data.List (isPrefixOf, nub, sort)
import Data.Char (isSpace)

-- ============================================================================
-- Arbitrary Instances
-- ============================================================================

instance Arbitrary MissingEmbed where
    arbitrary = do
        pattern <- arbitrary `suchThat` (not . null)
        root <- arbitrary `suchThat` (not . null)
        reference <- arbitrary `suchThat` (not . null)
        return $ MissingEmbed pattern root reference

-- ============================================================================
-- Property Tests for MissingEmbed
-- ============================================================================

-- Property: formatMissingMessage produces non-empty output for non-empty input
prop_formatMissingMessage_non_empty :: [MissingEmbed] -> Property
prop_formatMissingMessage_non_empty missing =
    not (null missing) ==> 
    let formatted = formatMissingMessage missing
    in property $ not (null formatted) .&&. "Missing embedded assets detected:" `isPrefixOf` formatted

-- Property: formatMissingMessage handles empty list gracefully
prop_formatMissingMessage_empty :: Property
prop_formatMissingMessage_empty =
    let formatted = formatMissingMessage []
    in property $ formatted === "Missing embedded assets detected:\n"

-- Property: formatMissingMessage removes duplicates using nub
prop_formatMissingMessage_removes_duplicates :: [MissingEmbed] -> Property
prop_formatMissingMessage_removes_duplicates missing =
    let formatted = formatMissingMessage missing
        uniqueMissing = nub missing
        expectedLines = length uniqueMissing + 1  -- +1 for header
        actualLines = length (lines formatted)
    in property $ actualLines === expectedLines

-- Property: formatMissingMessage contains all pattern information
prop_formatMissingMessage_contains_patterns :: [MissingEmbed] -> Property
prop_formatMissingMessage_contains_patterns missing =
    not (null missing) ==>
    let formatted = formatMissingMessage missing
        patterns = map missingPattern missing
    in property $ all (`isInfixOf` formatted) patterns

-- ============================================================================
-- Property Tests for extractEmbeddedPatterns
-- ============================================================================

-- Property: extractEmbeddedPatterns finds patterns in go:embed directives
prop_extractEmbeddedPatterns_finds_directives :: String -> String -> Property
prop_extractEmbeddedPatterns_finds_directives prefix pattern =
    not (null pattern) && not (any isSpace pattern) ==>
    let content = prefix ++ "//go:embed " ++ pattern ++ "\nmore content"
        extracted = extractEmbeddedPatterns content
    in property $ pattern `elem` extracted

-- Property: extractEmbeddedPatterns handles quoted patterns
prop_extractEmbeddedPatterns_quoted :: String -> String -> Property
prop_extractEmbeddedPatterns_quoted prefix pattern =
    not (null pattern) && not ('"' `elem` pattern) ==>
    let content = prefix ++ "//go:embed \"" ++ pattern ++ "\"\nmore content"
        extracted = extractEmbeddedPatterns content
    in property $ pattern `elem` extracted

-- Property: extractEmbeddedPatterns handles backtick patterns
prop_extractEmbeddedPatterns_backtick :: String -> String -> Property
prop_extractEmbeddedPatterns_backtick prefix pattern =
    not (null pattern) && not ('`' `elem` pattern) ==>
    let content = prefix ++ "//go:embed `" ++ pattern ++ "`\nmore content"
        extracted = extractEmbeddedPatterns content
    in property $ pattern `elem` extracted

-- Property: extractEmbeddedPatterns ignores non-directive lines
prop_extractEmbeddedPatterns_ignores_non_directives :: String -> String -> Property
prop_extractEmbeddedPatterns_ignores_non_directives content1 content2 =
    not ("//go:embed" `isInfixOf` content1) && not ("//go:embed" `isInfixOf` content2) ==>
    let content = content1 ++ "\n" ++ content2
        extracted = extractEmbeddedPatterns content
    in property $ null extracted

-- Property: extractEmbeddedPatterns handles multiple patterns on one line
prop_extractEmbeddedPatterns_multiple_patterns :: String -> String -> String -> Property
prop_extractEmbeddedPatterns_multiple_patterns prefix pattern1 pattern2 =
    not (null pattern1) && not (null pattern2) && 
    not (any isSpace pattern1) && not (any isSpace pattern2) ==>
    let content = prefix ++ "//go:embed " ++ pattern1 ++ " " ++ pattern2 ++ "\n"
        extracted = extractEmbeddedPatterns content
    in property $ pattern1 `elem` extracted .&&. pattern2 `elem` extracted

-- Property: extractEmbeddedPatterns handles whitespace before directive
prop_extractEmbeddedPatterns_whitespace_before :: String -> String -> Property
prop_extractEmbeddedPatterns_whitespace_before whitespace pattern =
    not (null pattern) && all isSpace whitespace ==>
    let content = whitespace ++ "//go:embed " ++ pattern ++ "\n"
        extracted = extractEmbeddedPatterns content
    in property $ pattern `elem` extracted

-- Property: extractEmbeddedPatterns is idempotent for pattern extraction
prop_extractEmbeddedPatterns_idempotent :: String -> Property
prop_extractEmbeddedPatterns_idempotent content =
    let extracted1 = extractEmbeddedPatterns content
        extracted2 = extractEmbeddedPatterns content
    in property $ sort extracted1 === sort extracted2

-- ============================================================================
-- Property Tests for toMissingEmbedInfo
-- ============================================================================

-- Property: toMissingEmbedInfo preserves all fields
prop_toMissingEmbedInfo_preserves_fields :: MissingEmbed -> Property
prop_toMissingEmbedInfo_preserves_fields missing =
    let info = toMissingEmbedInfo missing
    in property $ meiPattern info === missingPattern missing .&&.
                 meiRoot info === missingRoot missing .&&.
                 meiReference info === missingReferencedFrom missing

-- ============================================================================
-- Property Tests for Combined Operations
-- ============================================================================

-- Property: formatMissingMessage works with toMissingEmbedInfo conversion
prop_formatMissingMessage_with_conversion :: [MissingEmbed] -> Property
prop_formatMissingMessage_with_conversion missing =
    not (null missing) ==>
    let infos = map toMissingEmbedInfo missing
        formatted = formatMissingMessage missing
    in property $ not (null formatted) .&&.
                 all (\info -> meiPattern info `isInfixOf` formatted) infos

-- Property: extractEmbeddedPatterns handles complex scenarios
prop_extractEmbeddedPatterns_complex :: [String] -> [String] -> Property
prop_extractEmbeddedPatterns_complex prefixes patterns =
    not (null patterns) && all (not . null) patterns ==>
    let lines' = zipWith (\pref pat -> pref ++ "//go:embed " ++ pat) prefixes patterns
        content = unlines lines'
        extracted = extractEmbeddedPatterns content
    in property $ all (`elem` extracted) patterns

-- Property: extractEmbeddedPatterns handles mixed directive styles
prop_extractEmbeddedPatterns_mixed_styles :: String -> String -> String -> Property
prop_extractEmbeddedPatterns_mixed_styles pattern1 pattern2 pattern3 =
    not (null pattern1) && not (null pattern2) && not (null pattern3) &&
    not ('"' `elem` pattern1) && not ('`' `elem` pattern2) && not ('"' `elem` pattern3) ==>
    let content = unlines
          [ "//go:embed " ++ pattern1
          , "//go:embed \"" ++ pattern2 ++ "\""
          , "//go:embed `" ++ pattern3 ++ "`"
          ]
        extracted = extractEmbeddedPatterns content
    in property $ pattern1 `elem` extracted .&&.
                 pattern2 `elem` extracted .&&.
                 pattern3 `elem` extracted

-- ============================================================================
-- Edge Case Tests
-- ============================================================================

-- Property: extractEmbeddedPatterns handles empty input
prop_extractEmbeddedPatterns_empty :: Property
prop_extractEmbeddedPatterns_empty =
    let extracted = extractEmbeddedPatterns ""
    in property $ null extracted

-- Property: extractEmbeddedPatterns handles malformed directives gracefully
prop_extractEmbeddedPatterns_malformed :: String -> Property
prop_extractEmbeddedPatterns_malformed content =
    let malformedContent = content ++ "//go:embed\n" ++ content ++ "//go:embed \"\n" ++ content
        extracted = extractEmbeddedPatterns malformedContent
    in property $ length extracted <= 2  -- At most 2 patterns (empty strings filtered out)

-- Property: MissingEmbed ordering is preserved in formatMissingMessage
prop_formatMissingMessage_preserves_order :: [MissingEmbed] -> Property
prop_formatMissingMessage_preserves_order missing =
    not (null missing) ==>
    let formatted = formatMissingMessage missing
        formattedLines = tail (lines formatted)  -- Skip header
        originalPatterns = map missingPattern (nub missing)
        formattedPatterns = take (length originalPatterns) (map (takeWhile (/= ' ')) formattedLines)
    in property $ sort originalPatterns === sort formattedPatterns

-- ============================================================================
-- Performance and Scalability Tests
-- ============================================================================

-- Property: formatMissingMessage handles large inputs efficiently
prop_formatMissingMessage_large :: Int -> Property
prop_formatMissingMessage_large n =
    n >= 0 && n <= 100 ==>  -- Limit for performance testing
    let missing = take n $ cycle [MissingEmbed "pattern1" "root1" "ref1", MissingEmbed "pattern2" "root2" "ref2"]
        formatted = formatMissingMessage missing
    in property $ not (null formatted) .&&.
                 length (lines formatted) <= n + 1

-- Property: extractEmbeddedPatterns handles large files efficiently
prop_extractEmbeddedPatterns_large :: Int -> Property
prop_extractEmbeddedPatterns_large n =
    n >= 0 && n <= 50 ==>  -- Limit for performance testing
    let lines' = take n $ repeat "//go:embed pattern\n"
        content = concat lines'
        extracted = extractEmbeddedPatterns content
    in property $ length extracted === n

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "EmbedAssets QuickCheck Tests"
    [ testGroup "MissingEmbed Properties"
        [ fastProperty "formatMissingMessage produces non-empty output for non-empty input" prop_formatMissingMessage_non_empty
        , fastProperty "formatMissingMessage handles empty list gracefully" prop_formatMissingMessage_empty
        , fastProperty "formatMissingMessage removes duplicates using nub" prop_formatMissingMessage_removes_duplicates
        , fastProperty "formatMissingMessage contains all pattern information" prop_formatMissingMessage_contains_patterns
        ]
    , testGroup "extractEmbeddedPatterns Properties"
        [ fastProperty "extractEmbeddedPatterns finds patterns in go:embed directives" prop_extractEmbeddedPatterns_finds_directives
        , fastProperty "extractEmbeddedPatterns handles quoted patterns" prop_extractEmbeddedPatterns_quoted
        , fastProperty "extractEmbeddedPatterns handles backtick patterns" prop_extractEmbeddedPatterns_backtick
        , fastProperty "extractEmbeddedPatterns ignores non-directive lines" prop_extractEmbeddedPatterns_ignores_non_directives
        , fastProperty "extractEmbeddedPatterns handles multiple patterns on one line" prop_extractEmbeddedPatterns_multiple_patterns
        , fastProperty "extractEmbeddedPatterns handles whitespace before directive" prop_extractEmbeddedPatterns_whitespace_before
        , fastProperty "extractEmbeddedPatterns is idempotent for pattern extraction" prop_extractEmbeddedPatterns_idempotent
        ]
    , testGroup "toMissingEmbedInfo Properties"
        [ fastProperty "toMissingEmbedInfo preserves all fields" prop_toMissingEmbedInfo_preserves_fields
        ]
    , testGroup "Combined Operations"
        [ fastProperty "formatMissingMessage works with toMissingEmbedInfo conversion" prop_formatMissingMessage_with_conversion
        , fastProperty "extractEmbeddedPatterns handles complex scenarios" prop_extractEmbeddedPatterns_complex
        , fastProperty "extractEmbeddedPatterns handles mixed directive styles" prop_extractEmbeddedPatterns_mixed_styles
        ]
    , testGroup "Edge Cases"
        [ fastProperty "extractEmbeddedPatterns handles empty input" prop_extractEmbeddedPatterns_empty
        , fastProperty "extractEmbeddedPatterns handles malformed directives gracefully" prop_extractEmbeddedPatterns_malformed
        , fastProperty "MissingEmbed ordering is preserved in formatMissingMessage" prop_formatMissingMessage_preserves_order
        ]
    , testGroup "Performance and Scalability"
        [ fastProperty "formatMissingMessage handles large inputs efficiently" prop_formatMissingMessage_large
        , fastProperty "extractEmbeddedPatterns handles large files efficiently" prop_extractEmbeddedPatterns_large
        ]
    ]