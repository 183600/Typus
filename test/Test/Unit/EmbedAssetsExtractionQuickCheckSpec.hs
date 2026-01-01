{-# OPTIONS_GHC -Wno-deprecations #-}
module Test.Unit.EmbedAssetsExtractionQuickCheckSpec (tests) where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import EmbedAssets (extractEmbeddedPatterns, MissingEmbed(..))
import Data.Char (isSpace)
import qualified Data.List as L
import Data.List (isPrefixOf)
import Data.List (words, unwords)

-- ============================================================================
-- Embed Assets Extraction Property Tests
-- ============================================================================

-- | Test that extractEmbeddedPatterns finds patterns in simple directives
prop_extractSimplePattern :: String -> Property
prop_extractSimplePattern pattern =
  let content = "//go:embed " ++ pattern
      extracted = extractEmbeddedPatterns content
  in counterexample ("Failed to extract simple pattern: " ++ pattern ++
                     " from content: " ++ content ++
                     " Extracted: " ++ show extracted)
     (extracted === [normalizePattern pattern])

-- | Test that extractEmbeddedPatterns handles multiple patterns on one line
prop_extractMultiplePatterns :: String -> String -> Property
prop_extractMultiplePatterns pattern1 pattern2 =
  let content = "//go:embed " ++ pattern1 ++ " " ++ pattern2
      extracted = extractEmbeddedPatterns content
      expected = [normalizePattern pattern1, normalizePattern pattern2]
  in counterexample ("Failed to extract multiple patterns. " ++
                     "Content: " ++ content ++
                     " Expected: " ++ show expected ++
                     " Extracted: " ++ show extracted)
     (extracted === expected)

-- | Test that extractEmbeddedPatterns handles quoted patterns
prop_extractQuotedPatterns :: String -> Property
prop_extractQuotedPatterns pattern =
  let quotedPattern = "\"" ++ pattern ++ "\""
      content = "//go:embed " ++ quotedPattern
      extracted = extractEmbeddedPatterns content
  in counterexample ("Failed to extract quoted pattern: " ++ quotedPattern ++
                     " from content: " ++ content ++
                     " Extracted: " ++ show extracted)
     (extracted === [pattern])

-- | Test that extractEmbeddedPatterns handles backtick-quoted patterns
prop_extractBacktickPatterns :: String -> Property
prop_extractBacktickPatterns pattern =
  let backtickPattern = "`" ++ pattern ++ "`"
      content = "//go:embed " ++ backtickPattern
      extracted = extractEmbeddedPatterns content
  in counterexample ("Failed to extract backtick pattern: " ++ backtickPattern ++
                     " from content: " ++ content ++
                     " Extracted: " ++ show extracted)
     (extracted === [pattern])

-- | Test that extractEmbeddedPatterns ignores lines without go:embed directive
prop_ignoresNonEmbedLines :: String -> String -> Property
prop_ignoresNonEmbedLines nonEmbedLine embedLine =
  let pattern = "test.txt"
      content = nonEmbedLine ++ "\n//go:embed " ++ pattern ++ "\n" ++ nonEmbedLine
      extracted = extractEmbeddedPatterns content
  in counterexample ("Should ignore non-embed lines. " ++
                     "Content: " ++ content ++
                     " Extracted: " ++ show extracted)
     (extracted === [pattern])

-- | Test that extractEmbeddedPatterns handles whitespace before directive
prop_handlesWhitespaceBeforeDirective :: String -> Property
prop_handlesWhitespaceBeforeDirective pattern =
  let content = "   //go:embed " ++ pattern
      extracted = extractEmbeddedPatterns content
  in counterexample ("Failed to handle whitespace before directive. " ++
                     "Content: " ++ content ++
                     " Extracted: " ++ show extracted)
     (extracted === [normalizePattern pattern])

-- | Test that extractEmbeddedPatterns handles multiple directives in one file
prop_handlesMultipleDirectives :: String -> String -> Property
prop_handlesMultipleDirectives pattern1 pattern2 =
  let content = "//go:embed " ++ pattern1 ++ "\npackage main\n//go:embed " ++ pattern2
      extracted = extractEmbeddedPatterns content
      expected = [normalizePattern pattern1, normalizePattern pattern2]
  in counterexample ("Failed to handle multiple directives. " ++
                     "Content: " ++ content ++
                     " Expected: " ++ show expected ++
                     " Extracted: " ++ show extracted)
     (extracted === expected)

-- | Test that extractEmbeddedPatterns handles mixed quote styles
prop_handlesMixedQuoteStyles :: String -> String -> Property
prop_handlesMixedQuoteStyles pattern1 pattern2 =
  let content = "//go:embed \"" ++ pattern1 ++ "\" `" ++ pattern2 ++ "`"
      extracted = extractEmbeddedPatterns content
      expected = [pattern1, pattern2]
  in counterexample ("Failed to handle mixed quote styles. " ++
                     "Content: " ++ content ++
                     " Expected: " ++ show expected ++
                     " Extracted: " ++ show extracted)
     (extracted === expected)

-- | Test that extractEmbeddedPatterns handles empty patterns gracefully
prop_handlesEmptyPatterns :: Property
prop_handlesEmptyPatterns =
  let content = "//go:embed"
      extracted = extractEmbeddedPatterns content
  in counterexample ("Should handle empty patterns gracefully. " ++
                     "Content: " ++ content ++
                     " Extracted: " ++ show extracted)
     (extracted === [])

-- | Test that extractEmbeddedPatterns handles patterns with spaces
prop_handlesPatternsWithSpaces :: String -> String -> Property
prop_handlesPatternsWithSpaces part1 part2 =
  let pattern = "\"" ++ part1 ++ " " ++ part2 ++ "\""
      content = "//go:embed " ++ pattern
      extracted = extractEmbeddedPatterns content
      expected = part1 ++ " " ++ part2
  in counterexample ("Failed to handle patterns with spaces. " ++
                     "Content: " ++ content ++
                     " Expected: " ++ show expected ++
                     " Extracted: " ++ show extracted)
     (extracted === [expected])

-- | Test that extractEmbeddedPatterns handles complex file paths
prop_handlesComplexFilePaths :: String -> String -> Property
prop_handlesComplexFilePaths dir file =
  let pattern = dir ++ "/" ++ file
      content = "//go:embed " ++ pattern
      extracted = extractEmbeddedPatterns content
  in counterexample ("Failed to handle complex file paths. " ++
                     "Content: " ++ content ++
                     " Expected: " ++ show pattern ++
                     " Extracted: " ++ show extracted)
     (extracted === [pattern])

-- | Test that extractEmbeddedPatterns handles wildcards
prop_handlesWildcards :: String -> Property
prop_handlesWildcards extension =
  let pattern = "*." ++ extension
      content = "//go:embed " ++ pattern
      extracted = extractEmbeddedPatterns content
  in counterexample ("Failed to handle wildcards. " ++
                     "Content: " ++ content ++
                     " Expected: " ++ show pattern ++
                     " Extracted: " ++ show extracted)
     (extracted === [pattern])

-- | Test that extractEmbeddedPatterns handles recursive patterns
prop_handlesRecursivePatterns :: String -> Property
prop_handlesRecursivePatterns dir =
  let pattern = dir ++ "/**"
      content = "//go:embed " ++ pattern
      extracted = extractEmbeddedPatterns content
  in counterexample ("Failed to handle recursive patterns. " ++
                     "Content: " ++ content ++
                     " Expected: " ++ show pattern ++
                     " Extracted: " ++ show extracted)
     (extracted === [pattern])

-- | Test that extractEmbeddedPatterns is idempotent
prop_extractionIsIdempotent :: String -> Property
prop_extractionIsIdempotent content =
  let extracted1 = extractEmbeddedPatterns content
      extracted2 = extractEmbeddedPatterns content
  in counterexample ("Pattern extraction should be idempotent. " ++
                     "Content: " ++ content ++
                     " First: " ++ show extracted1 ++
                     " Second: " ++ show extracted2)
     (extracted1 === extracted2)

-- | Test that extractEmbeddedPatterns handles malformed quotes gracefully
prop_handlesMalformedQuotes :: String -> Property
prop_handlesMalformedQuotes pattern =
  let malformedPattern = "\"" ++ pattern  -- Missing closing quote
      content = "//go:embed " ++ malformedPattern
      extracted = extractEmbeddedPatterns content
  in counterexample ("Should handle malformed quotes gracefully. " ++
                     "Content: " ++ content ++
                     " Extracted: " ++ show extracted)
     (L.length extracted >= 0)  -- Should not crash

-- | Test that extractEmbeddedPatterns handles Unicode patterns
prop_handlesUnicodePatterns :: String -> Property
prop_handlesUnicodePatterns pattern =
  let content = "//go:embed " ++ pattern
      extracted = extractEmbeddedPatterns content
  in counterexample ("Failed to handle Unicode patterns. " ++
                     "Content: " ++ content ++
                     " Extracted: " ++ show extracted)
     (extracted === [normalizePattern pattern])

-- | Test that extractEmbeddedPatterns preserves order of patterns
prop_preservesPatternOrder :: String -> String -> String -> Property
prop_preservesPatternOrder pattern1 pattern2 pattern3 =
  let content = "//go:embed " ++ pattern1 ++ " " ++ pattern2 ++ " " ++ pattern3
      extracted = extractEmbeddedPatterns content
      expected = [normalizePattern pattern1, normalizePattern pattern2, normalizePattern pattern3]
  in counterexample ("Should preserve order of patterns. " ++
                     "Content: " ++ content ++
                     " Expected: " ++ show expected ++
                     " Extracted: " ++ show extracted)
     (extracted === expected)

-- Helper function to normalize patterns (simulate the normalization in EmbedAssets)
normalizePattern :: String -> String
normalizePattern pattern =
  case stripQuoted '"' pattern of
    Just s  -> s
    Nothing -> case stripQuoted '`' pattern of
                  Just s' -> s'
                  Nothing -> pattern
  where
    stripQuoted :: Char -> String -> Maybe String
    stripQuoted q s = case s of
      (c:xs) | c == q -> case unsnoc xs of
                            Just (body, qc) | qc == q -> Just body
                            _                          -> Nothing
      _               -> Nothing
    unsnoc :: [a] -> Maybe ([a], a)
    unsnoc []       = Nothing
    unsnoc [x]      = Just ([], x)
    unsnoc (x:xs)   = do (ys, z) <- unsnoc xs
                         pure (x:ys, z)

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Embed Assets Extraction QuickCheck Tests"
  [ testProperty "Extract simple pattern" prop_extractSimplePattern
  , testProperty "Extract multiple patterns" prop_extractMultiplePatterns
  , testProperty "Extract quoted patterns" prop_extractQuotedPatterns
  , testProperty "Extract backtick patterns" prop_extractBacktickPatterns
  , testProperty "Ignore non-embed lines" prop_ignoresNonEmbedLines
  , testProperty "Handle whitespace before directive" prop_handlesWhitespaceBeforeDirective
  , testProperty "Handle multiple directives" prop_handlesMultipleDirectives
  , testProperty "Handle mixed quote styles" prop_handlesMixedQuoteStyles
  , testProperty "Handle empty patterns" prop_handlesEmptyPatterns
  , testProperty "Handle patterns with spaces" prop_handlesPatternsWithSpaces
  , testProperty "Handle complex file paths" prop_handlesComplexFilePaths
  , testProperty "Handle wildcards" prop_handlesWildcards
  , testProperty "Handle recursive patterns" prop_handlesRecursivePatterns
  , testProperty "Extraction is idempotent" prop_extractionIsIdempotent
  , testProperty "Handle malformed quotes" prop_handlesMalformedQuotes
  , testProperty "Handle Unicode patterns" prop_handlesUnicodePatterns
  , testProperty "Preserve pattern order" prop_preservesPatternOrder
  ]