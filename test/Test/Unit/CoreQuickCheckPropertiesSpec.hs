{-# LANGUAGE OverloadedStrings #-}
module Test.Unit.CoreQuickCheckPropertiesSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (==>), property, suchThat)

import Utils (trim, splitBy, splitByCollapsed)
import SourceLocation (SourcePos(..), advancePos, advancePosBy, isValidSpan, spanBetween)

-- | QuickCheck property tests for core functionality
tests :: TestTree
tests = testGroup "Core QuickCheck Properties"
  [ testUtilsProperties
  , testSourceLocationProperties
  ]

-- | Property tests for Utils module
testUtilsProperties :: TestTree
testUtilsProperties = testGroup "Utils Properties"
  [ testProperty "trim idempotent" propTrimIdempotent
  , testProperty "splitBy L.length preservation" propSplitByLengthPreservation
  , testProperty "splitByCollapsed removes empty" propSplitByCollapsedRemovesEmpty
  , testProperty "trim split consistency" propTrimSplitConsistency
  ]

-- | Property tests for SourceLocation module  
testSourceLocationProperties :: TestTree
testSourceLocationProperties = testGroup "SourceLocation Properties"
  [ testProperty "position advancement line consistency" propPositionAdvancementLineConsistency
  , testProperty "span validity ordering" propSpanValidityOrdering
  , testProperty "advancePosBy consistency" propAdvancePosByConsistency
  ]

-- ============================================================================
-- Utils Properties
-- ============================================================================

-- | trim should be idempotent: trim(trim(x)) == trim(x)
propTrimIdempotent :: String -> Property
propTrimIdempotent str = property (trim (trim str) == trim str)

-- | splitBy should preserve total character count when rejoined
propSplitByLengthPreservation :: Char -> String -> Property
propSplitByLengthPreservation delim str =
  let parts = splitBy delim str
      rejoined = concatMap (++ [delim]) (init parts) ++ last parts
  in property (L.length rejoined == L.length str + L.length parts - 1)

-- | splitByCollapsed should never return empty strings
propSplitByCollapsedRemovesEmpty :: Char -> String -> Property
propSplitByCollapsedRemovesEmpty delim str =
  let parts = splitByCollapsed delim str
  in property (L.all (not . null) parts)

-- | trim L.and split should be consistent for whitespace-separated values
propTrimSplitConsistency :: String -> Property
propTrimSplitConsistency str =
  let trimmed = trim str
      parts = splitBy ' ' trimmed
      nonEmptyParts = L.filter (not . null) parts
  in property (L.all (not . null . trim) nonEmptyParts)

-- ============================================================================
-- SourceLocation Properties
-- ============================================================================

-- | Position advancement should maintain line consistency
propPositionAdvancementLineConsistency :: SourcePos -> Char -> Property
propPositionAdvancementLineConsistency pos char =
  let newPos = advancePos char pos
      oldLine = posLine pos
      newLine = posLine newPos
  in property (if char == '\n'
              then newLine == oldLine + 1
              else newLine == oldLine)

propSpanValidityOrdering :: SourcePos -> SourcePos -> Property
propSpanValidityOrdering start end =
  let span = spanBetween start end
      valid = isValidSpan span
      startLine = posLine start
      endLine = posLine end
      startCol = posColumn start
      endCol = posColumn end
      properlyOrdered = startLine < endLine || 
                        (startLine == endLine && startCol < endCol)
  in property (valid == properlyOrdered)

-- | advancePosBy should be consistent with repeated advancePos
propAdvancePosByConsistency :: SourcePos -> String -> Property
propAdvancePosByConsistency pos str =
  let posByString = advancePosBy str pos
      posByChars = foldl (flip advancePos) pos str
  in property (posByString == posByChars)

-- ============================================================================
-- Arbitrary Instances
-- ============================================================================

instance Arbitrary SourcePos where
  arbitrary = SourcePos <$> positive <*> positive <*> pure 0
    where
      positive = getPositive <$> arbitrary

newtype Positive = Positive Int deriving (Show, Eq)

instance Arbitrary Positive where
  arbitrary = Positive <$> arbitrary `suchThat` (> 0)

getPositive :: Positive -> Int
getPositive (Positive n) = n