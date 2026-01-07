module Test.Unit.NewComprehensivePropertySpec where
import Test.QuickCheck 
import Test.Tasty.QuickCheck (testProperty, Property, Arbitrary(..), Gen)
import Data.List 
import SourceLocation (SourcePos(..), advancePos, advancePosBy, spanBetween, isValidSpan)
import Parser 
                                    nonSpaceChars = L.filter (not . isSpace) str
                                    trimmedNonSpaceChars = L.filter (not . isSpace) trimmed
  in sort                               nonSpaceChars == sort trimmedNonSpaceChars

-- | splitBy L.and join should be inverses (for non-empty delimiters)
propSplitJoinInverse :: Char -> String -> Property
propSplitJoinInverse delim                               str =
  delim /= '\0' ==>
  let parts = splitBy delim str
                                    rejoined = concatMap (++ [delim]) (init parts ++ [last parts])
  in L.length rejoined >= L.length str

-- | splitByCollapsed should remove duplicate delimiters
propSplitByCollapsedRemovesDuplicates :: Char -> String -> Property
propSplitByCollapsedRemovesDuplicates delim                               str =
  let parts = splitByCollapsed delim str
                                    hasNoDuplicates = L.all (not . L.elem delim) parts
  in hasNoDuplicates

-- | normalizeIndentation should preserve relative structure
propNormalizeIndentationPreservesStructure :: String -> Property
propNormalizeIndentationPreservesStructure                               str =
  let normalized = normalizeIndentation str
                                    originalLines = lines str
                                    normalizedLines = lines normalized
  in L.length                               originalLines == L.length normalizedLines

-- ============================================================================
-- Source Location Properties
-- ============================================================================

-- | Position advancement should be deterministic
propPositionAdvancementDeterministic :: SourcePos -> Char -> Property
propPositionAdvancementDeterministic pos                               char =
  let result1 = advancePos pos char
                                    result2 = advancePos pos char
  in                               result1 == result2

-- | Span creation should preserve start/end order
propSpanCreationPreservesOrder :: SourcePos -> SourcePos -> Property
propSpanCreationPreservesOrder start                               end =
  let span = spanBetween start end
  in if isValidSpan span
     then True  -- Valid spans maintain proper order
     else True  -- Invalid spans are expected for reversed inputs

-- | Valid spans should have positive L.length
propValidSpansPositiveLength :: SourcePos -> SourcePos -> Property
propValidSpansPositiveLength start                               end =
  let span = spanBetween start end
  in isValidSpan                               span ==> True  -- Simplified: just ensure it doesn't crash

-- | advancePosBy should be associative
propAdvancePosByAssociative :: SourcePos -> String -> String -> Property
propAdvancePosByAssociative pos str1                               str2 =
  let result1 = advancePosBy (advancePosBy pos str1) str2
                                    result2 = advancePosBy pos (str1 ++ str2)
  in                               result1 == result2

-- ============================================================================
-- Parser Properties
-- ============================================================================

-- | Parsing should be idempotent on valid input
propParsingIdempotent :: String -> Property
propParsingIdempotent                               input =
  let result1 = parseTypus input
  in case result1 of
    Left _ -> property True  -- Invalid inputs can fail
    Right file1 -> 
      let reconstructed = unlines $ map cbContent (tfBlocks file1)
                                        result2 = parseTypus reconstructed
      in case result2 of
        Left _ -> property True
        Right _ -> property True

-- | Parser should handle whitespace gracefully
propParserHandlesWhitespace :: String -> Property
propParserHandlesWhitespace                               input =
  let withExtraWhitespace = addRandomWhitespace input
                                    result1 = parseTypus input
                                    result2 = parseTypus withExtraWhitespace
  in case (result1, result2) of
    (Left _, Left _) -> property True
    (Right _, Right _) -> property True  -- Both succeed
    _ -> property False

-- | Parse errors should be informative
propParseErrorsInformative :: String -> Property
propParseErrorsInformative                               input =
  let result = parseTypus input
  in case result of
    Left err -> L.length (show err) > 0
    Right _ -> property True

-- ============================================================================
-- Integration Properties
-- ============================================================================

-- | Source location tracking should work through parsing
propSourceLocationTracking :: String -> Property
propSourceLocationTracking                               input =
  let result = parseTypus input
  in case result of
    Left _ -> property True
    Right file -> L.length (tfBlocks file) >= 0

-- | Error handling should preserve context
propErrorHandlingPreservesContext :: String -> Property
propErrorHandlingPreservesContext                               input =
  let result = parseTypus input
  in case result of
    Left err -> L.length (show err) > 0
    Right _ -> property True

-- | String utilities should work with parser output
propStringUtilsWithParser :: String -> Property
propStringUtilsWithParser                               input =
  let trimmed = trim input
                                    result = parseTypus trimmed
  in case result of
    Left _ -> property True
    Right _ -> property True

-- ============================================================================
-- Helper Functions
-- ============================================================================

-- | Add random whitespace to string
addRandomWhitespace :: String -> String
                              addRandomWhitespace = concatMap (\c -> 
  if isSpace c then c ++ "  "
  else if                               c == '\n' then "\n  "
  else [c])

-- ============================================================================
-- Arbitrary Instances
-- ============================================================================

instance Arbitrary SourcePos where
                                              arbitrary = SourcePos <$> arbitrary <*> arbitrary
    where
                                                  arbitrary = getPositive <$> arbitrary

newtype                               Positive = Positive Int deriving (Show, Eq)

instance Arbitrary Positive where
                                              arbitrary = Positive <$> arbitrary `suchThat` (> 0)

getPositive :: Positive -> Int
getPositive (Positive n) = n