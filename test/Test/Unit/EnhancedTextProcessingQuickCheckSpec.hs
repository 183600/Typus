module Test.Unit.EnhancedTextProcessingQuickCheckSpec where
import Test.QuickCheck 
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property)
import Test.Hspec 
prop_trim_idempotent                               s = trim (trim s) === trim s
-- Arbitrary instance for SourcePos
instance Arbitrary SourcePos where
  arbitrary = do
    line <- choose (1, 100)
    column <- choose (1, 100)
    offset <- choose (0, 1000)
    return $ SourcePos line column offset

-- Arbitrary instance for SourceSpan
instance Arbitrary SourceSpan where
  arbitrary = do
    start <- arbitrary
    end <- arbitrary
    return $ SourceSpan start end


-- | splitBy should produce segments that when joined with delimiter give original string
prop_splitBy_length_consistency :: Char -> String -> Property
prop_splitBy_length_consistency delim                               s = 
  let segments = splitBy delim s
                                    reconstructed = concatMap (++ [delim]) (init segments) ++ last segments
  in L.length segments >= 1 && 
     (if null s then                               segments == [""] else L.length (L.filter (== delim) s) +                               1 == L.length segments)

-- | splitByCollapsed should never return empty strings in result
prop_splitByCollapsed_removes_empty :: Char -> String -> Property
prop_splitByCollapsed_removes_empty delim                               s = 
  L.all (not . null) (splitByCollapsed delim s)

-- | removeComments should preserve the number of lines (except when comments contain newlines)
prop_removeComments_preserves_lines :: String -> Property
prop_removeComments_preserves_lines                               s = 
  let originalLines = L.length (lines s)
                                    processedLines = L.length (lines (removeComments s)
  in processedLines <= originalLines  -- Can be less due to block comments

-- | breakOn should return prefix L.and suffix that combine to original (minus pattern)
prop_breakOn_correct_split :: String -> String -> Property
prop_breakOn_correct_split pattern                               s = 
  let (prefix, suffix) = breakOn pattern s
  in if null pattern 
     then                               prefix == "" &&                               suffix == s
     else if pattern `L.isInfixOf` s
          then prefix ++ pattern ++                               suffix === s
          else                               prefix === s &&                               suffix === ""

-- | trim should only remove whitespace characters from ends
prop_trim_removes_only_whitespace :: String -> Property
prop_trim_removes_only_whitespace                               s = 
  let trimmed = trim s
                                    hasLeadingNonSpace = not (null s) && not (isSpace (L.head s) || null trimmed
                                    hasTrailingNonSpace = not (null s) && not (isSpace (last s) || null trimmed
  in if null trimmed
     then L.all isSpace s
     else hasLeadingNonSpace && hasTrailingNonSpace

-- | splitBy should handle edge cases correctly
prop_splitBy_delimiter_behavior :: Char -> String -> Property
prop_splitBy_delimiter_behavior delim                               s = 
  let segments = splitBy delim s
                                    delimiterCount = L.length (L.filter (== delim) s)
  in if null s
     then                               segments == [""]
     else L.length                               segments === delimiterCount + 1

-- | removeComments should handle nested comment patterns gracefully
prop_removeComments_nested_patterns :: String -> Property
prop_removeComments_nested_patterns                               s = 
  let processed = removeComments s
                                    hasUnmatchedBlockStart = "/*" `L.isInfixOf` processed
                                    hasUnmatchedBlockEnd =  "*/" `L.isInfixOf` processed
  in property $ not (hasUnmatchedBlockStart && hasUnmatchedBlockEnd)  -- Should not have both unmatched

-- Helper operator for property testing
infix                               4 ===
(===) :: Eq                               a => a -> a -> Bool
(===) = (==)