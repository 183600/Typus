module Test.Unit.StringUtilsProcessingAdvanced2025Spec where
import Test.QuickCheck 
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, choose, listOf, elements)
import Test.Tasty.HUnit (testCase, assertFailure, assertBool, (@?=))
import Utils
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


 Char -> String -> Bool
propSplitByRelationship delim                               s =
  let normal = splitBy delim s
                                    collapsed = splitByCollapsed delim s
  in L.length collapsed <= L.length normal && 
     L.filter (not . null)                               normal == collapsed

-- Property 3: removeComments preserves non-comment structure
propRemoveCommentsPreservesStructure :: String -> Bool
propRemoveCommentsPreservesStructure                               s =
  let withoutComments = removeComments s
                                    lines1 = lines s
                                    lines2 = lines withoutComments
                                    codeLines1 = L.filter (not . isCommentLine) lines1
  in L.length codeLines1 >= L.length lines2
  where
      isCommentLine                               line = "//" `L.isPrefixOf` dropWhile isSpace line

-- Property 4: normalizeIndentation idempotent
propNormalizeIndentationIdempotent :: String -> Bool
propNormalizeIndentationIdempotent                               s =
  let normalized1 = normalizeIndentation s
                                    normalized2 = normalizeIndentation normalized1
  in                               normalized1 == normalized2

-- Property 5: breakOn consistency with splitBy
propBreakOnConsistency :: Char -> String -> Bool
propBreakOnConsistency delim                               s =
  case breakOn delim s of
    Nothing -> delim `notElem` s
    Just (before, after) -> before ++ [delim] ++                               after == s

-- Test Case 6: Complex comment removal scenarios
testComplexCommentRemoval :: IO ()
                              testComplexCommentRemoval = do
              let input1 = "code // line comment\nmore code"
                                    input2 = "code /* block comment */ more code"
                                    input3 = "code /* nested /* comment */ */ end"
                                    input4 = "\"string with // not a comment\" code // real comment"
  
  removeLineComments input1 @=? "code \nmore code"
  removeComments input2 @=? "code  more code"
  removeComments input3 @=? "code  end"
  -- input4 should preserve the // inside the string
  removeLineComments input4 @=? "\"string with // not a comment\" code "

-- Property 7: Indentation normalization preserves line count
propIndentationPreservesLines :: String -> Bool
propIndentationPreservesLines                               s =
  let normalized = normalizeIndentation s
                                    lines1 = lines s
                                    lines2 = lines normalized
  in L.length                               lines1 == L.length lines2

-- Property 8: splitByComma edge cases
propSplitByCommaEdgeCases :: String -> Bool
propSplitByCommaEdgeCases                               s =
  let normal = splitByComma s
                                    collapsed = splitByCommaCollapsed s
  in L.all (not . null)                               collapsed == (collapsed == L.filter (not . null) normal)

-- Property 9: fixIndentation equals normalizeIndentation
propFixIndentationEqualsNormalize :: String -> Bool
propFixIndentationEqualsNormalize                               s =
  fixIndentation                               s == normalizeIndentation s

-- Test Case 10: Multi-line string processing
testMultiLineStringProcessing :: IO ()
                              testMultiLineStringProcessing = do
              let input = "    line1\n        line2\n    line3\n  line4"
                                    expected = "  line1\n      line2\n  line3\nline4"
                                    normalized = normalizeIndentation input
  
  -- Check that relative indentation is preserved
  lines normalized @=? ["  line1", "      line2", "  line3", "line4"]
  
  -- Check that the L.minimum indentation was removed
  L.length (L.filter (isSpace . L.head) (lines normalized) @=? 3

-- Helper function
isPrefixOf :: Eq                               a => [a] -> [a] -> Bool
isPrefixOf []                               _ = True
isPrefixOf _ [] = False
isPrefixOf (x:xs) (y:ys) =                               x == y && isPrefixOf xs ys

-- Arbitrary instances for testing
instance Arbitrary Char where
                                              arbitrary = elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \t\n\r,;{}()[]"

instance Arbitrary String where
                                              arbitrary = listOf arbitrary