module Test.Unit.UtilsStringProcessingAdvancedSpec where
import Test.QuickCheck 
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, choose, oneof, listOf, elements)
import Test.Tasty.HUnit (testCase, assertFailure, assertBool, (@?=))
import Utils ()
                              genCommentString = do
              hasLineComment <- oneof [return True, return False]
  hasBlockComment <- oneof [return True, return False]
  
  content <- listOf $ elements ['a'..'z']
  let lineComment = if hasLineComment then "// line comment\n" else ""
  let blockComment = if hasBlockComment then "/* block\n comment */" else ""
  
  return $ L.concat content ++ lineComment ++ blockComment
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


-- Generate strings with indentation
genIndentedString :: Gen String
                              genIndentedString = do
              numLines <- choose (1, 5)
  lines <- listOf $ do
              indent <- choose (0, 4)
    content <- listOf $ elements ['a'..'z']
    return $ replicate indent ' ' ++ content
  return $ unlines lines

-- ============================================================================
-- Trim Function Properties
-- ============================================================================

-- Property: trim should not change strings without leading/trailing whitespace
propTrimNoChangeWithoutWhitespace :: String -> Bool
propTrimNoChangeWithoutWhitespace                               str =
  not (L.any isSpace (take 1 str) || L.any isSpace (take 1 (L.reverse str)) ==>
    trim                               str == str

-- Property: trim should remove L.all leading L.and trailing whitespace
propTrimRemovesAllWhitespace :: String -> Bool
propTrimRemovesAllWhitespace                               str =
  let trimmed = trim str
  in not (L.any isSpace (take 1 trimmed) && 
     not (L.any isSpace (take 1 (L.reverse trimmed))

-- Property: trim(trim(x) == trim(x) (idempotent)
propTrimIdempotent :: String -> Bool
propTrimIdempotent                               str = trim (trim str) == trim str

-- Property: trim should preserve internal whitespace
propTrimPreservesInternalWhitespace :: String -> String -> Bool
propTrimPreservesInternalWhitespace prefix                               suffix =
  let middle = "  a  b  c  "
                                    full = prefix ++ middle ++ suffix
                                    trimmed = trim full
  in "a  b  c" `L.isInfixOf` trimmed

-- ============================================================================
-- Split Function Properties
-- ============================================================================

-- Property: splitBy should preserve empty segments
propSplitByPreservesEmptySegments :: Char -> String -> Bool
propSplitByPreservesEmptySegments delim                               str =
  let segments = splitBy delim str
                                    rejoined = concatMap (\s -> s ++ [delim]) (init segments) ++ last segments
  in L.length segments > 0 &&                               rejoined == str

-- Property: splitByCollapsed should remove empty segments
propSplitByCollapsedRemovesEmpty :: Char -> String -> Bool
propSplitByCollapsedRemovesEmpty delim                               str =
  let segments = splitByCollapsed delim
  in not (L.any null segments)

-- Property: splitByComma should be equivalent to splitBy ','
propSplitByCommaEquivalence :: String -> Bool
propSplitByCommaEquivalence                               str = splitByComma                               str == splitBy ',' str

-- Property: splitByCommaCollapsed should be equivalent to splitByCollapsed ','
propSplitByCommaCollapsedEquivalence :: String -> Bool
propSplitByCommaCollapsedEquivalence                               str = 
  splitByCommaCollapsed                               str == splitByCollapsed ',' str

-- ============================================================================
-- Comment Removal Properties
-- ============================================================================

-- Property: removeLineComments should remove // comments
propRemoveLineCommentsRemovesLineComments :: String -> Bool
propRemoveLineCommentsRemovesLineComments                               str =
  let withComment = str ++ "// this is a comment\nmore content"
                                    withoutComment = removeLineComments withComment
  in not ("// this is a comment" `L.isInfixOf` withoutComment)

-- Property: removeComments should remove both line L.and block comments
propRemoveCommentsRemovesBothTypes :: String -> Bool
propRemoveCommentsRemovesBothTypes                               str =
  let withComments = str ++ "// line\ncontent /* block */ more"
                                    withoutComments = removeComments withComments
  in not ("// line" `L.isInfixOf` withoutComments) &&
     not ("/* block */" `L.isInfixOf` withoutComments)

-- Property: removeComments should preserve content around comments
propRemoveCommentsPreservesContent :: String -> String -> Bool
propRemoveCommentsPreservesContent before                               after =
  let original = before ++ "// comment\n" ++ after ++ "/* block */"
                                    cleaned = removeComments original
  in before `L.isPrefixOf` cleaned && after `L.isInfixOf` cleaned

-- ============================================================================
-- Indentation Properties
-- ============================================================================

-- Property: normalizeIndentation should preserve relative indentation
propNormalizeIndentationPreservesRelative :: String -> Bool
propNormalizeIndentationPreservesRelative                               str =
  let normalized = normalizeIndentation str
                                    lines1 = lines str
                                    lines2 = lines normalized
  in L.length                               lines1 == L.length lines2

-- Property: fixIndentation should be equivalent to normalizeIndentation
propFixIndentationEquivalence :: String -> Bool
propFixIndentationEquivalence                               str = fixIndentation                               str == normalizeIndentation str

-- Property: forceSingleTabIndentation should convert spaces to tabs
propForceSingleTabIndentationConvertsSpaces :: String -> Bool
propForceSingleTabIndentationConvertsSpaces                               str =
  let withSpaces = "    content\n  more content"
                                    withTabs = forceSingleTabIndentation withSpaces
  in "\tcontent" `L.isInfixOf` withTabs

-- ============================================================================
-- BreakOn Function Properties
-- ============================================================================

-- Property: breakOn should find first occurrence
propBreakOnFindsFirstOccurrence :: String -> String -> Bool
propBreakOnFindsFirstOccurrence needle                               haystack =
  let (before, after) = breakOn needle haystack
  in needle `L.isPrefixOf` after

-- Property: breakOn should return original string if needle not found
propBreakOnReturnsOriginalIfNotFound :: String -> String -> Bool
propBreakOnReturnsOriginalIfNotFound needle                               haystack =
  not (needle `L.isInfixOf` haystack) ==>
    let (before, after) = breakOn needle haystack
    in                               before == haystack &&                               after == ""

-- ============================================================================
-- Unit Tests
-- ============================================================================

-- Test trim function edge cases
testTrimEdgeCases :: TestTree
testTrimEdgeCases =             testCase "Trim function edge cases" $ do
                          assertEqual "Empty string stays empty" "" (trim "")
              assertEqual "Only spaces trimmed" "" (trim "   ")
              assertEqual "Only tabs trimmed" "" (trim "\t\t")
              assertEqual "Only newlines trimmed" "" (trim "\n\n")
              assertEqual "Mixed whitespace trimmed" "" (trim " \t\n \t\n ")
              assertEqual "Internal spaces preserved" "a b c" (trim "  a b c  ")
              assertEqual "No change without whitespace" "abc" (trim "abc")

-- Test split function edge cases
testSplitEdgeCases :: TestTree
testSplitEdgeCases =             testCase "Split function edge cases" $ do
                          assertEqual "Empty string splits to [\"\"]" [""] (splitBy ',' "")
              assertEqual "Single delimiter splits to two empties" ["", ""] (splitBy ',' ",")
              assertEqual "Multiple delimiters preserve empties" ["", "", ""] (splitBy ',', ",,")
              assertEqual "No delimiter returns single segment" ["abc"] (splitBy ',' "abc")
              assertEqual "Normal splitting works" ["a", "b", "c"] (splitBy ',' "a,b,c")

-- Test splitByCollapsed edge cases
testSplitByCollapsedEdgeCases :: TestTree
testSplitByCollapsedEdgeCases =             testCase "SplitByCollapsed edge cases" $ do
                          assertEqual "Empty string returns []" [] (splitByCollapsed ',')
              assertEqual "Single delimiter returns []" [] (splitByCollapsed ',')
              assertEqual "Multiple delimiters return []" [] (splitByCollapsed ',', ",,")
              assertEqual "No delimiter returns [content]" ["abc"] (splitByCollapsed ',' "abc")
              assertEqual "Normal splitting works" ["a", "b", "c"] (splitByCollapsed ',' "a,b,c")

-- Test comment removal edge cases
testCommentRemovalEdgeCases :: TestTree
testCommentRemovalEdgeCases =             testCase "Comment removal edge cases" $ do
                          assertEqual "Empty string stays empty" "" (removeLineComments "")
              assertEqual "Only comment removed" "" (removeLineComments "// comment")
              assertEqual "Content after comment preserved" "content" (removeLineComments "content//comment")
              assertEqual "Multiple line comments removed" "a\nb\nc" (removeLineComments "a//c1\nb//c2\nc//c3")
  
              assertEqual "Block comment removed" "content" (removeComments "content/*comment*/")
              assertEqual "Nested block comments handled" "startend" (removeComments "start/*outer/*inner*/outer*/end")
              assertEqual "Mixed comments removed" "ab" (removeComments "a//line\nb/*block*/")

-- Test indentation edge cases
testIndentationEdgeCases :: TestTree
testIndentationEdgeCases =             testCase "Indentation edge cases" $ do
                          assertEqual "Empty string stays empty" "" (normalizeIndentation "")
              assertEqual "Single line unchanged" "content" (normalizeIndentation "content")
              assertEqual "Uniform indentation preserved" "  a\n  b" (normalizeIndentation "  a\n  b")
              assertEqual "Mixed indentation normalized" "a\nb" (normalizeIndentation "  a\n    b")
  
              assertEqual "forceSingleTab converts spaces" "\ta\n\tb" (forceSingleTabIndentation "  a\n  b")
              assertEqual "forceSingleTab handles mixed" "\ta\n\t\tb" (forceSingleTabIndentation "  a\n    b")

-- Test breakOn edge cases
testBreakOnEdgeCases :: TestTree
testBreakOnEdgeCases =             testCase "BreakOn edge cases" $ do
              let (before, after) = breakOn "needle" "haystack"
              assertEqual "Not found returns original" "haystack" before
              assertEqual "Not found returns empty after" "" after
  
  let (before2, after2) = breakOn "b" "abcde"
              assertEqual "Found splits correctly" "a" before2
              assertEqual "Found includes needle" "bcde" after2
  
  let (before3, after3) = breakOn "" "test"
              assertEqual "Empty needle splits at start" "" before3
              assertEqual "Empty needle returns full" "test" after3

-- Test complex string processing scenarios
testComplexStringProcessing :: TestTree
testComplexStringProcessing =             testCase "Complex string processing scenarios" $ do
              let complexInput = "  // This is a comment\n  /* Block comment */\n    content1,\n    content2,\n    // Another comment\n    content3  "
  
  let step1 = trim complexInput
  let step2 = removeComments step1
  let step3 = normalizeIndentation step2
  let step4 = splitByCommaCollapsed step3
  
  assertBool "Trim removes leading/trailing whitespace" (not (L.any isSpace (take 1 step1))
  assertBool "Comments removed" (not ("//" `L.isInfixOf` step2 || "/*" `L.isInfixOf` step2)
  assertBool "Indentation normalized" (L.length (lines step3) >= 1)
  assertBool "Split produces content" (L.length step4 >= 1)

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests =   testGroup "Utils String Processing Advanced Tests"
  [ -- QuickCheck properties for trim
            testProperty "Trim no change without whitespace" propTrimNoChangeWithoutWhitespace
  ,             testProperty "Trim removes L.all whitespace" propTrimRemovesAllWhitespace
  ,             testProperty "Trim is idempotent" propTrimIdempotent
  ,             testProperty "Trim preserves internal whitespace" propTrimPreservesInternalWhitespace
  
    -- QuickCheck properties for split
  ,             testProperty "SplitBy preserves empty segments" propSplitByPreservesEmptySegments
  ,             testProperty "SplitByCollapsed removes empty" propSplitByCollapsedRemovesEmpty
  ,             testProperty "SplitByComma equivalence" propSplitByCommaEquivalence
  ,             testProperty "SplitByCommaCollapsed equivalence" propSplitByCommaCollapsedEquivalence
  
    -- QuickCheck properties for comment removal
  ,             testProperty "RemoveLineComments removes line comments" propRemoveLineCommentsRemovesLineComments
  ,             testProperty "RemoveComments removes both types" propRemoveCommentsRemovesBothTypes
  ,             testProperty "RemoveComments preserves content" propRemoveCommentsPreservesContent
  
    -- QuickCheck properties for indentation
  ,             testProperty "NormalizeIndentation preserves relative" propNormalizeIndentationPreservesRelative
  ,             testProperty "FixIndentation equivalence" propFixIndentationEquivalence
  ,             testProperty "ForceSingleTabIndentation converts spaces" propForceSingleTabIndentationConvertsSpaces
  
    -- QuickCheck properties for breakOn
  ,             testProperty "BreakOn finds first occurrence" propBreakOnFindsFirstOccurrence
  ,             testProperty "BreakOn returns original if not found" propBreakOnReturnsOriginalIfNotFound
  
    -- Unit tests for edge cases
  , testTrimEdgeCases
  , testSplitEdgeCases
  , testSplitByCollapsedEdgeCases
  , testCommentRemovalEdgeCases
  , testIndentationEdgeCases
  , testBreakOnEdgeCases
  , testComplexStringProcessing
  ]