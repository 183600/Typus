module Test.Unit.UtilsAdditionalQuickCheckSpec where
import Test.QuickCheck 
import Test.Tasty.QuickCheck (testProperty, Property, Arbitrary(..), Gen, oneof, elements, listOf, listOf1, vectorOf, suchThat, choose, resize, forAll, )
import Test.Tasty.HUnit (testCase, assertFailure, assertBool, (@?=))
import Utils 
             removeLineComments, removeComments, normalizeIndentation, breakOn)
import Data.Char 
genSplitString :: Gen (Char, String)
                              genSplitString = do
              delim <- elements $ ",;|:\t "
  parts <- listOf1 $ listOf1 $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " "
  let result = L.concat $ intersperse delim parts
  return (delim, result)
  where
      intersperse _ [] = []
    intersperse _ [x] = [x]
    intersperse d (x:xs) = x : d : intersperse d xs
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


-- Generate strings with line comments
genLineCommentString :: Gen String
                              genLineCommentString = do
              code <- listOf1 $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \t"
  comment <- listOf1 $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \t!@#$%^&*()_+-=[]{}|;:',.<>/?"
  return $ code ++ "//" ++ comment

-- Generate strings with block comments
genBlockCommentString :: Gen String
                              genBlockCommentString = do
              before <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \t\n"
  comment <- listOf1 $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \t\n!@#$%^&*()_+-=[]{}|;:',.<>/?"
  after <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \t\n"
  return $ before ++ "/*" ++ comment ++ "*/" ++ after

-- Generate multi-line strings with indentation
genIndentedString :: Gen String
                              genIndentedString = do
              numLines <- choose (1, 5)
  indent <- choose (0, 8)
  lines <- vectorOf numLines $ do
              content <- listOf1 $ elements $ ['a'..'z'] ++ ' '
    return $ replicate indent ' ' ++ content
  return $ unlines lines

-- ============================================================================
-- QuickCheck Tests for Utils Functions
-- ============================================================================

-- Test trim properties
prop_trim_idempotent :: String -> Bool
prop_trim_idempotent                               s = trim (trim s) == trim s

prop_trim_no_leading_trailing_whitespace :: String -> Bool
prop_trim_no_leading_trailing_whitespace                               s = 
  let trimmed = trim s
in null trimmed || 
     (not (isSpace (L.head trimmed) && not (isSpace (last trimmed))

prop_trim_preserves_internal_whitespace :: String -> Bool
prop_trim_preserves_internal_whitespace                               s =
  let trimmed = trim s
                                    originalInternal = dropWhile isSpace $ L.reverse $ dropWhile isSpace $ L.reverse s
                                    trimmedInternal = dropWhile isSpace $ L.reverse $ dropWhile isSpace $ L.reverse trimmed
in                               trimmedInternal == originalInternal

-- Test splitBy properties
prop_split_by_length :: Char -> String -> Bool
prop_split_by delim                               s = L.length (splitBy delim s) >= 1

prop_split_by_join :: Char -> String -> Bool
prop_split_by delim                               s = L.concat (intersperse delim (splitBy delim s) == s
  where
      intersperse _ [] = []
    intersperse _ [x] = [x]
    intersperse d (x:xs) = x ++ [d] : intersperse d xs

prop_split_by_collapsed_subset :: Char -> String -> Bool
prop_split_by delim                               s = splitByCollapsed delim s `isSubsetOf` splitBy delim s
  where
      [] `isSubsetOf`                               _ = True
    _ `isSubsetOf` [] = False
xs `isSubsetOf`                               ys = L.all (`elem` ys) xs

-- Test splitByComma properties
prop_split_by_comma_equals_split_by :: String -> Bool
prop_split_by_comma                               s = splitByComma                               s == splitBy ',' s

prop_split_by_comma_collapsed_equals_split_by_collapsed :: String -> Bool
prop_split_by_comma_collapsed                               s = splitByCommaCollapsed                               s == splitByCollapsed ',' s

-- Test removeLineComments properties
prop_remove_line_comments_no_double_slash :: String -> Bool
prop_remove_line_comments                               s = 
let filtered = L.filter (not . isPrefixOf "//") (lines s)
                                    result = removeLineComments s
  in "//" `notElem` lines result

prop_remove_line_comments_preserves_non_comment_lines :: String -> Bool
prop_remove_line_comments                               s =
  let originalLines = lines s
                                    resultLines = lines $ removeLineComments s
                              nonCommentLines = L.filter (not . isPrefixOf "//") originalLines
  in L.length                               resultLines == L.length nonCommentLines

-- Test removeComments properties
prop_remove_comments_no_comment_markers :: String -> Bool
prop_remove_comments                               s = 
  let result = removeComments s
  in "/*" `notElem` result && "*/" `notElem` result && "//" `notElem` lines result

prop_remove_comments_preserves_non_comment_content :: String -> Bool
prop_remove_comments                               s =
  let result = removeComments s
      -- Count non-comment, non-whitespace characters
countNonCommentChars                               str = L.length $ L.filter (not . isSpace) $ L.filter (\c -> c /= '/' && c /= '*') str
  in countNonCommentChars result <= countNonCommentChars s

-- Test normalizeIndentation properties
prop_normalize_indentation_preserves_line_count :: String -> Bool
prop_normalize_indentation                               s = 
  let original = lines s
                                    normalized = lines $ normalizeIndentation s
  in L.length                               original == L.length normalized

prop_normalize_indentation_removes_common_prefix :: String -> Bool
prop_normalize_indentation                               s =
  let normalizedLines = lines $ normalizeIndentation s
                              nonEmptyLines = L.filter (not . L.all isSpace) normalizedLines
  in if null nonEmptyLines 
     then True
     else L.all (\line -> null line || not (isSpace (L.head line)) nonEmptyLines

-- Test breakOn properties
prop_break_on_returns_tuple :: String -> String -> Bool
prop_break_on pat                               s = 
  let (before, after) = breakOn pat s
  in L.length before + L.length pat + L.length after <= L.length s + L.length pat

prop_break_on_empty_pattern :: String -> Bool
prop_break_on                               s = breakOn ""                               s == ("", s)

prop_break_on_pattern_not_found :: String -> String -> Bool
prop_break_on pat                               s = 
  pat `notElem` s && pat /= "" ==> 
let (before, after) = breakOn pat s
  in                               before == s &&                               after == ""

-- ============================================================================
-- Unit Tests for Edge Cases
-- ============================================================================

test_trim_empty :: TestTree
test_trim_empty =             testCase "trim empty string" $
            assertEqual "" "" (trim "")

test_trim_all_whitespace :: TestTree
test_trim_all_whitespace =             testCase "trim L.all whitespace" $
            assertEqual "" "" (trim "   \t\n\r   ")

test_trim_single_word :: TestTree
test_trim_single_word =             testCase "trim single word" $
            assertEqual "hello" "hello" (trim "hello")

test_trim_with_internal_whitespace :: TestTree
test_trim_with_internal_whitespace =             testCase "trim with internal whitespace" $
            assertEqual "hello world" "hello world" (trim "  hello world  ")

test_split_by_empty_string :: TestTree
test_split_by_empty_string =             testCase "splitBy empty string" $
            assertEqual [""] (splitBy ',' "")

test_split_by_no_delimiter :: TestTree
test_split_by_no_delimiter =             testCase "splitBy no delimiter" $
            assertEqual ["hello"] (splitBy ',' "hello")

test_split_by_multiple_delimiters :: TestTree
test_split_by_multiple_delimiters =             testCase "splitBy multiple delimiters" $
            assertEqual ["a", "b", "c"] (splitBy ',' "a,b,c")

test_split_by_preserves_empty :: TestTree
test_split_by_preserves_empty =             testCase "splitBy preserves empty segments" $
            assertEqual ["a", "", "c"] (splitBy ',' "a,,c")

test_remove_comments_empty :: TestTree
test_remove_comments_empty =             testCase "removeComments empty string" $
            assertEqual "" (removeComments "")

test_remove_comments_no_comments :: TestTree
test_remove_comments_no_comments =             testCase "removeComments no comments" $
            assertEqual "hello world" (removeComments "hello world")

test_remove_comments_only_line_comment :: TestTree
test_remove_comments_only_line_comment =             testCase "removeComments only line comment" $
            assertEqual "" (removeComments "// this is a comment")

test_remove_comments_only_block_comment :: TestTree
test_remove_comments_only_block_comment =             testCase "removeComments only block comment" $
            assertEqual "" (removeComments "/* this is a block comment */")

test_remove_comments_mixed :: TestTree
test_remove_comments_mixed =             testCase "removeComments mixed content" $
            assertEqual "hello world\n" (removeComments "hello // comment\nworld /* block */\n")

test_normalize_indentation_empty :: TestTree
test_normalize_indentation_empty =             testCase "normalizeIndentation empty string" $
            assertEqual "" (normalizeIndentation "")

test_normalize_indentation_no_indentation :: TestTree
test_normalize_indentation_no_indentation =             testCase "normalizeIndentation no indentation" $
            assertEqual "hello\nworld" (normalizeIndentation "hello\nworld")

test_normalize_indentation_with_indentation :: TestTree
test_normalize_indentation_with_indentation =             testCase "normalizeIndentation with indentation" $
            assertEqual "hello\n  world\n    test" (normalizeIndentation "  hello\n    world\n      test")

test_break_on_empty_pattern :: TestTree
test_break_on_empty_pattern =             testCase "breakOn empty pattern" $
            assertEqual ("", "hello") (breakOn "" "hello")

test_break_on_pattern_found :: TestTree
test_break_on_pattern_found =             testCase "breakOn pattern found" $
            assertEqual ("he", "o") (breakOn "ll" "hello")

test_break_on_pattern_not_found :: TestTree
test_break_on_pattern_not_found =             testCase "breakOn pattern not found" $
            assertEqual ("hello", "") (breakOn "xyz" "hello")

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests =   testGroup "Utils Additional QuickCheck Tests"
  [ testGroup "QuickCheck Properties"
    [             testProperty "trim idempotent" prop_trim_idempotent
    ,             testProperty "trim no leading/trailing whitespace" prop_trim_no_leading_trailing_whitespace
    ,             testProperty "trim preserves internal whitespace" prop_trim_preserves_internal_whitespace
    ,             testProperty "splitBy L.length" prop_split_by_length
    ,             testProperty "splitBy join" prop_split_by_join
    ,             testProperty "splitByCollapsed subset" prop_split_by_collapsed_subset
    ,             testProperty "splitByComma equals splitBy" prop_split_by_comma_equals_split_by
    ,             testProperty "splitByCommaCollapsed equals splitByCollapsed" prop_split_by_comma_collapsed_equals_split_by_collapsed
    ,             testProperty "removeLineComments no double slash" prop_remove_line_comments_no_double_slash
    ,             testProperty "removeLineComments preserves non-comment lines" prop_remove_line_comments_preserves_non_comment_lines
    ,             testProperty "removeComments no comment markers" prop_remove_comments_no_comment_markers
    ,             testProperty "removeComments preserves non-comment content" prop_remove_comments_preserves_non_comment_content
    ,             testProperty "normalizeIndentation preserves line count" prop_normalize_indentation_preserves_line_count
    ,             testProperty "normalizeIndentation removes common prefix" prop_normalize_indentation_removes_common_prefix
    ,             testProperty "breakOn returns tuple" prop_break_on_returns_tuple
    ,             testProperty "breakOn empty pattern" prop_break_on_empty_pattern
    ,             testProperty "breakOn pattern not found" prop_break_on_pattern_not_found
    ]
  , testGroup "Unit Tests"
    [ test_trim_empty
    , test_trim_all_whitespace
    , test_trim_single_word
    , test_trim_with_internal_whitespace
    , test_split_by_empty_string
    , test_split_by_no_delimiter
    , test_split_by_multiple_delimiters
    , test_split_by_preserves_empty
    , test_remove_comments_empty
    , test_remove_comments_no_comments
    , test_remove_comments_only_line_comment
    , test_remove_comments_only_block_comment
    , test_remove_comments_mixed
    , test_normalize_indentation_empty
    , test_normalize_indentation_no_indentation
    , test_normalize_indentation_with_indentation
    , test_break_on_empty_pattern
    , test_break_on_pattern_found
    , test_break_on_pattern_not_found
    ]
  ]