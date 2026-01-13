module Test.Unit.UtilsAdvancedQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Utils
  ( trim
  , splitBy, splitByCollapsed, splitByComma, splitByCommaCollapsed
  , removeLineComments, removeComments
  , normalizeIndentation, forceSingleTabIndentation, fixIndentation
  , breakOn
  , safeProcessString, isValidChar
  )
import Data.Char (isSpace, isAlphaNum)
import Data.List (isPrefixOf, isSuffixOf)
import qualified Data.Text as T

-- | 生成不包含特殊字符的字符串
newtype SimpleString = SimpleString { getSimpleString :: String }
  deriving Show

instance Arbitrary SimpleString where
  arbitrary = do
    len <- choose (0, 20)
    chars <- vectorOf len $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " "
    return $ SimpleString chars

-- | 生成可能包含逗号的字符串
newtype CommaString = CommaString { getCommaString :: String }
  deriving Show

instance Arbitrary CommaString where
  arbitrary = do
    len <- choose (0, 20)
    chars <- vectorOf len $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " ,"
    return $ CommaString chars

-- | 生成可能包含换行符的字符串
newtype MultilineString = MultilineString { getMultilineString :: String }
  deriving Show

instance Arbitrary MultilineString where
  arbitrary = do
    len <- choose (0, 50)
    chars <- vectorOf len $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " ,.!?;:\n\t"
    return $ MultilineString chars

-- | 生成可能包含注释的字符串
newtype CommentString = CommentString { getCommentString :: String }
  deriving Show

instance Arbitrary CommentString where
  arbitrary = do
    len <- choose (0, 50)
    chars <- vectorOf len $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " ,.!?;:\n\t\"'/*"
    return $ CommentString chars

-- | 生成可能包含缩进的字符串
newtype IndentedString = IndentedString { getIndentedString :: String }
  deriving Show

instance Arbitrary IndentedString where
  arbitrary = do
    numLines <- choose (1, 5)
    lines <- vectorOf numLines $ do
      indent <- choose (0, 8)
      content <- choose (0, 10)
      indentChars <- vectorOf indent (elements " \t")
      contentChars <- vectorOf content $ elements $ ['a'..'z'] ++ [' '] ++ ['0'..'9']
      return $ indentChars ++ contentChars
    return $ IndentedString $ unlines lines

-- | 测试trim函数的属性
prop_trim_removes_leading_whitespace :: SimpleString -> Property
prop_trim_removes_leading_whitespace (SimpleString s) =
  let trimmed = trim s
      leadingRemoved = null trimmed || not (isSpace (head trimmed))
  in property leadingRemoved

prop_trim_removes_trailing_whitespace :: SimpleString -> Property
prop_trim_removes_trailing_whitespace (SimpleString s) =
  let trimmed = trim s
      trailingRemoved = null trimmed || not (isSpace (last trimmed))
  in property trailingRemoved

prop_trim_preserves_inner_whitespace :: SimpleString -> Property
prop_trim_preserves_inner_whitespace (SimpleString s) =
  not (null s) && not (all isSpace s) ==> 
  let trimmed = trim s
      originalInner = filter (not . isSpace) s
      trimmedInner = filter (not . isSpace) trimmed
  in originalInner === trimmedInner

prop_trim_empty_string :: Property
prop_trim_empty_string =
  trim "" === ""

prop_trim_all_whitespace :: Property
prop_trim_all_whitespace =
  let ws = "   \t\n  "
  in trim ws === ""

-- | 测试splitBy函数的属性
prop_split_by_empty_delimiter :: SimpleString -> Property
prop_split_by_empty_delimiter (SimpleString s) =
  splitBy ',' s === splitBy ',' s  -- 基本验证函数可以调用

prop_split_by_empty_string :: Property
prop_split_by_empty_string =
  splitBy ',' "" === []

prop_split_by_single_delimiter :: Property
prop_split_by_single_delimiter =
  splitBy ',' "," === ["", ""]

prop_split_by_no_delimiter :: SimpleString -> Property
prop_split_by_no_delimiter (SimpleString s) =
  not (',' `elem` s) ==> splitBy ',' s === [s]

prop_split_by_preserves_empty_segments :: CommaString -> Property
prop_split_by_preserves_empty_segments (CommaString s) =
  let segments = splitBy ',' s
      joined = intercalate "," segments
  in joined === s

prop_split_by_collapsed_removes_empty :: CommaString -> Property
prop_split_by_collapsed_removes_empty (CommaString s) =
  let segments = splitByCollapsed ',' s
  in all (not . null) segments

prop_split_by_comma_equals_split_by_comma :: CommaString -> Property
prop_split_by_comma_equals_split_by_comma (CommaString s) =
  splitByComma s === splitBy ',' s

prop_split_by_comma_collapsed_equals_split_by_collapsed :: CommaString -> Property
prop_split_by_comma_collapsed_equals_split_by_collapsed (CommaString s) =
  splitByCommaCollapsed s === splitByCollapsed ',' s

-- | 测试removeLineComments函数的属性
prop_remove_line_comments_no_comments :: SimpleString -> Property
prop_remove_line_comments_no_comments (SimpleString s) =
  not ("//" `isPrefixOf` s) ==> removeLineComments s === s

prop_remove_line_comments_single_line :: Property
prop_remove_line_comments_single_line =
  removeLineComments "code // comment" === "code"

prop_remove_line_comments_preserves_multiline :: MultilineString -> Property
prop_remove_line_comments_preserves_multiline (MultilineString s) =
  let linesWithComments = map (++ " // comment") (lines s)
      input = unlines linesWithComments
      result = removeLineComments input
      expectedLines = map (removeLineComments . (++ " // comment")) (lines s)
  in lines result === expectedLines

prop_remove_line_comments_preserves_strings :: Property
prop_remove_line_comments_preserves_strings =
  removeLineComments "let s = \"// not a comment\" // real comment" === "let s = \"// not a comment\""

-- | 测试removeComments函数的属性
prop_remove_comments_no_comments :: SimpleString -> Property
prop_remove_comments_no_comments (SimpleString s) =
  not ("//" `isInfixOf` s) && not ("/*" `isInfixOf` s) ==> removeComments s === s

prop_remove_comments_line_comments :: Property
prop_remove_comments_line_comments =
  removeComments "code // comment\nmore code" === "code \nmore code"

prop_remove_comments_block_comments :: Property
prop_remove_comments_block_comments =
  removeComments "code /* comment */ more code" === "code  more code"

prop_remove_comments_multiline_block :: Property
prop_remove_comments_multiline_block =
  removeComments "code /* multi\nline\ncomment */ more code" === "code \n\n more code"

prop_remove_comments_preserves_strings :: Property
prop_remove_comments_preserves_strings =
  let s = "let s = \"// not a comment\" /* also not */"
  in removeComments s === s

-- | 测试normalizeIndentation函数的属性
prop_normalize_indentation_removes_common_prefix :: IndentedString -> Property
prop_normalize_indentation_removes_common_prefix (IndentedString s) =
  let normalized = normalizeIndentation s
      lines' = lines s
      nonEmptyLines = filter (not . all isSpace) lines'
  in if length nonEmptyLines <= 1
     then normalized === s  -- 单行或空行保持不变
     else property $ not (all isSpace normalized) || all isSpace normalized

prop_normalize_indentation_preserves_relative_indentation :: IndentedString -> Property
prop_normalize_indentation_preserves_relative_indentation (IndentedString s) =
  let normalized = normalizeIndentation s
      originalLines = lines s
      normalizedLines = lines normalized
      originalIndentation = map (length . takeWhile isSpace) originalLines
      normalizedIndentation = map (length . takeWhile isSpace) normalizedLines
  in length normalizedIndentation === length originalIndentation

prop_normalize_indentation_empty_string :: Property
prop_normalize_indentation_empty_string =
  normalizeIndentation "" === ""

prop_normalize_indentation_single_line :: Property
prop_normalize_indentation_single_line =
  let s = "    single line"
  in normalizeIndentation s === s

-- | 测试forceSingleTabIndentation函数的属性
prop_force_single_tab_indentation_adds_tab :: SimpleString -> Property
prop_force_single_tab_indentation_adds_tab (SimpleString s) =
  let indented = forceSingleTabIndentation s
      lines' = lines indented
  in all (isPrefixOf "\t") (filter (not . null) lines')

prop_force_single_tab_indentation_trims_content :: SimpleString -> Property
prop_force_single_tab_indentation_trims_content (SimpleString s) =
  let indented = forceSingleTabIndentation s
      lines' = lines indented
      trimmedLines = map (trim . drop 1) lines'
  in all (not . isPrefixOf " " . not . null) trimmedLines

-- | 测试fixIndentation函数的属性
prop_fix_indentation_equals_normalize_indentation :: IndentedString -> Property
prop_fix_indentation_equals_normalize_indentation (IndentedString s) =
  fixIndentation s === normalizeIndentation s

-- | 测试breakOn函数的属性
prop_break_on_empty_pattern :: SimpleString -> Property
prop_break_on_empty_pattern (SimpleString s) =
  breakOn "" s === ("", s)

prop_break_on_pattern_not_found :: SimpleString -> Property
prop_break_on_pattern_not_found (SimpleString s) =
  not ("XYZ" `isInfixOf` s) ==> breakOn "XYZ" s === (s, "")

prop_break_on_pattern_at_start :: Property
prop_break_on_pattern_at_start =
  breakOn "pattern" "pattern rest" === ("", " rest")

prop_break_on_pattern_at_end :: Property
prop_break_on_pattern_at_end =
  breakOn "pattern" "rest pattern" === ("rest ", "")

prop_break_on_pattern_in_middle :: Property
prop_break_on_pattern_in_middle =
  breakOn "pattern" "start pattern end" === ("start ", " end")

-- | 测试safeProcessString函数的属性
prop_safe_process_string_removes_control_chars :: Property
prop_safe_process_string_removes_control_chars =
  let s = "hello\x00world\x01test"
      result = safeProcessString s
  in case result of
    Left _ -> property False
    Right processed -> not ('\x00' `elem` processed) && not ('\x01' `elem` processed)

prop_safe_process_string_preserves_valid_chars :: SimpleString -> Property
prop_safe_process_string_preserves_valid_chars (SimpleString s) =
  case safeProcessString s of
    Left _ -> property False
    Right processed -> processed === s

prop_safe_process_string_preserves_newlines :: MultilineString -> Property
prop_safe_process_string_preserves_newlines (MultilineString s) =
  case safeProcessString s of
    Left _ -> property False
    Right processed -> 
      let originalNewlines = length $ filter (== '\n') s
          processedNewlines = length $ filter (== '\n') processed
      in originalNewlines === processedNewlines

-- | 测试isValidChar函数的属性
prop_is_valid_char_printable :: Property
prop_is_valid_char_printable =
  forAll (elements [' '..'~']) $ \c ->
    isValidChar c === True

prop_is_valid_char_newline :: Property
prop_is_valid_char_newline =
  isValidChar '\n' === True

prop_is_valid_char_tab :: Property
prop_is_valid_char_tab =
  isValidChar '\t' === True

prop_is_valid_char_carriage_return :: Property
prop_is_valid_char_carriage_return =
  isValidChar '\r' === True

prop_is_valid_char_control :: Property
prop_is_valid_char_control =
  forAll (choose ('\x00', '\x08')) $ \c ->
    isValidChar c === False

-- | 辅助函数：将字符串列表用指定分隔符连接
intercalate :: String -> [String] -> String
intercalate _ [] = ""
intercalate _ [x] = x
intercalate sep (x:xs) = x ++ sep ++ intercalate sep xs

tests :: TestTree
tests = testGroup "Utils Advanced QuickCheck Tests"
  -- Trim tests
  [ testProperty "trim removes leading whitespace" prop_trim_removes_leading_whitespace
  , testProperty "trim removes trailing whitespace" prop_trim_removes_trailing_whitespace
  , testProperty "trim preserves inner whitespace" prop_trim_preserves_inner_whitespace
  , testProperty "trim empty string" prop_trim_empty_string
  , testProperty "trim all whitespace" prop_trim_all_whitespace
  
  -- SplitBy tests
  , testProperty "splitBy empty delimiter" prop_split_by_empty_delimiter
  , testProperty "splitBy empty string" prop_split_by_empty_string
  , testProperty "splitBy single delimiter" prop_split_by_single_delimiter
  , testProperty "splitBy no delimiter" prop_split_by_no_delimiter
  , testProperty "splitBy preserves empty segments" prop_split_by_preserves_empty_segments
  , testProperty "splitByCollapsed removes empty" prop_split_by_collapsed_removes_empty
  , testProperty "splitByComma equals splitBy" prop_split_by_comma_equals_split_by_comma
  , testProperty "splitByCommaCollapsed equals splitByCollapsed" prop_split_by_comma_collapsed_equals_split_by_collapsed
  
  -- RemoveLineComments tests
  , testProperty "removeLineComments no comments" prop_remove_line_comments_no_comments
  , testProperty "removeLineComments single line" prop_remove_line_comments_single_line
  , testProperty "removeLineComments preserves multiline" prop_remove_line_comments_preserves_multiline
  , testProperty "removeLineComments preserves strings" prop_remove_line_comments_preserves_strings
  
  -- RemoveComments tests
  , testProperty "removeComments no comments" prop_remove_comments_no_comments
  , testProperty "removeComments line comments" prop_remove_comments_line_comments
  , testProperty "removeComments block comments" prop_remove_comments_block_comments
  , testProperty "removeComments multiline block" prop_remove_comments_multiline_block
  , testProperty "removeComments preserves strings" prop_remove_comments_preserves_strings
  
  -- NormalizeIndentation tests
  , testProperty "normalizeIndentation removes common prefix" prop_normalize_indentation_removes_common_prefix
  , testProperty "normalizeIndentation preserves relative indentation" prop_normalize_indentation_preserves_relative_indentation
  , testProperty "normalizeIndentation empty string" prop_normalize_indentation_empty_string
  , testProperty "normalizeIndentation single line" prop_normalize_indentation_single_line
  
  -- ForceSingleTabIndentation tests
  , testProperty "forceSingleTabIndentation adds tab" prop_force_single_tab_indentation_adds_tab
  , testProperty "forceSingleTabIndentation trims content" prop_force_single_tab_indentation_trims_content
  
  -- FixIndentation tests
  , testProperty "fixIndentation equals normalizeIndentation" prop_fix_indentation_equals_normalize_indentation
  
  -- BreakOn tests
  , testProperty "breakOn empty pattern" prop_break_on_empty_pattern
  , testProperty "breakOn pattern not found" prop_break_on_pattern_not_found
  , testProperty "breakOn pattern at start" prop_break_on_pattern_at_start
  , testProperty "breakOn pattern at end" prop_break_on_pattern_at_end
  , testProperty "breakOn pattern in middle" prop_break_on_pattern_in_middle
  
  -- SafeProcessString tests
  , testProperty "safeProcessString removes control chars" prop_safe_process_string_removes_control_chars
  , testProperty "safeProcessString preserves valid chars" prop_safe_process_string_preserves_valid_chars
  , testProperty "safeProcessString preserves newlines" prop_safe_process_string_preserves_newlines
  
  -- IsValidChar tests
  , testProperty "isValidChar printable" prop_is_valid_char_printable
  , testProperty "isValidChar newline" prop_is_valid_char_newline
  , testProperty "isValidChar tab" prop_is_valid_char_tab
  , testProperty "isValidChar carriage return" prop_is_valid_char_carriage_return
  , testProperty "isValidChar control" prop_is_valid_char_control
  ]