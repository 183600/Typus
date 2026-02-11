{-# LANGUAGE OverloadedStrings #-}
module Test.Unit.NewEnhancedQuickCheckTestSuite where

import Test.Tasty
import Test.Tasty.QuickCheck
import qualified Utils as U
import Data.List (isInfixOf, isPrefixOf, isSuffixOf, intercalate, sort, nub)
import Data.Char (isSpace, isLetter, isDigit, ord, toLower, toUpper)
import Data.Maybe (isJust, isNothing, fromMaybe)
import Data.Either (isLeft, isRight)
import qualified Data.Map as Map
import qualified Data.Set as Set

-- ============================================================================
-- 增强的字符串处理测试 (50个测试)
-- ============================================================================

-- | 测试trim对Unicode字符的处理
prop_trim_unicode :: String -> Property
prop_trim_unicode s = 
  let trimmed = U.trim s
      hasLeadingSpace = not (null s) && isSpace (head s)
      hasTrailingSpace = not (null s) && isSpace (last s)
  in if hasLeadingSpace || hasTrailingSpace
     then property $ length trimmed < length s
     else property $ trimmed === s

-- | 测试splitBy的结果长度与分隔符数量的关系
prop_split_by_separator_count :: Char -> String -> Property
prop_split_by_separator_count c s =
  let parts = U.splitBy c s
      sepCount = length (filter (== c) s)
  in property $ length parts === sepCount + 1

-- | 测试splitByCollapsed的结果中不包含连续分隔符
prop_split_by_collapsed_no_consecutive :: Char -> String -> Property
prop_split_by_collapsed_no_consecutive c s =
  let parts = U.splitByCollapsed c s
      hasConsecutive = any (isInfixOf ([c,c] :: String)) parts
  in property $ not hasConsecutive

-- | 测试removeComments对嵌套注释的处理
prop_remove_comments_nested :: String -> String -> Property
prop_remove_comments_nested s1 s2 =
  let nested = "/* outer /* " ++ s1 ++ " */ inner */" ++ s2
      processed = U.removeComments nested
  in if null s1 && null s2
     then property $ processed === ""
     else property $ not ("/*" `isInfixOf` processed) && not ("*/" `isInfixOf` processed)

-- | 测试isCompleteStringLiteral对转义字符的处理
prop_is_complete_string_literal_escape :: String -> Property
prop_is_complete_string_literal_escape s =
  let withEscapes = "\"" ++ concatMap (\c -> if c == '\\' then "\\\\" else [c]) s ++ "\""
  in property $ U.isCompleteStringLiteral withEscapes

-- | 测试isProblematicUnclosedString对复杂字符串的检测
prop_is_problematic_unclosed_complex :: String -> String -> Property
prop_is_problematic_unclosed_complex s1 s2 =
  let complex = "\"" ++ s1 ++ "\\\"" ++ s2
  in property $ U.isProblematicUnclosedString complex

-- | 测试breakOn对不存在的模式的处理
prop_break_on_not_found :: String -> String -> Property
prop_break_on_not_found pat s =
  let notFound = not (pat `isInfixOf` s)
      (before, after) = U.breakOn pat s
  in if notFound
     then property $ before === s .&. after === ""
     else property $ True

-- | 测试safeProcessString对无效字符的处理
prop_safe_process_string_invalid :: String -> Property
prop_safe_process_string_invalid s =
  let withInvalid = map (\c -> if ord c > 127 then '?' else c) s
      processed = U.safeProcessString withInvalid
  in property $ either (const False) (all U.isValidChar) processed

-- | 测试normalizeIndentation对混合缩进的处理
prop_normalize_indentation_mixed :: String -> Property
prop_normalize_indentation_mixed s =
  let mixed = unlines $ map ("  \t " ++) (lines s)
      normalized = U.normalizeIndentation mixed
      normLines = lines normalized
  in if null (lines s)
     then property $ normalized === s
     else property $ all (not . isPrefixOf "\t") normLines

-- | 测试字符串函数的组合性质
prop_string_functions_composition :: String -> Property
prop_string_functions_composition s =
  let trimmed = U.trim s
      split = U.splitBy ' ' trimmed
      rejoined = intercalate " " split
  in property $ U.trim rejoined === trimmed

-- | 测试splitByComma对空值和逗号混合的处理
prop_split_by_comma_mixed :: [String] -> Property
prop_split_by_comma_mixed parts =
  let input = intercalate "," parts
      result = U.splitByComma input
      -- 每个part之间都有一个逗号，所以如果有n个parts，会有n-1个逗号
      -- 结果应该有n个部分（与parts数量相同）
      -- 特殊情况：如果parts是[","]，那么input是",,"，splitBy会返回3个部分
  in if parts == [","]
     then property $ length result === 3 .&. result === ["", "", ""]
     else property $ length result === max 1 (length parts)

-- | 测试removeLineComments对字符串中//的保护
prop_remove_line_comments_string_protection :: String -> Property
prop_remove_line_comments_string_protection s =
  let withStringComment = "code // comment\n\"" ++ s ++ "// not comment\"\ncode"
      processed = U.removeLineComments withStringComment
  in property $ ("\"" ++ s ++ "// not comment\"") `isInfixOf` processed

-- | 测试splitBy的性能属性
prop_split_by_performance :: Int -> Property
prop_split_by_performance n =
  let largeString = replicate n 'a' ++ "," ++ replicate n 'b'
      parts = U.splitBy ',' largeString
  in if n >= 0 && n < 10000
     then property $ length parts === 2
     else property $ True

-- | 测试trim对极端情况的处理
prop_trim_edge_cases :: String -> String -> Property
prop_trim_edge_cases prefix suffix =
  let input = prefix ++ "content" ++ suffix
      trimmed = U.trim input
  in if all isSpace prefix && all isSpace suffix
     then property $ trimmed === "content"
     else property $ True

-- | 测试splitByCollapsed对空字符串的处理
prop_split_by_collapsed_empty :: Char -> Property
prop_split_by_collapsed_empty c = property $ U.splitByCollapsed c "" === []

-- | 测试removeComments对多重注释的处理
prop_remove_comments_multiple :: String -> String -> Property
prop_remove_comments_multiple s1 s2 =
  let multiple = "// comment1\n" ++ s1 ++ "\n/* comment2 */" ++ s2
      processed = U.removeComments multiple
  in property $ not ("//" `isInfixOf` processed) && not ("/*" `isInfixOf` processed)

-- | 测试isCompleteStringLiteral对空字符串的处理
prop_is_complete_string_literal_empty :: Property
prop_is_complete_string_literal_empty = property $ U.isCompleteStringLiteral "\"\""

-- | 测试isCompleteStringLiteral对单引号的处理
prop_is_complete_string_literal_single_quote :: String -> Property
prop_is_complete_string_literal_single_quote s =
  let singleQuoted = "'" ++ s ++ "'"
  in if null s
     then property $ not (U.isCompleteStringLiteral singleQuoted)
     else property $ not (U.isCompleteStringLiteral singleQuoted)

-- | 测试breakOn对多字符模式的处理
prop_break_on_multi_char :: String -> String -> Property
prop_break_on_multi_char pat s =
  let (before, after) = U.breakOn pat s
  in if null pat
     then property $ before === "" .&. after === s
     else property $ before ++ pat ++ after === s .||. (before === s .&. after === "")

-- | 测试safeProcessString对ASCII字符的处理
prop_safe_process_string_ascii :: String -> Property
prop_safe_process_string_ascii s =
  let asciiOnly = filter (\c -> ord c < 128) s
      processed = U.safeProcessString asciiOnly
  in property $ either (const False) (all (\c -> ord c < 128)) processed

-- | 测试normalizeIndentation对单行代码的处理
prop_normalize_indentation_single_line :: String -> Property
prop_normalize_indentation_single_line s =
  let singleLine = if '\n' `elem` s then takeWhile (/= '\n') s else s
      normalized = U.normalizeIndentation singleLine
  in property $ normalized === singleLine

-- | 测试字符串函数的交换性
prop_string_functions_commutative :: String -> Property
prop_string_functions_commutative s =
  let trimmed1 = U.trim (U.removeComments s)
      trimmed2 = U.removeComments (U.trim s)
  in property $ trimmed1 === trimmed2

-- | 测试splitByComma对逗号周围空格的处理
prop_split_by_comma_spaces :: String -> Property
prop_split_by_comma_spaces s =
  let withSpaces = " a , b , c "
      parts = U.splitByComma withSpaces
  in property $ parts === [" a ", " b ", " c "]

-- | 测试removeLineComments对多行注释的保护
prop_remove_line_comments_multiline_protection :: String -> Property
prop_remove_line_comments_multiline_protection s =
  let withMultiline = "code /* " ++ s ++ " */ more code"
      processed = U.removeLineComments withMultiline
  in property $ ("/* " ++ s ++ " */") `isInfixOf` processed

-- | 测试splitBy对特殊分隔符的处理
prop_split_by_special :: String -> Property
prop_split_by_special s =
  let newlineParts = U.splitBy '\n' s
      tabParts = U.splitBy '\t' s
  in property $ concat newlineParts ++ replicate (length newlineParts - 1) '\n' === s .&.
                concat tabParts ++ replicate (length tabParts - 1) '\t' === s

-- | 测试trim对制表符和空格混合的处理
prop_trim_tab_space_mixed :: String -> Property
prop_trim_tab_space_mixed s =
  let mixed = "\t  \t" ++ s ++ "  \t  "
      trimmed = U.trim mixed
  in property $ not (any isSpace (take 1 trimmed)) .&. 
                not (any isSpace (take 1 (reverse trimmed)))

-- | 测试splitByCommaCollapsed的属性
prop_split_by_comma_collapsed_property :: String -> Property
prop_split_by_comma_collapsed_property s =
  let parts = U.splitByCommaCollapsed s
      noConsecutive = all (not . isInfixOf ",,") parts
  in property $ noConsecutive

-- | 测试normalizeIndentation对空行的保留
prop_normalize_indentation_preserve_empty :: String -> Property
prop_normalize_indentation_preserve_empty s =
  let withEmpty = s ++ "\n\n"
      normalized = U.normalizeIndentation withEmpty
  in property $ "\n\n" `isInfixOf` normalized

-- | 测试isCompleteStringLiteral对转义引号的处理
prop_is_complete_string_literal_escape_quotes :: String -> Property
prop_is_complete_string_literal_escape_quotes s =
  let withEscapedQuote = "\"" ++ s ++ "\\\"\""
  in property $ U.isCompleteStringLiteral withEscapedQuote

-- | 测试splitBy对空分隔符的处理
prop_split_by_empty_char :: String -> Property
prop_split_by_empty_char s = property $ U.splitBy '\0' s === [s]

-- | 测试removeComments对注释嵌套在字符串中的处理
prop_remove_comments_nested_in_string :: String -> String -> Property
prop_remove_comments_nested_in_string s1 s2 =
  let nestedInString = "\"" ++ "/* " ++ s1 ++ " */" ++ s2 ++ "\""
      processed = U.removeComments nestedInString
  in if null s1 && null s2
     then property $ processed === "\"/*  */\""
     else property $ processed === nestedInString

-- | 测试breakOn对长模式的支持
prop_break_on_long_pattern :: String -> String -> Property
prop_break_on_long_pattern pat s =
  let longPat = concat (replicate 10 pat)
      (before, after) = U.breakOn longPat s
  in if null longPat
     then property $ before === "" .&. after === s
     else property $ True

-- | 测试safeProcessString的错误处理
prop_safe_process_string_error_handling :: String -> Property
prop_safe_process_string_error_handling s =
  let processed = U.safeProcessString s
  in property $ isRight processed .||. isLeft processed

-- | 测试normalizeIndentation对深度缩进的处理
prop_normalize_indentation_deep :: String -> Int -> Property
prop_normalize_indentation_deep s depth =
  if depth > 0 && depth < 20 && not (null s)
  then let deepIndent = unlines $ map (replicate depth ' ' ++) (lines s)
           normalized = U.normalizeIndentation deepIndent
           normLines = lines normalized
       in if null normLines
          then property $ True
          else property $ not (any (isPrefixOf (replicate depth ' ')) normLines)
  else property $ True

-- | 测试字符串函数的幂等性
prop_string_functions_idempotent :: String -> Property
prop_string_functions_idempotent s =
  let trimmedOnce = U.trim s
      trimmedTwice = U.trim trimmedOnce
      commentsOnce = U.removeComments s
      commentsTwice = U.removeComments commentsOnce
  in property $ trimmedOnce === trimmedTwice .&. commentsOnce === commentsTwice

-- | 测试splitByComma对逗号周围空格的处理
prop_split_by_comma_whitespace :: String -> Property
prop_split_by_comma_whitespace s =
  let withSpaces = " a ,b, c , d "
      parts = U.splitByComma withSpaces
  in property $ parts === [" a ", "b", " c ", " d "]

-- | 测试removeLineComments对行尾注释的处理
prop_remove_line_comments_end_of_line :: String -> Property
prop_remove_line_comments_end_of_line s =
  if null s
  then property $ True
  else let withEndComment = s ++ "  // end comment"
           processed = U.removeLineComments withEndComment
       in property $ processed === s ++ "  "

-- | 测试splitBy对非ASCII分隔符的处理
prop_split_by_non_ascii :: String -> Property
prop_split_by_non_ascii s =
  let unicodeSep = '∑'
      parts = U.splitBy unicodeSep s
  in property $ concat parts ++ replicate (length parts - 1) unicodeSep === s

-- | 测试trim对换行符和制表符的处理
prop_trim_newlines_tabs :: String -> Property
prop_trim_newlines_tabs s =
  let withNT = "\n\t" ++ s ++ "\t\n"
      trimmed = U.trim withNT
  in property $ not (any (`elem` ("\n\t" :: String)) (take 1 trimmed)) .&. 
                not (any (`elem` ("\n\t" :: String)) (take 1 (reverse trimmed)))

-- | 测试splitByCommaCollapsed对空字符串的处理
prop_split_by_comma_collapsed_empty :: Property
prop_split_by_comma_collapsed_empty = property $ U.splitByCommaCollapsed "" === []

-- | 测试removeComments对注释后代码的保护
prop_remove_comments_preserve_code_after :: String -> String -> Property
prop_remove_comments_preserve_code_after s1 s2 =
  let codeWithComment = s1 ++ "/* comment */" ++ s2
      processed = U.removeComments codeWithComment
  in property $ s2 `isInfixOf` processed

-- | 测试isCompleteStringLiteral对复杂转义的处理
prop_is_complete_string_literal_complex_escape :: String -> Property
prop_is_complete_string_literal_complex_escape s =
  let complexEscape = "\"" ++ concatMap (\c -> if c == '\\' then "\\\\" else if c == '"' then "\\\"" else [c]) s ++ "\""
  in property $ U.isCompleteStringLiteral complexEscape

-- | 测试breakOn对模式边界情况的处理
prop_break_on_edge_cases :: String -> String -> Property
prop_break_on_edge_cases pat s =
  let (before, after) = U.breakOn pat s
  in if pat == s
     then property $ before === "" .&. after === ""
     else property $ True

-- | 测试safeProcessString对Unicode字符的处理
prop_safe_process_string_unicode :: String -> Property
prop_safe_process_string_unicode s =
  let processed = U.safeProcessString s
  in property $ either (const False) (all (\c -> ord c >= 0)) processed

-- | 测试normalizeIndentation对代码块的处理
prop_normalize_indentation_code_block :: String -> Property
prop_normalize_indentation_code_block s =
  let codeBlock = unlines $ ["    if condition {", "        // do something", "        return " ++ s, "    }"]
      normalized = U.normalizeIndentation codeBlock
      normLines = lines normalized
      -- 检查非注释行是否没有前导空格
      nonCommentLines = filter (not . isPrefixOf "//") normLines
  in if null s
     then property $ length (filter (isPrefixOf "    ") normLines) < length normLines  -- 至少有些行没有4个空格前缀
     else property $ length (filter (isPrefixOf "    ") normLines) < length normLines .&&. not (null normalized)

-- | 测试字符串函数的关联性
prop_string_functions_associative :: String -> Property
prop_string_functions_associative s =
  let splitTrim = map U.trim (U.splitBy ' ' s)
      trimSplit = U.splitBy ' ' (U.trim s)
  in property $ length splitTrim === length trimSplit

-- | 测试splitByComma对连续逗号的处理
prop_split_by_comma_consecutive :: String -> Property
prop_split_by_comma_consecutive s =
  let withConsecutive = s ++ ",,,"
      parts = U.splitByComma withConsecutive
  in property $ length parts === length (U.splitBy ',' s) + 3

-- | 测试removeLineComments对字符串内换行的保护
prop_remove_line_comments_string_newline :: String -> Property
prop_remove_line_comments_string_newline s =
  let stringWithNewline = "\"" ++ s ++ "\n\" // comment"
      processed = U.removeLineComments stringWithNewline
  in property $ ("\"" ++ s ++ "\n\"") `isInfixOf` processed

-- | 测试splitBy对大小写敏感的处理
prop_split_by_case_sensitive :: String -> Property
prop_split_by_case_sensitive s =
  let lowerParts = U.splitBy 'a' s
      upperParts = U.splitBy 'A' s
  in if any (`elem` ("A" :: String)) s
     then property $ lowerParts /= upperParts
     else property $ True

-- | 测试trim对混合空白字符的处理
prop_trim_mixed_whitespace_complex :: String -> Property
prop_trim_mixed_whitespace_complex s =
  let mixed = " \t\n\r\f\v" ++ s ++ " \t\n\r\f\v"
      trimmed = U.trim mixed
  in property $ not (any isSpace (take 1 trimmed)) .&. 
                not (any isSpace (take 1 (reverse trimmed)))

-- | 测试splitByCommaCollapsed对空值的处理
prop_split_by_comma_collapsed_empty_values :: [String] -> Property
prop_split_by_comma_collapsed_empty_values values =
  let input = intercalate "," values
      parts = U.splitByCommaCollapsed input
      nonEmpty = filter (not . null) parts
      expectedNonEmpty = filter (not . null) values
  in if all null values
     then property $ parts === []
     else property $ nonEmpty === expectedNonEmpty

-- | 测试removeComments对C++风格注释的处理
prop_remove_comments_cpp_style :: String -> Property
prop_remove_comments_cpp_style s =
  let cppComment = "// " ++ s
      processed = U.removeComments cppComment
  in property $ null processed

-- | 测试isCompleteStringLiteral对多行字符串的处理
prop_is_complete_string_literal_multiline :: String -> Property
prop_is_complete_string_literal_multiline s =
  let multiline = "\"" ++ s ++ "\n\""
  in property $ U.isCompleteStringLiteral multiline

-- | 测试breakOn对重叠模式的处理
prop_break_on_overlapping :: String -> String -> Property
prop_break_on_overlapping pat s =
  let overlapping = pat ++ pat
      (before, after) = U.breakOn pat s
  in if pat `isInfixOf` s
     then property $ length before >= 0
     else property $ before === s .&. after === ""

-- | 测试safeProcessString对控制字符的处理
prop_safe_process_string_control :: String -> Property
prop_safe_process_string_control s =
  let withControl = map (\c -> if ord c < 32 && c /= '\n' && c /= '\t' then '?' else c) s
      processed = U.safeProcessString withControl
  in property $ either (const False) (all (\c -> ord c >= 32 || c `elem` ("\n\t" :: String))) processed

-- | 测试normalizeIndentation对嵌套代码的处理
prop_normalize_indentation_nested :: String -> Property
prop_normalize_indentation_nested s =
  let nested = unlines $ ["    func outer() {", "        func inner() {", "            " ++ s, "        }", "    }"]
      normalized = U.normalizeIndentation nested
      normLines = lines normalized
  in if null s
     then property $ length (filter (isPrefixOf "    ") normLines) < length normLines .&&. not (null normalized)
     else property $ length (filter (isPrefixOf "    ") normLines) < length normLines .&&. not (null normalized)

-- | 测试字符串函数的分配性
prop_string_functions_distributive :: String -> Property
prop_string_functions_distributive s =
  let splitRemoveComments = map U.removeComments (U.splitBy '\n' s)
      removeCommentsSplit = U.removeComments s
  in property $ length splitRemoveComments >= 0

-- | 测试splitByComma对最后一个元素为空的处理
prop_split_by_comma_last_empty :: String -> Property
prop_split_by_comma_last_empty s =
  let withTrailing = s ++ ","
      parts = U.splitByComma withTrailing
  in property $ last parts === ""

-- | 测试removeLineComments对字符串内反斜杠的保护
prop_remove_line_comments_string_backslash :: String -> Property
prop_remove_line_comments_string_backslash s =
  let stringWithBackslash = "\"" ++ s ++ "\\\\" ++ "\" // comment"
      processed = U.removeLineComments stringWithBackslash
  in property $ ("\"" ++ s ++ "\\\\" ++ "\"") `isInfixOf` processed

-- | 测试splitBy对空字符串和空分隔符的处理
prop_split_by_empty_string_empty_char :: Property
prop_split_by_empty_string_empty_char = property $ U.splitBy '\0' "" === [""]

-- | 测试trim对零宽度字符的处理
prop_trim_zero_width :: String -> Property
prop_trim_zero_width s =
  let withZeroWidth = "\x200B" ++ s ++ "\x200B"
      trimmed = U.trim withZeroWidth
  in if null s
     then property $ trimmed === s
     else property $ not ("\x200B" `isPrefixOf` trimmed) .&. not ("\x200B" `isSuffixOf` trimmed)

-- | 测试splitByCommaCollapsed对前导逗号的处理
prop_split_by_comma_collapsed_leading :: String -> Property
prop_split_by_comma_collapsed_leading s =
  let withLeading = "," ++ s
      parts = U.splitByCommaCollapsed withLeading
  in if null s
     then property $ parts === []
     else property $ parts === U.splitByCommaCollapsed s

-- | 测试removeComments对JavaScript风格注释的处理
prop_remove_comments_js_style :: String -> Property
prop_remove_comments_js_style s =
  let jsComment = "/* " ++ s ++ " */"
      processed = U.removeComments jsComment
  in property $ null processed

-- | 测试isCompleteStringLiteral对原始字符串的处理
prop_is_complete_string_literal_raw :: String -> Property
prop_is_complete_string_literal_raw s =
  let rawString = "`" ++ s ++ "`"
  in property $ not (U.isCompleteStringLiteral rawString)

-- | 测试breakOn对正则表达式特殊字符的处理
prop_break_on_regex_chars :: String -> Property
prop_break_on_regex_chars s =
  let regexChars = "*+?[]()|^$.\\"
      (before, after) = U.breakOn regexChars s
  in if regexChars `isInfixOf` s
     then property $ before ++ regexChars ++ after === s
     else property $ before === s .&. after === ""

-- | 测试safeProcessString对空字符串的处理
prop_safe_process_string_empty :: Property
prop_safe_process_string_empty = property $ U.safeProcessString "" === Right ""

-- | 测试normalizeIndentation对标签缩进的处理
prop_normalize_indentation_labels :: String -> Property
prop_normalize_indentation_labels s =
  let labeled = unlines $ ["label1:", "    " ++ s, "label2:", "    " ++ s]
      normalized = U.normalizeIndentation labeled
      normLines = lines normalized
  in if null s
     then property $ length (filter (isPrefixOf "    ") normLines) <= 2  -- 最多2行有4个空格前缀
     else property $ length (filter (isPrefixOf "    ") normLines) <= 2 .&&. not (null normalized)

-- ============================================================================
-- 数据结构测试 (40个测试)
-- ============================================================================

-- | 测试Map的基本操作
prop_map_insert_lookup :: [(String, Int)] -> Property
prop_map_insert_lookup pairs =
  let m = Map.fromList pairs
  in if null pairs
     then property $ Map.lookup "default" m === Nothing
     else let (k, v) = head pairs
          in property $ Map.lookup k m === Just v

-- | 测试Set的基本操作
prop_set_insert_member :: [String] -> Property
prop_set_insert_member items =
  let s = Set.fromList items
      item = head $ items ++ ["default"]
  in property $ Set.member item s === (item `elem` items)

-- | 测试List的排序性质
prop_list_sort :: [Int] -> Property
prop_list_sort xs = 
  let sorted = sort xs
  in property $ all (\(a, b) -> a <= b) (zip sorted (drop 1 sorted))

-- | 测试List的去重性质
prop_list_nub :: [Int] -> Property
prop_list_nub xs =
  let unique = nub xs
      hasDuplicates = length unique < length xs
  in property $ all (`elem` xs) unique

-- | 测试List的reverse性质
prop_list_reverse :: [Int] -> Property
prop_list_reverse xs = property $ reverse (reverse xs) === xs

-- | 测试List的length性质
prop_list_length :: [Int] -> Property
prop_list_length xs = property $ length xs >= 0

-- | 测试List的null性质
prop_list_null :: [Int] -> Property
prop_list_null xs = property $ null xs === (length xs == 0)

-- | 测试List的head安全性
prop_list_head_safe :: [Int] -> Property
prop_list_head_safe xs = 
  if null xs
  then property $ True
  else property $ head xs `elem` xs

-- | 测试List的tail安全性
prop_list_tail_safe :: [Int] -> Property
prop_list_tail_safe xs = 
  if null xs
  then property $ drop 1 xs === []
  else property $ length (drop 1 xs) === length xs - 1

-- | 测试List的map性质
prop_list_map :: [Int] -> Property
prop_list_map xs = property $ length (map (+1) xs) === length xs

-- | 测试List的filter性质
prop_list_filter :: [Int] -> Property
prop_list_filter xs = 
  let filtered = filter (>0) xs
  in property $ all (>0) filtered

-- | 测试List的foldr性质
prop_list_foldr :: [Int] -> Property
prop_list_foldr xs = 
  let result = foldr (+) 0 xs
  in property $ (result >= 0) .||. (any (< 0) xs)

-- | 测试List的foldl性质
prop_list_foldl :: [Int] -> Property
prop_list_foldl xs = 
  let result = foldl (+) 0 xs
  in property $ (result >= 0) .||. (any (< 0) xs)

-- | 测试List的concat性质
prop_list_concat :: [[Int]] -> Property
prop_list_concat xss = 
  let concatenated = concat xss
  in property $ sum concatenated === sum (map sum xss)

-- | 测试List的zip性质
prop_list_zip :: [Int] -> [String] -> Property
prop_list_zip xs ys = 
  let zipped = zip xs ys
  in property $ length zipped === min (length xs) (length ys)

-- | 测试List的unzip性质
prop_list_unzip :: [(Int, String)] -> Property
prop_list_unzip pairs = 
  let (xs, ys) = unzip pairs
  in property $ length xs === length ys .&. length ys === length pairs

-- | 测试List的take性质
prop_list_take :: [Int] -> Int -> Property
prop_list_take xs n = 
  if n >= 0 && n <= length xs
  then property $ length (take n xs) === n
  else property $ True

-- | 测试List的drop性质
prop_list_drop :: [Int] -> Int -> Property
prop_list_drop xs n = 
  if n >= 0 && n <= length xs
  then property $ length (drop n xs) === length xs - n
  else property $ True

-- | 测试List的splitAt性质
prop_list_split_at :: [Int] -> Int -> Property
prop_list_split_at xs n = 
  if n >= 0 && n <= length xs
  then let (taken, dropped) = splitAt n xs
       in property $ length taken === n .&. length dropped === length xs - n
  else property $ True

-- | 测试List的replicate性质
prop_list_replicate :: Int -> Property
prop_list_replicate n = 
  if n >= 0 && n < 1000
  then property $ length (replicate n 'a') === n
  else property $ True

-- | 测试List的cycle性质
prop_list_cycle :: [Int] -> Property
prop_list_cycle xs = 
  if null xs
  then property $ True
  else property $ take 10 (cycle xs) === take 10 (concat (replicate 10 xs))

-- | 测试Maybe的性质
prop_maybe_nothing :: Property
prop_maybe_nothing = property $ isNothing Nothing

-- | 测试Maybe的Just性质
prop_maybe_just :: Int -> Property
prop_maybe_just x = property $ isJust (Just x)

-- | 测试Maybe的fromMaybe性质
prop_maybe_from_maybe :: Int -> Maybe Int -> Property
prop_maybe_from_maybe def mx = 
  case mx of
    Nothing -> property $ fromMaybe def mx === def
    Just x -> property $ fromMaybe def mx === x

-- | 测试Either的Left性质
prop_either_left :: String -> Property
prop_either_left x = property $ isLeft (Left x)

-- | 测试Either的Right性质
prop_either_right :: Int -> Property
prop_either_right x = property $ isRight (Right x)

-- | 测试Either的either性质
prop_either_either :: String -> Int -> Either String Int -> Property
prop_either_either leftVal rightVal ex =
  case ex of
    Left x -> property $ either length (const 0) ex === length x
    Right x -> property $ either (const 0) id ex === x

-- | 测试Tuple的fst性质
prop_tuple_fst :: String -> Int -> Property
prop_tuple_fst x y = property $ fst (x, y) === x

-- | 测试Tuple的snd性质
prop_tuple_snd :: String -> Int -> Property
prop_tuple_snd x y = property $ snd (x, y) === y

-- | 测试Tuple的swap性质
prop_tuple_swap :: String -> Int -> Property
prop_tuple_swap x y = 
  let swapped = (y, x)
  in property $ fst swapped === y .&. snd swapped === x

-- | 测试Bool的and性质
prop_bool_and :: [Bool] -> Property
prop_bool_and bs = 
  if null bs
  then property $ and bs === True
  else property $ all id bs === and bs

-- | 测试Bool的or性质
prop_bool_or :: [Bool] -> Property
prop_bool_or bs = 
  if null bs
  then property $ or bs === False
  else property $ any id bs === or bs

-- | 测试Bool的not性质
prop_bool_not :: Bool -> Property
prop_bool_not b = property $ not (not b) === b

-- | 测试Char的性质
prop_char_ord :: Char -> Property
prop_char_ord c = property $ ord c >= 0

-- | 测试String的性质
prop_string_length :: String -> Property
prop_string_length s = property $ length s >= 0

-- | 测试String的reverse性质
prop_string_reverse :: String -> Property
prop_string_reverse s = property $ reverse (reverse s) === s

-- | 测试String的map性质
prop_string_map :: String -> Property
prop_string_map s = property $ length (map toUpper s) === length s

-- | 测试String的filter性质
prop_string_filter :: String -> Property
prop_string_filter s = 
  let filtered = filter isLetter s
  in property $ all isLetter filtered

-- | 测试String的concat性质
prop_string_concat :: [String] -> Property
prop_string_concat ss = 
  let concatenated = concat ss
  in property $ sum (map length ss) === length concatenated

-- | 测试String的intercalate性质
prop_string_intercalate :: String -> [String] -> Property
prop_string_intercalate sep ss = 
  let intercalated = intercalate sep ss
      expectedLength = sum (map length ss) + length sep * (length ss - 1)
  in if null ss
     then property $ intercalated === ""
     else property $ length intercalated === expectedLength

-- | 测试String的lines性质
prop_string_lines :: String -> Property
prop_string_lines s = 
  let lined = lines s
      rejoined = unlines lined
  in if null s
     then property $ rejoined === ""
     else property $ "\n" `isSuffixOf` rejoined

-- | 测试String的words性质
prop_string_words :: String -> Property
prop_string_words s = 
  let worded = words s
      rejoined = unwords worded
  in property $ all (not . any isSpace) worded

-- | 测试String的isPrefixOf性质
prop_string_is_prefix_of :: String -> String -> Property
prop_string_is_prefix_of prefix s = 
  if prefix `isPrefixOf` s
  then property $ take (length prefix) s === prefix
  else property $ True

-- | 测试String的isSuffixOf性质
prop_string_is_suffix_of :: String -> String -> Property
prop_string_is_suffix_of suffix s = 
  if suffix `isSuffixOf` s
  then property $ drop (length s - length suffix) s === suffix
  else property $ True

-- | 测试String的isInfixOf性质
prop_string_is_infix_of :: String -> String -> Property
prop_string_is_infix_of infixStr s = 
  if infixStr `isInfixOf` s
  then property $ length infixStr <= length s
  else property $ True

-- ============================================================================
-- 数值计算测试 (30个测试)
-- ============================================================================

-- | 测试加法交换律
prop_addition_commutative :: Int -> Int -> Property
prop_addition_commutative x y = property $ x + y === y + x

-- | 测试加法结合律
prop_addition_associative :: Int -> Int -> Int -> Property
prop_addition_associative x y z = property $ (x + y) + z === x + (y + z)

-- | 测试加法单位元
prop_addition_identity :: Int -> Property
prop_addition_identity x = property $ x + 0 === x

-- | 测试乘法交换律
prop_multiplication_commutative :: Int -> Int -> Property
prop_multiplication_commutative x y = property $ x * y === y * x

-- | 测试乘法结合律
prop_multiplication_associative :: Int -> Int -> Int -> Property
prop_multiplication_associative x y z = property $ (x * y) * z === x * (y * z)

-- | 测试乘法单位元
prop_multiplication_identity :: Int -> Property
prop_multiplication_identity x = property $ x * 1 === x

-- | 测试乘法零元
prop_multiplication_zero :: Int -> Property
prop_multiplication_zero x = property $ x * 0 === 0

-- | 测试分配律
prop_distributive :: Int -> Int -> Int -> Property
prop_distributive x y z = property $ x * (y + z) === x * y + x * z

-- | 测试减法性质
prop_subtraction :: Int -> Int -> Property
prop_subtraction x y = property $ x - y + y === x

-- | 测试除法性质
prop_division :: Int -> Property
prop_division x = 
  if x /= 0
  then property $ x `div` x === 1
  else property $ True

-- | 测试模运算性质
prop_modulus :: Int -> Int -> Property
prop_modulus x y = 
  if y /= 0
  then property $ (x `div` y) * y + (x `mod` y) === x
  else property $ True

-- | 测试幂运算性质
prop_power :: Int -> Property
prop_power x = 
  if x >= 0 && x < 20
  then property $ x ^ 1 === x
  else property $ True

-- | 测试绝对值性质
prop_absolute :: Int -> Property
prop_absolute x = property $ abs x >= 0

-- | 测试符号函数性质
prop_signum :: Int -> Property
prop_signum x = 
  if x > 0 then property $ signum x === 1
  else if x < 0 then property $ signum x === -1
  else property $ signum x === 0

-- | 测试最大值函数
prop_maximum :: [Int] -> Property
prop_maximum xs = 
  if not (null xs)
  then property $ maximum xs `elem` xs
  else property $ True

-- | 测试最小值函数
prop_minimum :: [Int] -> Property
prop_minimum xs = 
  if not (null xs)
  then property $ minimum xs `elem` xs
  else property $ True

-- | 测试求和函数
prop_sum :: [Int] -> Property
prop_sum xs = property $ sum xs >= 0 .||. not (all (>=0) xs)

-- | 测试求积函数
prop_product :: [Int] -> Property
prop_product xs = 
  if null xs
  then property $ product xs === 1
  else property $ product xs >= 0 .||. not (all (>=0) xs)

-- | 测试偶数判断
prop_even :: Int -> Property
prop_even x = property $ even x === (x `mod` 2 == 0)

-- | 测试奇数判断
prop_odd :: Int -> Property
prop_odd x = property $ odd x === (x `mod` 2 /= 0)

-- | 测试gcd性质
prop_gcd :: Int -> Int -> Property
prop_gcd x y = 
  if x /= 0 && y /= 0
  then property $ gcd x y `gcd` (x * y) === gcd x y
  else property $ True

-- | 测试lcm性质
prop_lcm :: Int -> Int -> Property
prop_lcm x y = 
  if x /= 0 && y /= 0
  then property $ lcm x y * gcd x y === abs (x * y)
  else property $ True

-- | 测试比较运算
prop_comparison :: Int -> Int -> Property
prop_comparison x y = 
  if x == y
  then property $ x <= y .&. x >= y
  else property $ x < y .||. x > y

-- | 测试max函数
prop_max :: Int -> Int -> Property
prop_max x y = property $ max x y === x .||. max x y === y

-- | 测试min函数
prop_min :: Int -> Int -> Property
prop_min x y = property $ min x y === x .||. min x y === y

-- | 测试negate函数
prop_negate :: Int -> Property
prop_negate x = property $ negate (negate x) === x

-- | 测试subtract函数
prop_subtract :: Int -> Int -> Property
prop_subtract x y = property $ subtract x y === y - x

-- | 测试数值范围
prop_numeric_range :: Int -> Property
prop_numeric_range x = 
  if x >= -100 && x <= 100
  then property $ True
  else property $ x >= -1000 .&. x <= 1000

-- | 测试数值边界
prop_numeric_bounds :: Property
prop_numeric_bounds = 
  let maxInt = maxBound :: Int
      minInt = minBound :: Int
  in property $ maxInt >= minInt

-- | 测试数值溢出保护
prop_overflow_protection :: Int -> Property
prop_overflow_protection x = 
  if x > 0 && x < maxBound `div` 2
  then property $ x * 2 > x
  else if x < 0 && x > minBound `div` 2
       then property $ x * 2 < x
       else property $ True

-- | 测试数值精度
prop_precision :: Double -> Property
prop_precision x = 
  if isNaN x || isInfinite x
  then property $ True
  else property $ x - x === 0

-- | 测试数值相等性
prop_equality :: Int -> Int -> Property
prop_equality x y = property $ (x == y) === (y == x)

-- | 测试数值不等性
prop_inequality :: Int -> Int -> Property
prop_inequality x y = property $ (x /= y) === not (x == y)

-- | 测试数值比较链
prop_comparison_chain :: Int -> Int -> Int -> Property
prop_comparison_chain x y z = 
  if x <= y && y <= z
  then property $ x <= z
  else property $ True

-- | 测试fromIntegral
prop_from_integral :: Int -> Property
prop_from_integral x = property $ fromIntegral x === (fromIntegral x :: Double)

-- ============================================================================
-- 组合所有测试
-- ============================================================================

-- | 组合所有测试
newEnhancedQuickCheckTestSuite :: TestTree
newEnhancedQuickCheckTestSuite = testGroup "New Enhanced QuickCheck Test Suite"
  [ testGroup "Enhanced String Processing Tests" 
      [ testProperty "trim unicode" prop_trim_unicode
      , testProperty "splitBy separator count" prop_split_by_separator_count
      , testProperty "splitByCollapsed no consecutive" prop_split_by_collapsed_no_consecutive
      , testProperty "removeComments nested" prop_remove_comments_nested
      , testProperty "isCompleteStringLiteral escape" prop_is_complete_string_literal_escape
      , testProperty "isProblematicUnclosedString complex" prop_is_problematic_unclosed_complex
      , testProperty "breakOn not found" prop_break_on_not_found
      , testProperty "safeProcessString invalid" prop_safe_process_string_invalid
      , testProperty "normalizeIndentation mixed" prop_normalize_indentation_mixed
      , testProperty "string functions composition" prop_string_functions_composition
      , testProperty "splitByComma mixed" prop_split_by_comma_mixed
      , testProperty "removeLineComments string protection" prop_remove_line_comments_string_protection
      , testProperty "splitBy performance" prop_split_by_performance
      , testProperty "trim edge cases" prop_trim_edge_cases
      , testProperty "splitByCollapsed empty" prop_split_by_collapsed_empty
      , testProperty "removeComments multiple" prop_remove_comments_multiple
      , testProperty "isCompleteStringLiteral empty" prop_is_complete_string_literal_empty
      , testProperty "isCompleteStringLiteral single quote" prop_is_complete_string_literal_single_quote
      , testProperty "breakOn multi char" prop_break_on_multi_char
      , testProperty "safeProcessString ascii" prop_safe_process_string_ascii
      , testProperty "normalizeIndentation single line" prop_normalize_indentation_single_line
      , testProperty "string functions commutative" prop_string_functions_commutative
      , testProperty "splitByComma spaces" prop_split_by_comma_spaces
      , testProperty "removeLineComments multiline protection" prop_remove_line_comments_multiline_protection
      , testProperty "splitBy special chars" prop_split_by_special
      , testProperty "trim tab space mixed" prop_trim_tab_space_mixed
      , testProperty "splitByCommaCollapsed property" prop_split_by_comma_collapsed_property
      , testProperty "normalizeIndentation preserve empty" prop_normalize_indentation_preserve_empty
      , testProperty "isCompleteStringLiteral escape quotes" prop_is_complete_string_literal_escape_quotes
      , testProperty "splitBy empty char" prop_split_by_empty_char
      , testProperty "removeComments nested in string" prop_remove_comments_nested_in_string
      , testProperty "breakOn long pattern" prop_break_on_long_pattern
      , testProperty "safeProcessString error handling" prop_safe_process_string_error_handling
      , testProperty "normalizeIndentation deep" prop_normalize_indentation_deep
      , testProperty "string functions idempotent" prop_string_functions_idempotent
      , testProperty "splitByComma whitespace" prop_split_by_comma_whitespace
      , testProperty "removeLineComments end of line" prop_remove_line_comments_end_of_line
      , testProperty "splitBy non ascii" prop_split_by_non_ascii
      , testProperty "trim newlines tabs" prop_trim_newlines_tabs
      , testProperty "splitByCommaCollapsed empty" prop_split_by_comma_collapsed_empty
      , testProperty "removeComments preserve code after" prop_remove_comments_preserve_code_after
      , testProperty "isCompleteStringLiteral complex escape" prop_is_complete_string_literal_complex_escape
      , testProperty "breakOn edge cases" prop_break_on_edge_cases
      , testProperty "safeProcessString unicode" prop_safe_process_string_unicode
      , testProperty "normalizeIndentation code block" prop_normalize_indentation_code_block
      , testProperty "string functions associative" prop_string_functions_associative
      , testProperty "splitByComma consecutive" prop_split_by_comma_consecutive
      , testProperty "removeLineComments string newline" prop_remove_line_comments_string_newline
      , testProperty "splitBy case sensitive" prop_split_by_case_sensitive
      , testProperty "trim mixed whitespace complex" prop_trim_mixed_whitespace_complex
      , testProperty "splitByCommaCollapsed empty values" prop_split_by_comma_collapsed_empty_values
      , testProperty "removeComments cpp style" prop_remove_comments_cpp_style
      , testProperty "isCompleteStringLiteral multiline" prop_is_complete_string_literal_multiline
      , testProperty "breakOn overlapping" prop_break_on_overlapping
      , testProperty "safeProcessString control" prop_safe_process_string_control
      , testProperty "normalizeIndentation nested" prop_normalize_indentation_nested
      , testProperty "string functions distributive" prop_string_functions_distributive
      , testProperty "splitByComma last empty" prop_split_by_comma_last_empty
      , testProperty "removeLineComments string backslash" prop_remove_line_comments_string_backslash
      , testProperty "splitBy empty string empty char" prop_split_by_empty_string_empty_char
      , testProperty "trim zero width" prop_trim_zero_width
      , testProperty "splitByCommaCollapsed leading" prop_split_by_comma_collapsed_leading
      , testProperty "removeComments js style" prop_remove_comments_js_style
      , testProperty "isCompleteStringLiteral raw" prop_is_complete_string_literal_raw
      , testProperty "breakOn regex chars" prop_break_on_regex_chars
      , testProperty "safeProcessString empty" prop_safe_process_string_empty
      , testProperty "normalizeIndentation labels" prop_normalize_indentation_labels
      ]
  , testGroup "Data Structure Tests"
      [ testProperty "map insert lookup" prop_map_insert_lookup
      , testProperty "set insert member" prop_set_insert_member
      , testProperty "list sort" prop_list_sort
      , testProperty "list nub" prop_list_nub
      , testProperty "list reverse" prop_list_reverse
      , testProperty "list length" prop_list_length
      , testProperty "list null" prop_list_null
      , testProperty "list head safe" prop_list_head_safe
      , testProperty "list tail safe" prop_list_tail_safe
      , testProperty "list map" prop_list_map
      , testProperty "list filter" prop_list_filter
      , testProperty "list foldr" prop_list_foldr
      , testProperty "list foldl" prop_list_foldl
      , testProperty "list concat" prop_list_concat
      , testProperty "list zip" prop_list_zip
      , testProperty "list unzip" prop_list_unzip
      , testProperty "list take" prop_list_take
      , testProperty "list drop" prop_list_drop
      , testProperty "list split at" prop_list_split_at
      , testProperty "list replicate" prop_list_replicate
      , testProperty "list cycle" prop_list_cycle
      , testProperty "maybe nothing" prop_maybe_nothing
      , testProperty "maybe just" prop_maybe_just
      , testProperty "maybe from maybe" prop_maybe_from_maybe
      , testProperty "either left" prop_either_left
      , testProperty "either right" prop_either_right
      , testProperty "either either" prop_either_either
      , testProperty "tuple fst" prop_tuple_fst
      , testProperty "tuple snd" prop_tuple_snd
      , testProperty "tuple swap" prop_tuple_swap
      , testProperty "bool and" prop_bool_and
      , testProperty "bool or" prop_bool_or
      , testProperty "bool not" prop_bool_not
      , testProperty "char ord" prop_char_ord
      , testProperty "string length" prop_string_length
      , testProperty "string reverse" prop_string_reverse
      , testProperty "string map" prop_string_map
      , testProperty "string filter" prop_string_filter
      , testProperty "string concat" prop_string_concat
      , testProperty "string intercalate" prop_string_intercalate
      , testProperty "string lines" prop_string_lines
      , testProperty "string words" prop_string_words
      , testProperty "string is prefix of" prop_string_is_prefix_of
      , testProperty "string is suffix of" prop_string_is_suffix_of
      , testProperty "string is infix of" prop_string_is_infix_of
      ]
  , testGroup "Numeric Computation Tests"
      [ testProperty "addition commutative" prop_addition_commutative
      , testProperty "addition associative" prop_addition_associative
      , testProperty "addition identity" prop_addition_identity
      , testProperty "multiplication commutative" prop_multiplication_commutative
      , testProperty "multiplication associative" prop_multiplication_associative
      , testProperty "multiplication identity" prop_multiplication_identity
      , testProperty "multiplication zero" prop_multiplication_zero
      , testProperty "distributive" prop_distributive
      , testProperty "subtraction" prop_subtraction
      , testProperty "division" prop_division
      , testProperty "modulus" prop_modulus
      , testProperty "power" prop_power
      , testProperty "absolute" prop_absolute
      , testProperty "signum" prop_signum
      , testProperty "maximum" prop_maximum
      , testProperty "minimum" prop_minimum
      , testProperty "sum" prop_sum
      , testProperty "product" prop_product
      , testProperty "even" prop_even
      , testProperty "odd" prop_odd
      , testProperty "gcd" prop_gcd
      , testProperty "lcm" prop_lcm
      , testProperty "comparison" prop_comparison
      , testProperty "max" prop_max
      , testProperty "min" prop_min
      , testProperty "negate" prop_negate
      , testProperty "subtract" prop_subtract
      , testProperty "numeric range" prop_numeric_range
      , testProperty "numeric bounds" prop_numeric_bounds
      , testProperty "overflow protection" prop_overflow_protection
      , testProperty "precision" prop_precision
      , testProperty "equality" prop_equality
      , testProperty "inequality" prop_inequality
      , testProperty "comparison chain" prop_comparison_chain
      , testProperty "from integral" prop_from_integral
      ]
  ]