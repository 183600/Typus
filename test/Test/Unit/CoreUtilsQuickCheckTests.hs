{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Unit.CoreUtilsQuickCheckTests where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import qualified Utils as U
import Data.List (isInfixOf, isPrefixOf, isSuffixOf)
import Data.Char (isSpace, isLetter, isDigit)
import Data.Maybe (isJust, isNothing)
import Data.Either (isLeft, isRight)
import Control.Arrow ((***))

-- | 测试trim函数的幂等性
prop_trim_idempotent :: String -> Property
prop_trim_idempotent s = U.trim (U.trim s) === U.trim s

-- | 测试trim不会增加字符串长度
prop_trim_never_increases :: String -> Property
prop_trim_never_increases s = 
  let trimmed = U.trim s
  in property $ length trimmed <= length s

-- | 测试trim对全空白字符串的处理
prop_trim_all_whitespace :: String -> Property
prop_trim_all_whitespace s =
  let wsOnly = filter isSpace s
  in property $ null (U.trim wsOnly)

-- | 测试splitBy的基本属性
prop_split_by_length :: Char -> String -> Property
prop_split_by_length c s =
  let parts = U.splitBy c s
      rejoined = intercalate [c] parts
  in if null s 
     then property $ null parts
     else property $ rejoined === s

-- | 测试splitByComma与splitBy的一致性
prop_split_by_comma_consistency :: String -> Property
prop_split_by_comma_consistency s = 
  U.splitBy ',' s === U.splitByComma s

-- | 测试splitByCollapsed的折叠属性
prop_split_by_collapsed_fold :: Char -> String -> Property
prop_split_by_collapsed_fold c s =
  let collapsed = U.splitByCollapsed c s
      hasNoConsecutive = all (not . isInfixOf [c,c]) collapsed
  in property hasNoConsecutive

-- | 测试removeLineComments不影响字符串字面量
prop_remove_line_comments_preserves_strings :: String -> Property
prop_remove_line_comments_preserves_strings s =
  let withQuote = "\"" ++ s ++ "\""
      after = U.removeLineComments withQuote
  in property $ "\"" `isPrefixOf` after && "\"" `isSuffixOf` after

-- | 测试removeComments的平衡性
prop_remove_comments_balanced :: String -> Property
prop_remove_comments_balanced s =
  let withBlock = "/*" ++ s ++ "*/"
      after = U.removeComments withBlock
  in property $ not ("/*" `isInfixOf` after) && not ("*/" `isInfixOf` after)

-- | 测试isCompleteStringLiteral的识别能力
prop_is_complete_string_literal :: String -> Property
prop_is_complete_string_literal s =
  let quoted = "\"" ++ s ++ "\""
      incomplete = "\"" ++ s
  in property $ U.isCompleteStringLiteral quoted && not (U.isCompleteStringLiteral incomplete)

-- | 测试normalizeIndentation的相对性
prop_normalize_indentation_relative :: String -> Property
prop_normalize_indentation_relative s =
  let lines' = lines s
      normalized = U.normalizeIndentation s
      normLines = lines normalized
  in if length lines' <= 1
     then property $ normalized === s
     else property $ length normLines === length lines'

-- | 测试breakOn的正确性
prop_break_on_correctness :: String -> String -> Property
prop_break_on_correctness pat s =
  let (before, after) = U.breakOn pat s
      combined = before ++ pat ++ after
  in if pat `isInfixOf` s
     then property $ combined === s
     else property $ before === s && after === ""

-- | 测试safeProcessString的安全性
prop_safe_process_string_safe :: String -> Property
prop_safe_process_string_safe s =
  let processed = U.safeProcessString s
      allValid = all U.isValidChar processed
  in property allValid

-- | 测试isValidChar的属性
prop_is_valid_char_ascii :: Char -> Property
prop_is_valid_char_ascii c =
  let ascii = ord c < 128
  in property $ if ascii then U.isValidChar c else True

-- | 测试trim与splitBy的交互
prop_trim_split_interaction :: Char -> String -> Property
prop_trim_split_interaction c s =
  let parts = U.splitBy c s
      trimmedParts = map U.trim parts
  in property $ length parts === length trimmedParts

-- | 测试removeComments的幂等性
prop_remove_comments_idempotent :: String -> Property
prop_remove_comments_idempotent s =
  let first = U.removeComments s
      second = U.removeComments first
  in property $ first === second

-- | 测试splitByCommaCollapsed的属性
prop_split_by_comma_collapsed :: String -> Property
prop_split_by_comma_collapsed s =
  let parts = U.splitByCommaCollapsed s
      noEmpty = filter (not . null) parts
  in property $ noEmpty === parts

-- | 测试normalizeIndentation保持非空行
prop_normalize_indentation_preserves_nonempty :: String -> Property
prop_normalize_indentation_preserves_nonempty s =
  let lines' = lines s
      nonEmpty = filter (not . all isSpace) lines'
      normalized = U.normalizeIndentation s
      normLines = lines normalized
      normNonEmpty = filter (not . all isSpace) normLines
  in property $ length nonEmpty === length normNonEmpty

-- | 测试isProblematicUnclosedString的识别
prop_is_problematic_unclosed_string :: String -> Property
prop_is_problematic_unclosed_string s =
  let closed = "\"" ++ s ++ "\""
      unclosed = "\"" ++ s
      withEscaped = "\"" ++ s ++ "\\\""
  in property $ not (U.isProblematicUnclosedString closed) && 
                U.isProblematicUnclosedString unclosed &&
                U.isCompleteStringLiteral withEscaped

-- | 测试removeLineComments处理多行
prop_remove_line_comments_multiline :: [String] -> Property
prop_remove_line_comments_multiline lines' =
  let code = unlines lines'
      processed = U.removeLineComments code
      procLines = lines processed
  in property $ length procLines === length lines'

-- | 测试splitBy对空字符串的处理
prop_split_by_empty :: Char -> Property
prop_split_by_empty c = U.splitBy c "" === []

-- | 测试splitBy对单字符分隔符的处理
prop_split_by_single_char :: Char -> Char -> Property
prop_split_by_single_char c1 c2 =
  let s = [c1, c2, c1]
      parts = U.splitBy c1 s
  in property $ parts === ["", [c2], ""]

-- | 测试trim对制表符和空格的处理
prop_trim_tab_space :: String -> Property
prop_trim_tab_space s =
  let withTabs = "\t" ++ s ++ "\t"
      withSpaces = " " ++ s ++ " "
      trimmedTabs = U.trim withTabs
      trimmedSpaces = U.trim withSpaces
  in property $ trimmedTabs === trimmedSpaces

-- | 测试removeComments处理嵌套注释
prop_remove_comments_nested :: String -> Property
prop_remove_comments_nested s =
  let nested = "/* outer /* inner */ */" ++ s
      processed = U.removeComments nested
  in property $ not ("/*" `isInfixOf` processed)

-- | 测试normalizeIndentation对混合缩进的处理
prop_normalize_indentation_mixed :: String -> Property
prop_normalize_indentation_mixed s =
  let mixed = "  \t  " ++ s
      normalized = U.normalizeIndentation mixed
  in property $ not ("\t" `isInfixOf` normalized)

-- | 测试safeProcessString对Unicode的处理
prop_safe_process_string_unicode :: String -> Property
prop_safe_process_string_unicode s =
  let unicode = s ++ "ñáéíóú"
      processed = U.safeProcessString unicode
  in property $ length processed >= length s

-- | 测试isRight函数的属性
prop_is_right_property :: Either String Int -> Property
prop_is_right_property e = property $ U.isRight e === isRight e

-- | 测试splitByComma对数字的处理
prop_split_by_comma_numbers :: [Int] -> Property
prop_split_by_comma_numbers nums =
  let str = intercalate "," (map show nums)
      parts = U.splitByComma str
  in property $ length parts === length nums

-- | 测试trim与字符串连接的交互
prop_trim_concat_interaction :: String -> String -> Property
prop_trim_concat_interaction s1 s2 =
  let trimmed1 = U.trim s1
      trimmed2 = U.trim s2
      concatenated = trimmed1 ++ " " ++ trimmed2
      trimmedConcat = U.trim concatenated
  in property $ not (null trimmedConcat) || (null trimmed1 && null trimmed2)

-- | 测试removeLineComments对行尾注释的处理
prop_remove_line_comments_end :: String -> Property
prop_remove_line_comments_end s =
  let withComment = s ++ "// comment"
      processed = U.removeLineComments withComment
  in property $ processed === s

-- | 测试normalizeIndentation对空行的处理
prop_normalize_indentation_empty_lines :: String -> Property
prop_normalize_indentation_empty_lines s =
  let withEmpty = s ++ "\n\n"
      normalized = U.normalizeIndentation withEmpty
  in property $ "\n\n" `isInfixOf` normalized

-- | 测试breakOn对空模式的处理
prop_break_on_empty :: String -> Property
prop_break_on_empty s = U.breakOn "" s === ("", s)

-- | 测试safeProcessString对控制字符的处理
prop_safe_process_string_control :: String -> Property
prop_safe_process_string_control s =
  let withControl = s ++ "\x01\x02"
      processed = U.safeProcessString withControl
  in property $ not (any (< '\x20') processed)

-- | 测试isCompleteStringLiteral对转义引号的处理
prop_is_complete_string_literal_escaped :: String -> Property
prop_is_complete_string_literal_escaped s =
  let escaped = "\"" ++ s ++ "\\\"\""
  in property $ U.isCompleteStringLiteral escaped

-- | 测试splitBy对连续分隔符的处理
prop_split_by_consecutive :: Char -> Int -> Property
prop_split_by_consecutive c n =
  let separators = replicate n c
      parts = U.splitBy c separators
  in property $ length parts === n + 1

-- | 测试removeComments对单行注释的处理
prop_remove_comments_single_line :: String -> Property
prop_remove_comments_single_line s =
  let withSingle = "//" ++ s
      processed = U.removeComments withSingle
  in property $ null processed

-- | 测试normalizeIndentation对制表符的处理
prop_normalize_indentation_tabs :: String -> Property
prop_normalize_indentation_tabs s =
  let withTabs = "\t\t" ++ s ++ "\t"
      normalized = U.normalizeIndentation withTabs
  in property $ not ("\t\t" `isPrefixOf` normalized)

-- | 测试trim对换行符的处理
prop_trim_newlines :: String -> Property
prop_trim_newlines s =
  let withNewlines = "\n" ++ s ++ "\n"
      trimmed = U.trim withNewlines
  in property $ not ("\n" `isPrefixOf` trimmed) && not ("\n" `isSuffixOf` trimmed)

-- | 测试splitByComma对空字符串的处理
prop_split_by_comma_empty :: Property
prop_split_by_comma_empty = U.splitByComma "" === []

-- | 测试removeLineComments对多行注释的影响
prop_remove_line_comments_multiline_block :: String -> Property
prop_remove_line_comments_multiline_block s =
  let withBlock = s ++ "\n/* comment */\n" ++ s
      processed = U.removeLineComments withBlock
  in property $ "/* comment */" `isInfixOf` processed

-- | 测试isProblematicUnclosedString对转义字符的处理
prop_is_problematic_unclosed_escaped :: String -> Property
prop_is_problematic_unclosed_escaped s =
  let withEscaped = "\"" ++ s ++ "\\"
  in property $ U.isProblematicUnclosedString withEscaped

-- | 测试normalizeIndentation对空字符串的处理
prop_normalize_indentation_empty :: Property
prop_normalize_indentation_empty = U.normalizeIndentation "" === ""

-- | 测试splitBy对特殊字符的处理
prop_split_by_special :: String -> Property
prop_split_by_special s =
  let parts = U.splitBy '\n' s
  in property $ concat parts ++ replicate (length parts - 1) '\n' === s

-- | 测试removeComments对字符串字面量中注释的保护
prop_remove_comments_protect_strings :: String -> Property
prop_remove_comments_protect_strings s =
  let withString = "code /* not comment */ \"" ++ s ++ "/* not comment */\" code"
      processed = U.removeComments withString
  in property $ s `isInfixOf` processed

-- | 测试safeProcessString对空字符串的处理
prop_safe_process_string_empty :: Property
prop_safe_process_string_empty = U.safeProcessString "" === ""

-- | 测试isCompleteStringLiteral对空字符串字面量的处理
prop_is_complete_string_literal_empty :: Property
prop_is_complete_string_literal_empty = U.isCompleteStringLiteral "\"\""

-- | 测试trim对混合空白字符的处理
prop_trim_mixed_whitespace :: String -> Property
prop_trim_mixed_whitespace s =
  let mixed = " \t\n " ++ s ++ " \t\n "
      trimmed = U.trim mixed
  in property $ not (any isSpace (take 1 trimmed)) && 
                not (any isSpace (take 1 (reverse trimmed)))

-- | 组合所有测试
coreUtilsTests :: TestTree
coreUtilsTests = testGroup "Core Utils QuickCheck Tests"
  [ testProperty "trim idempotent" prop_trim_idempotent
  , testProperty "trim never increases length" prop_trim_never_increases
  , testProperty "trim all whitespace" prop_trim_all_whitespace
  , testProperty "splitBy length" prop_split_by_length
  , testProperty "splitBy comma consistency" prop_split_by_comma_consistency
  , testProperty "splitBy collapsed fold" prop_split_by_collapsed_fold
  , testProperty "remove line comments preserves strings" prop_remove_line_comments_preserves_strings
  , testProperty "remove comments balanced" prop_remove_comments_balanced
  , testProperty "is complete string literal" prop_is_complete_string_literal
  , testProperty "normalize indentation relative" prop_normalize_indentation_relative
  , testProperty "breakOn correctness" prop_break_on_correctness
  , testProperty "safe process string safe" prop_safe_process_string_safe
  , testProperty "is valid char ascii" prop_is_valid_char_ascii
  , testProperty "trim split interaction" prop_trim_split_interaction
  , testProperty "remove comments idempotent" prop_remove_comments_idempotent
  , testProperty "splitBy comma collapsed" prop_split_by_comma_collapsed
  , testProperty "normalize indentation preserves nonempty" prop_normalize_indentation_preserves_nonempty
  , testProperty "is problematic unclosed string" prop_is_problematic_unclosed_string
  , testProperty "remove line comments multiline" prop_remove_line_comments_multiline
  , testProperty "splitBy empty" prop_split_by_empty
  , testProperty "splitBy single char" prop_split_by_single_char
  , testProperty "trim tab space" prop_trim_tab_space
  , testProperty "remove comments nested" prop_remove_comments_nested
  , testProperty "normalize indentation mixed" prop_normalize_indentation_mixed
  , testProperty "safe process string unicode" prop_safe_process_string_unicode
  , testProperty "isRight property" prop_is_right_property
  , testProperty "splitBy comma numbers" prop_split_by_comma_numbers
  , testProperty "trim concat interaction" prop_trim_concat_interaction
  , testProperty "remove line comments end" prop_remove_line_comments_end
  , testProperty "normalize indentation empty lines" prop_normalize_indentation_empty_lines
  , testProperty "breakOn empty" prop_break_on_empty
  , testProperty "safe process string control" prop_safe_process_string_control
  , testProperty "is complete string literal escaped" prop_is_complete_string_literal_escaped
  , testProperty "splitBy consecutive" prop_split_by_consecutive
  , testProperty "remove comments single line" prop_remove_comments_single_line
  , testProperty "normalize indentation tabs" prop_normalize_indentation_tabs
  , testProperty "trim newlines" prop_trim_newlines
  , testProperty "splitBy comma empty" prop_split_by_comma_empty
  , testProperty "remove line comments multiline block" prop_remove_line_comments_multiline_block
  , testProperty "is problematic unclosed escaped" prop_is_problematic_unclosed_escaped
  , testProperty "normalize indentation empty" prop_normalize_indentation_empty
  , testProperty "splitBy special" prop_split_by_special
  , testProperty "remove comments protect strings" prop_remove_comments_protect_strings
  , testProperty "safe process string empty" prop_safe_process_string_empty
  , testProperty "is complete string literal empty" prop_is_complete_string_literal_empty
  , testProperty "trim mixed whitespace" prop_trim_mixed_whitespace
  ]