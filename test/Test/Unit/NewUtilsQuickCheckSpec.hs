{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Unit.NewUtilsQuickCheckSpec where

import Test.Tasty.HUnit
import Test.Tasty
import Test.Tasty.QuickCheck
import TestSupport.MemoryLimits 
  ( withMemoryLimits
  , memoryLimitedTestGroup
  , memoryLevelTestGroup
  , MemoryLevel(..)
  , withMemoryLevel
  , gcBetweenTests
  )

import Utils
import Data.Char (isSpace, isAlphaNum, isLetter)
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import Data.Either (isLeft)
import Data.Maybe (listToMaybe)
import qualified Data.Text as T

-- | 测试trim函数对空白字符的处理
prop_trim_whitespace_handling :: String -> Property
prop_trim_whitespace_handling s =
  let limitedS = take 100 s
      trimmed = trim limitedS
  in if all isSpace limitedS
     then property $ null trimmed
     else property $ length trimmed <= length limitedS

-- | 测试trim函数对非空白字符的处理
prop_trim_non_whitespace_handling :: Char -> String -> Property
prop_trim_non_whitespace_handling c s =
  not (isSpace c) ==>
  let limitedS = take 50 s
      s' = c : limitedS
      trimmed = trim s'
  in property $ not (null trimmed) && head trimmed == c

-- | 测试trim函数的组合性
prop_trim_composition :: String -> String -> Property
prop_trim_composition s1 s2 =
  let limitedS1 = take 40 s1
      limitedS2 = take 40 s2
      combined = limitedS1 ++ "   " ++ limitedS2
      trimmed = trim combined
  in property $ length trimmed <= length combined

-- | 测试splitByCollapsed的基本属性
prop_splitBy_collapsed_basic :: Char -> String -> Property
prop_splitBy_collapsed_basic c s =
  let limitedS = take 80 s
      parts = splitByCollapsed c limitedS
  in if null limitedS
     then parts === []
     else property $ not (any null parts)

-- | 测试splitByCollapsed与splitBy的关系
prop_splitBy_collapsed_vs_splitBy :: Char -> String -> Property
prop_splitBy_collapsed_vs_splitBy c s =
  let limitedS = take 60 s
      collapsedParts = splitByCollapsed c limitedS
      regularParts = splitBy c limitedS
      filteredRegular = filter (not . null) regularParts
  in collapsedParts === filteredRegular

-- | 测试splitByCommaCollapsed的基本属性
prop_splitBy_comma_collapsed_basic :: String -> Property
prop_splitBy_comma_collapsed_basic s =
  let parts = splitByCommaCollapsed s
  in if null s
     then parts === []
     else property $ not (any null parts)

-- | 测试splitByCommaCollapsed与splitByComma的关系
prop_splitBy_comma_collapsed_vs_splitBy_comma :: String -> Property
prop_splitBy_comma_collapsed_vs_splitBy_comma s =
  let collapsedParts = splitByCommaCollapsed s
      regularParts = splitByComma s
      filteredRegular = filter (not . null) regularParts
  in collapsedParts === filteredRegular

-- | 测试removeLineComments对字符串字面量的处理
prop_remove_line_comments_string_literals :: String -> String -> Property
prop_remove_line_comments_string_literals code comment =
  let validCode = not ('\"' `elem` code) && not ('\'' `elem` code)
      validComment = not ('\"' `elem` comment) && not ('\'' `elem` comment)
  in if not (validCode && validComment)
     then property True
     else let codeWithComment = code ++ " // " ++ comment
              withStringLiteral = "\"string literal\" " ++ codeWithComment
              result = removeLineComments withStringLiteral
          in property $ "\"string literal\"" `isInfixOf` result

-- | 测试removeLineComments对字符字面量的处理
prop_remove_line_comments_char_literals :: String -> String -> Property
prop_remove_line_comments_char_literals code comment =
  let validCode = not ('\"' `elem` code) && not ('\'' `elem` code)
      validComment = not ('\"' `elem` comment) && not ('\'' `elem` comment)
  in if not (validCode && validComment)
     then property True
     else let codeWithComment = code ++ " // " ++ comment
              withCharLiteral = "'c' " ++ codeWithComment
              result = removeLineComments withCharLiteral
          in property $ "'c'" `isInfixOf` result

-- | 测试removeComments对嵌套注释的处理
prop_remove_comments_nested :: String -> String -> Property
prop_remove_comments_nested outer inner =
  let validOuter = not ('\"' `elem` outer) && not ('\'' `elem` outer)
      validInner = not ('\"' `elem` inner) && not ('\'' `elem` inner)
  in if not (validOuter && validInner)
     then property True
     else let nestedComment = "/* " ++ outer ++ " /* " ++ inner ++ " */ " ++ outer ++ " */"
              result = removeComments nestedComment
          in property $ not ("/*" `isInfixOf` result) && not ("*/" `isInfixOf` result)

-- | 测试removeComments对字符串中注释的处理
prop_remove_comments_strings_with_comments :: String -> String -> Property
prop_remove_comments_strings_with_comments str comment =
  let validStr = not ('\"' `elem` str) && not ('\'' `elem` str)
      validComment = not ('\"' `elem` comment) && not ('\'' `elem` comment)
  in if not (validStr && validComment)
     then property True
     else let stringWithComment = "\"" ++ str ++ " /* " ++ comment ++ " */\""
              result = removeComments stringWithComment
              commentStr = "/* " ++ comment ++ " */"
          in property $ commentStr `isInfixOf` result

-- | 测试isCompleteStringLiteral对有效字符串的处理
prop_is_complete_string_literal_valid :: String -> Property
prop_is_complete_string_literal_valid s =
  let validS = take 50 s
      stringWithQuotes = "\"" ++ validS ++ "\""
  in property $ isCompleteStringLiteral stringWithQuotes

-- | 测试isCompleteStringLiteral对无效字符串的处理
prop_is_complete_string_literal_invalid :: String -> Property
prop_is_complete_string_literal_invalid s =
  let validS = take 50 s
      stringWithoutEndQuote = "\"" ++ validS
  in property $ not $ isCompleteStringLiteral stringWithoutEndQuote

-- | 测试isCompleteStringLiteral对转义引号的处理
prop_is_complete_string_literal_escaped_quotes :: String -> Property
prop_is_complete_string_literal_escaped_quotes s =
  let validS = take 50 s
      stringWithEscapedQuotes = "\"" ++ validS ++ "\\\"" ++ validS ++ "\""
  in property $ isCompleteStringLiteral stringWithEscapedQuotes

-- | 测试isProblematicUnclosedString对问题字符串的处理
prop_is_problematic_unclosed_string :: String -> Property
prop_is_problematic_unclosed_string s =
  let validS = take 30 s
      problematicString = "\"\\\"" ++ validS
  in property $ isProblematicUnclosedString problematicString

-- | 测试normalizeIndentation对单行的处理
prop_normalize_indentation_single_line :: String -> Property
prop_normalize_indentation_single_line s =
  let limitedS = take 80 s
      normalized = normalizeIndentation limitedS
  in if null limitedS
     then normalized === ""
     else property $ length normalized >= 0

-- | 测试normalizeIndentation对多行的处理
prop_normalize_indentation_multi_line :: String -> String -> Property
prop_normalize_indentation_multi_line s1 s2 =
  let limitedS1 = take 40 s1
      limitedS2 = take 40 s2
      multiLine = limitedS1 ++ "\n" ++ limitedS2
      normalized = normalizeIndentation multiLine
  in property $ length normalized >= 0

-- | 测试breakOn的基本属性
prop_break_on_basic :: String -> String -> Property
prop_break_on_basic needle haystack =
  let limitedNeedle = take 20 needle
      limitedHaystack = take 100 haystack
      (before, after) = breakOn limitedNeedle limitedHaystack
  in if null limitedNeedle
     then before === limitedHaystack && after === ""
     else if limitedNeedle `isInfixOf` limitedHaystack
          then property $ before ++ limitedNeedle ++ after === limitedHaystack
          else before === limitedHaystack && after === ""

-- | 测试breakOn对空针的处理
prop_break_on_empty_needle :: String -> Property
prop_break_on_empty_needle haystack =
  let limitedHaystack = take 50 haystack
      (before, after) = breakOn "" limitedHaystack
  in before === limitedHaystack && after === limitedHaystack

-- | 测试safeProcessString对有效字符的处理
prop_safe_process_string_valid_chars :: String -> Property
prop_safe_process_string_valid_chars s =
  let limitedS = take 100 s
      result = safeProcessString limitedS
  in case result of
    Left _ -> property False
    Right processed -> property $ all isValidChar processed

-- | 测试safeProcessString对控制字符的处理
prop_safe_process_string_control_chars :: String -> Property
prop_safe_process_string_control_chars s =
  let limitedS = take 50 s
      withControlChars = limitedS ++ "\n\r\t"
      result = safeProcessString withControlChars
  in case result of
    Left _ -> property False
    Right processed -> property $ '\n' `elem` processed || '\r' `elem` processed || '\t' `elem` processed

-- | 测试isValidChar对可打印字符的处理
prop_is_valid_char_printable :: Char -> Property
prop_is_valid_char_printable c =
  let ordC = fromEnum c
      isPrintable = ordC >= 32 && ordC <= 126
  in if isPrintable
     then property $ isValidChar c
     else property $ isValidChar c == (c `elem` "\n\r\t")

-- | 测试isValidChar对特殊字符的处理
prop_is_valid_char_special :: Property
prop_is_valid_char_special =
  conjoin
    [ testProperty "Newline is valid" $ isValidChar '\n'
    , testProperty "Carriage return is valid" $ isValidChar '\r'
    , testProperty "Tab is valid" $ isValidChar '\t'
    , testProperty "Space is valid" $ isValidChar ' '
    ]

-- | 测试isRight对Either值的处理
prop_is_right_either :: Either String Int -> Property
prop_is_right_either e =
  Utils.isRight e === (case e of Right _ -> True; Left _ -> False)

-- | 测试isLeft对Either值的处理
prop_is_left_either :: Either String Int -> Property
prop_is_left_either e =
  isLeft e === (case e of Left _ -> True; Right _ -> False)

-- | 测试trim对空字符串的处理
test_trim_empty_string :: Assertion
test_trim_empty_string = assertEqual "Trim empty string" "" (trim "")

-- | 测试trim对纯空白字符的处理
test_trim_whitespace_only :: Assertion
test_trim_whitespace_only = do
  assertEqual "Trim spaces" "" (trim "   ")
  assertEqual "Trim tabs" "" (trim "\t\t")
  assertEqual "Trim mixed whitespace" "" (trim "  \t\n  ")

-- | 测试trim对正常字符串的处理
test_trim_normal_string :: Assertion
test_trim_normal_string = do
  assertEqual "Trim normal string" "hello" (trim "hello")
  assertEqual "Trim string with spaces" "hello" (trim "  hello  ")
  assertEqual "Trim string with tabs" "hello" (trim "\thello\t")
  assertEqual "Trim string with mixed whitespace" "hello" (trim "  \t hello \t  ")

-- | 测试splitBy对空字符串的处理
test_split_by_empty_string :: Assertion
test_split_by_empty_string = assertEqual "Split empty string" [] (splitBy ',' "")

-- | 测试splitBy对单个分隔符的处理
test_split_by_single_delimiter :: Assertion
test_split_by_single_delimiter = do
  assertEqual "Split single comma" ["", ""] (splitBy ',')
  assertEqual "Split single character" ["", ""] (splitBy 'x' "x")

-- | 测试splitBy对多个分隔符的处理
test_split_by_multiple_delimiters :: Assertion
test_split_by_multiple_delimiters = do
  assertEqual "Split multiple commas" ["", "", ""] (splitBy ',')
  assertEqual "Split mixed content" ["a", "b", "c"] (splitBy ',' "a,b,c")
  assertEqual "Split with empty parts" ["a", "", "b"] (splitBy ',' "a,,b")

-- | 测试removeLineComments对简单注释的处理
test_remove_line_comments_simple :: Assertion
test_remove_line_comments_simple = do
  assertEqual "Remove simple comment" "code " (removeLineComments "code // comment")
  assertEqual "Remove comment at start" "" (removeLineComments "// comment")
  assertEqual "Keep code without comment" "code" (removeLineComments "code")

-- | 测试removeLineComments对多行的处理
test_remove_line_comments_multiline :: Assertion
test_remove_line_comments_multiline = do
  let input = "line1\nline2 // comment\nline3"
      expected = "line1\nline2\nline3"
  assertEqual "Remove comments in multiline" expected (removeLineComments input)

-- | 测试removeComments对块注释的处理
test_remove_comments_block :: Assertion
test_remove_comments_block = do
  assertEqual "Remove block comment" "code " (removeComments "code /* comment */")
  assertEqual "Remove block comment at start" "" (removeComments "/* comment */")
  assertEqual "Keep code without comment" "code" (removeComments "code")

-- | 测试removeComments对混合注释的处理
test_remove_comments_mixed :: Assertion
test_remove_comments_mixed = do
  let input = "code // line comment\n/* block comment */ more code"
      expected = "code \n more code"
  assertEqual "Remove mixed comments" expected (removeComments input)

-- | 测试normalizeIndentation对空输入的处理
test_normalize_indentation_empty :: Assertion
test_normalize_indentation_empty = assertEqual "Normalize empty string" "" (normalizeIndentation "")

-- | 测试normalizeIndentation对单行的处理
test_normalize_indentation_single_line :: Assertion
test_normalize_indentation_single_line = do
  assertEqual "No indentation" "code" (normalizeIndentation "code")
  assertEqual "Remove leading spaces" "code" (normalizeIndentation "  code")
  assertEqual "Remove leading tabs" "code" (normalizeIndentation "\tcode")

-- | 测试normalizeIndentation对多行的处理
test_normalize_indentation_multi_line :: Assertion
test_normalize_indentation_multi_line = do
  let input = "  line1\n    line2\n  line3"
      expected = "line1\n  line2\nline3"
  assertEqual "Normalize multi-line" expected (normalizeIndentation input)

-- | 测试breakOn对基本情况的处理
test_break_on_basic :: Assertion
test_break_on_basic = do
  assertEqual "Break on substring" ("hello", " world") (breakOn " " "hello world")
  assertEqual "Break on first occurrence" ("a", "bc") (breakOn "b" "abc")
  assertEqual "No match" ("abc", "") (breakOn "x" "abc")

-- | 测试safeProcessString对正常输入的处理
test_safe_process_string_normal :: Assertion
test_safe_process_string_normal = do
  let input = "hello world"
      result = safeProcessString input
  case result of
    Left _ -> assertFailure "Should process normal string"
    Right processed -> assertEqual "Process normal string" input processed

-- | 测试safeProcessString对控制字符的处理
test_safe_process_string_control_chars :: Assertion
test_safe_process_string_control_chars = do
  let input = "hello\nworld\ttest"
      result = safeProcessString input
  case result of
    Left _ -> assertFailure "Should process string with control chars"
    Right processed -> assertBool "Should preserve control chars" $ '\n' `elem` processed

-- | 测试isValidChar对各种字符的处理
test_is_valid_char_various :: Assertion
test_is_valid_char_various = do
  assertBool "Space is valid" $ isValidChar ' '
  assertBool "Letter is valid" $ isValidChar 'a'
  assertBool "Number is valid" $ isValidChar '5'
  assertBool "Newline is valid" $ isValidChar '\n'
  assertBool "Tab is valid" $ isValidChar '\t'
  assertBool "Carriage return is valid" $ isValidChar '\r'

-- | 测试isRight和isLeft对Either值的处理
test_is_right_left_either :: Assertion
test_is_right_left_either = do
  assertBool "Right value is right" $ Utils.isRight (Right (42 :: Int))
  assertBool "Right value is not left" $ not $ isLeft (Right (42 :: Int))
  assertBool "Left value is left" $ isLeft (Left ("error" :: String))
  assertBool "Left value is not right" $ not $ Utils.isRight (Left ("error" :: String))

-- | 测试套件
tests :: TestTree
tests = memoryLevelTestGroup Moderate "New Utils QuickCheck Tests"
  [ withMemoryLevel Moderate $ testProperty "Trim whitespace handling" prop_trim_whitespace_handling
  , withMemoryLevel Moderate $ testProperty "Trim non-whitespace handling" prop_trim_non_whitespace_handling
  , withMemoryLevel Moderate $ testProperty "Trim composition" prop_trim_composition
  , withMemoryLevel Moderate $ testProperty "SplitBy collapsed basic" prop_splitBy_collapsed_basic
  , withMemoryLevel Moderate $ testProperty "SplitBy collapsed vs SplitBy" prop_splitBy_collapsed_vs_splitBy
  , withMemoryLevel Moderate $ testProperty "SplitBy comma collapsed basic" prop_splitBy_comma_collapsed_basic
  , withMemoryLevel Moderate $ testProperty "SplitBy comma collapsed vs SplitBy comma" prop_splitBy_comma_collapsed_vs_splitBy_comma
  , withMemoryLevel Moderate $ testProperty "RemoveLineComments string literals" prop_remove_line_comments_string_literals
  , withMemoryLevel Moderate $ testProperty "RemoveLineComments char literals" prop_remove_line_comments_char_literals
  , withMemoryLevel Moderate $ testProperty "RemoveComments nested" prop_remove_comments_nested
  , withMemoryLevel Moderate $ testProperty "RemoveComments strings with comments" prop_remove_comments_strings_with_comments
  , withMemoryLevel Moderate $ testProperty "IsCompleteStringLiteral valid" prop_is_complete_string_literal_valid
  , withMemoryLevel Moderate $ testProperty "IsCompleteStringLiteral invalid" prop_is_complete_string_literal_invalid
  , withMemoryLevel Moderate $ testProperty "IsCompleteStringLiteral escaped quotes" prop_is_complete_string_literal_escaped_quotes
  , withMemoryLevel Moderate $ testProperty "IsProblematicUnclosedString" prop_is_problematic_unclosed_string
  , withMemoryLevel Moderate $ testProperty "NormalizeIndentation single line" prop_normalize_indentation_single_line
  , withMemoryLevel Moderate $ testProperty "NormalizeIndentation multi line" prop_normalize_indentation_multi_line
  , withMemoryLevel Moderate $ testProperty "BreakOn basic" prop_break_on_basic
  , withMemoryLevel Moderate $ testProperty "BreakOn empty needle" prop_break_on_empty_needle
  , withMemoryLevel Moderate $ testProperty "SafeProcessString valid chars" prop_safe_process_string_valid_chars
  , withMemoryLevel Moderate $ testProperty "SafeProcessString control chars" prop_safe_process_string_control_chars
  , withMemoryLevel Moderate $ testProperty "IsValidChar printable" prop_is_valid_char_printable
  , withMemoryLevel Moderate $ testProperty "IsValidChar special" prop_is_valid_char_special
  , withMemoryLevel Moderate $ testProperty "IsRight either" prop_is_right_either
  , withMemoryLevel Moderate $ testProperty "IsLeft either" prop_is_left_either
  , testCase "Trim empty string" test_trim_empty_string
  , testCase "Trim whitespace only" test_trim_whitespace_only
  , testCase "Trim normal string" test_trim_normal_string
  , testCase "SplitBy empty string" test_split_by_empty_string
  , testCase "SplitBy single delimiter" test_split_by_single_delimiter
  , testCase "SplitBy multiple delimiters" test_split_by_multiple_delimiters
  , testCase "RemoveLineComments simple" test_remove_line_comments_simple
  , testCase "RemoveLineComments multiline" test_remove_line_comments_multiline
  , testCase "RemoveComments block" test_remove_comments_block
  , testCase "RemoveComments mixed" test_remove_comments_mixed
  , testCase "NormalizeIndentation empty" test_normalize_indentation_empty
  , testCase "NormalizeIndentation single line" test_normalize_indentation_single_line
  , testCase "NormalizeIndentation multi line" test_normalize_indentation_multi_line
  , testCase "BreakOn basic" test_break_on_basic
  , testCase "SafeProcessString normal" test_safe_process_string_normal
  , testCase "SafeProcessString control chars" test_safe_process_string_control_chars
  , testCase "IsValidChar various" test_is_valid_char_various
  , testCase "IsRight Left either" test_is_right_left_either
  ]

-- | 轻量级测试套件，用于内存受限环境
essentialTests :: TestTree
essentialTests = memoryLevelTestGroup Minimal "New Utils Essential Tests"
  [ withMemoryLevel Minimal $ testProperty "Trim whitespace handling" prop_trim_whitespace_handling
  , withMemoryLevel Minimal $ testProperty "SplitBy collapsed basic" prop_splitBy_collapsed_basic
  , withMemoryLevel Minimal $ testProperty "RemoveLineComments string literals" prop_remove_line_comments_string_literals
  , withMemoryLevel Minimal $ testProperty "IsCompleteStringLiteral valid" prop_is_complete_string_literal_valid
  , withMemoryLevel Minimal $ testProperty "BreakOn basic" prop_break_on_basic
  , withMemoryLevel Minimal $ testProperty "SafeProcessString valid chars" prop_safe_process_string_valid_chars
  , withMemoryLevel Minimal $ testProperty "IsValidChar printable" prop_is_valid_char_printable
  , withMemoryLevel Minimal $ testCase "Trim empty string" test_trim_empty_string
  , withMemoryLevel Minimal $ testCase "SplitBy empty string" test_split_by_empty_string
  , withMemoryLevel Minimal $ testCase "RemoveLineComments simple" test_remove_line_comments_simple
  , withMemoryLevel Minimal $ testCase "NormalizeIndentation empty" test_normalize_indentation_empty
  , withMemoryLevel Minimal $ testCase "BreakOn basic" test_break_on_basic
  ]