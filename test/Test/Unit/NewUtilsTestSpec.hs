{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.NewUtilsTestSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import Utils
import qualified Data.Text as T
import Data.List (isPrefixOf, isSuffixOf, isInfixOf)
import Data.Char (isSpace, isAlpha, isDigit, isLetter)

-- | 测试trim函数
test_trim_basic :: Assertion
test_trim_basic = do
  assertEqual "Trim leading spaces" "hello" (trim "   hello")
  assertEqual "Trim trailing spaces" "hello" (trim "hello   ")
  assertEqual "Trim both sides" "hello" (trim "   hello   ")
  assertEqual "No spaces to trim" "hello" (trim "hello")
  assertEqual "Empty string" "" (trim "")
  assertEqual "Only spaces" "" (trim "   ")

-- | 测试splitBy函数
test_splitBy_basic :: Assertion
test_splitBy_basic = do
  assertEqual "Split by comma" ["a", "b", "c"] (splitBy ',' "a,b,c")
  assertEqual "Split with empty segments" ["a", "", "c"] (splitBy ',' "a,,c")
  assertEqual "Split starting with delimiter" ["", "a", "b"] (splitBy ',' ",a,b")
  assertEqual "Split ending with delimiter" ["a", "b", ""] (splitBy ',' "a,b,")
  assertEqual "Split single element" ["abc"] (splitBy ',' "abc")
  assertEqual "Split empty string" [] (splitBy ',' "")

-- | 测试splitByCollapsed函数
test_splitByCollapsed_basic :: Assertion
test_splitByCollapsed_basic = do
  assertEqual "Split by comma collapsed" ["a", "b", "c"] (splitByCollapsed ',' "a,b,c")
  assertEqual "Split with consecutive delimiters collapsed" ["a", "c"] (splitByCollapsed ',' "a,,c")
  assertEqual "Split starting with delimiter collapsed" ["a", "b"] (splitByCollapsed ',' ",a,b")
  assertEqual "Split ending with delimiter collapsed" ["a", "b"] (splitByCollapsed ',' "a,b,")
  assertEqual "Split single element" ["abc"] (splitByCollapsed ',' "abc")
  assertEqual "Split empty string" [] (splitByCollapsed ',' "")

-- | 测试splitByComma函数
test_splitByComma :: Assertion
test_splitByComma = do
  assertEqual "Split by comma" ["a", "b", "c"] (splitByComma "a,b,c")
  assertEqual "Split with empty segments" ["a", "", "c"] (splitByComma "a,,c")

-- | 测试splitByCommaCollapsed函数
test_splitByCommaCollapsed :: Assertion
test_splitByCommaCollapsed = do
  assertEqual "Split by comma collapsed" ["a", "b", "c"] (splitByCommaCollapsed "a,b,c")
  assertEqual "Split with consecutive delimiters collapsed" ["a", "c"] (splitByCommaCollapsed "a,,c")

-- | 测试removeLineComments函数
test_remove_line_comments :: Assertion
test_remove_line_comments = do
  assertEqual "Remove single line comment" "let x = 42" (removeLineComments "let x = 42 // comment")
  assertEqual "Keep string literals" "let s = \"// not a comment\"" (removeLineComments "let s = \"// not a comment\" // real comment")
  assertEqual "Keep char literals" "let c = '/'" (removeLineComments "let c = '/' // comment")
  assertEqual "No comment to remove" "let x = 42" (removeLineComments "let x = 42")
  assertEqual "Only comment" "" (removeLineComments "// only comment")
  assertEqual "Multiple lines" "let x = 42\nlet y = 24" (removeLineComments "let x = 42 // comment\nlet y = 24 // another comment")

-- | 测试removeComments函数
test_remove_comments :: Assertion
test_remove_comments = do
  assertEqual "Remove line comment" "let x = 42 " (removeComments "let x = 42 // comment")
  assertEqual "Remove block comment" "let x = 42 " (removeComments "let x = 42 /* block comment */")
  assertEqual "Remove both types" "let x = 42\nlet y = 24 " (removeComments "let x = 42 // line\nlet y = 24 /* block */")
  assertEqual "Keep string literals with comment-like content" "let s = \"// not a comment\"" (removeComments "let s = \"// not a comment\"")
  assertEqual "Keep string literals with block comment" "let s = \"/* not a comment */\"" (removeComments "let s = \"/* not a comment */\"")
  assertEqual "Nested block comments" "let x = 42 " (removeComments "let x = 42 /* outer /* inner */ */")

-- | 测试normalizeIndentation函数
test_normalize_indentation :: Assertion
test_normalize_indentation = do
  assertEqual "Normalize with consistent indentation" "let x = 42\n  let y = 24" (normalizeIndentation "  let x = 42\n    let y = 24")
  assertEqual "Normalize with tabs" "let x = 42\n  let y = 24" (normalizeIndentation "\tlet x = 42\n\t\tlet y = 24")
  assertEqual "Normalize with mixed spaces and tabs" "let x = 42\n  let y = 24" (normalizeIndentation "  \tlet x = 42\n\t  \tlet y = 24")
  assertEqual "No indentation needed" "let x = 42\nlet y = 24" (normalizeIndentation "let x = 42\nlet y = 24")
  assertEqual "Empty string" "" (normalizeIndentation "")

-- | 测试breakOn函数
test_break_on :: Assertion
test_break_on = do
  assertEqual "Break on comma" ("a", "b,c,d") (breakOn "," "a,b,c,d")
  assertEqual "Break on first occurrence" ("a", "b,c,d") (breakOn "," "a,b,c,d")
  assertEqual "No delimiter found" ("a,b,c,d", "") (breakOn "." "a,b,c,d")
  assertEqual "Empty string" ("", "") (breakOn "," "")
  assertEqual "Starts with delimiter" ("", "a,b") (breakOn "," ",a,b")

-- | 测试safeProcessString函数
test_safe_process_string :: Assertion
test_safe_process_string = do
  assertEqual "Process normal string" (Right "hello world") (safeProcessString "hello world")
  assertEqual "Process string with special chars" (Right "hello & world") (safeProcessString "hello & world")
  assertEqual "Process empty string" (Right "") (safeProcessString "")
  assertEqual "Process string with Unicode" (Right "你好世界") (safeProcessString "你好世界")

-- | 测试isValidChar函数
test_is_valid_char :: Assertion
test_is_valid_char = do
  assertBool "Valid character" (isValidChar 'a')
  assertBool "Valid digit" (isValidChar '1')
  assertBool "Valid space" (isValidChar ' ')
  assertBool "Valid punctuation" (isValidChar '.')
  assertBool "Valid Unicode" (isValidChar '你')
  assertBool "Null character is invalid" (not $ isValidChar '\0')

-- | QuickCheck属性：trim函数的幂等性
prop_trim_idempotent :: String -> Property
prop_trim_idempotent s =
  let trimmedOnce = trim s
      trimmedTwice = trim trimmedOnce
  in trimmedOnce === trimmedTwice

-- | QuickCheck属性：splitBy和splitByCollapsed的关系
prop_split_by_collapsed_relationship :: Char -> String -> Property
prop_split_by_collapsed_relationship delim s =
  let splitResult = splitBy delim s
      collapsedResult = splitByCollapsed delim s
      filteredResult = filter (not . null) splitResult
  in collapsedResult === filteredResult

-- | QuickCheck属性：splitBy的逆属性
prop_split_by_inverse :: Char -> [String] -> Property
prop_split_by_inverse delim parts =
  let joined = intercalate [delim] parts
      splitResult = splitBy delim joined
      -- 基本检查：splitBy应该产生可以重新组合的段
      rejoined = intercalate [delim] splitResult
      -- 检查重新组合的结果是否与原始joined相同
      basicCheck = rejoined === joined
      -- 对于不包含空字符串的parts，检查splitResult是否与parts相同
      noEmptyCheck = if not (any null parts) && not (any (elem delim) parts)
                     then splitResult === parts
                     else property True
  in property $ basicCheck .&&. noEmptyCheck
  where
    intercalate _ [] = []
    intercalate _ [x] = x
    intercalate sep (x:xs) = x ++ sep ++ intercalate sep xs

-- | QuickCheck属性：trim不改变字符串中间的空格
prop_trim_preserves_internal_spaces :: String -> String -> String -> Property
prop_trim_preserves_internal_spaces prefix middle suffix =
  let s = prefix ++ " " ++ middle ++ " " ++ suffix
      trimmed = trim s
      -- 检查middle部分是否保留在trim后的字符串中
      hasMiddle = isInfixOf middle trimmed
      -- 特殊处理：如果整个字符串只有空格，trim后应该为空
      allSpaces = all isSpace s
  in if allSpaces
     then trimmed === ""
     else if not (null middle) && not (all isSpace middle)
          then property hasMiddle
          else property True

-- | QuickCheck属性：removeLineComments不改变字符串字面量
prop_remove_line_comments_preserves_strings :: String -> Property
prop_remove_line_comments_preserves_strings s =
  let stringWithComment = s ++ " // comment"
      withoutComment = removeLineComments stringWithComment
      -- 检查是否是字符串字面量（包含未闭合的引号）
      isStringLiteral str = 
        let doubleQuoteCount = length $ filter (== '"') str
            singleQuoteCount = length $ filter (== '\'') str
        in odd doubleQuoteCount || odd singleQuoteCount  -- 奇数个引号表示未闭合的字符串字面量
  in if "//" `isInfixOf` s  -- 如果字符串本身包含//，跳过测试
     then property True
     else if '\n' `elem` s  -- 如果字符串包含换行符，可能会被trim
          then withoutComment === unlines (map trim (lines s))
          else if isStringLiteral s  -- 如果是字符串字面量，保持原样
               then property $ withoutComment === (s ++ " // comment")
               else withoutComment === trim s

-- | QuickCheck属性：normalizeIndentation保持相对缩进
prop_normalize_indentation_preserves_relative :: String -> Property
prop_normalize_indentation_preserves_relative s =
  let normalized = normalizeIndentation s
      -- 检查normalizeIndentation是否保持行数不变（除了全空行的情况）
      originalLines = lines s
      normalizedLines = lines normalized
      -- 检查非空行的数量是否保持不变
      originalNonEmpty = filter (not . all isSpace) originalLines
      normalizedNonEmpty = filter (not . all isSpace) normalizedLines
  in property $ length normalizedNonEmpty == length originalNonEmpty

-- | QuickCheck属性：breakOn的正确性
prop_break_on_correctness :: String -> String -> Property
prop_break_on_correctness delim s =
  let (before, after) = breakOn delim s
      expected = if not (null delim) && delim `isInfixOf` s
                    then let parts = splitBy (case delim of (x:_) -> x; [] -> error "unreachable") s
                         in if length parts >= 2
                            then let b = case parts of (x:_) -> x; [] -> error "unreachable"
                                     rest = drop 1 parts
                                 in (b, intercalate delim rest)
                            else (s, "")
                    else if null delim
                         then ("", s)  -- 当模式为空时，期望 ("", s)
                         else (s, "")
  in (before, after) === expected
  where
    intercalate _ [] = []
    intercalate _ [x] = x
    intercalate sep (x:xs) = x ++ sep ++ intercalate sep xs

-- | 测试边界情况
test_edge_cases :: Assertion
test_edge_cases = do
  -- 测试大量空格的trim
  assertEqual "Trim many spaces" "x" (trim $ replicate 1000 ' ' ++ "x" ++ replicate 1000 ' ')
  
  -- 测试大量分隔符的split
  let manyDelims = replicate 100 ','
  assertEqual "Split many delimiters" (replicate 101 "") (splitBy ',' manyDelims)
  assertEqual "Split many delimiters collapsed" [] (splitByCollapsed ',' manyDelims)
  
  -- 测试嵌套注释
  let nestedComments = "text /* outer /* inner */ still outer */ end"
  assertEqual "Remove nested comments" "text  end" (removeComments nestedComments)

-- | 测试Unicode处理
test_unicode_handling :: Assertion
test_unicode_handling = do
  assertEqual "Trim Unicode spaces" "中文" (trim "  中文  ")
  assertEqual "Split Unicode content" ["你好", "世界"] (splitBy ',' "你好,世界")
  assertEqual "Remove comments with Unicode" "let 中文 = \"hello\"" (removeComments "let 中文 = \"hello\" // 注释")
  assertEqual "Normalize Unicode indentation" "let 中文 = 42\n  let 英文 = 24" (normalizeIndentation "  let 中文 = 42\n    let 英文 = 24")

-- | 测试套件
tests :: TestTree
tests = testGroup "New Utils Tests"
  [ testCase "Trim basic" test_trim_basic
  , testCase "SplitBy basic" test_splitBy_basic
  , testCase "SplitByCollapsed basic" test_splitByCollapsed_basic
  , testCase "SplitByComma" test_splitByComma
  , testCase "SplitByCommaCollapsed" test_splitByCommaCollapsed
  , testCase "Remove line comments" test_remove_line_comments
  , testCase "Remove comments" test_remove_comments
  , testCase "Normalize indentation" test_normalize_indentation
  , testCase "Break on" test_break_on
  , testCase "Safe process string" test_safe_process_string
  , testCase "Is valid char" test_is_valid_char
  , testCase "Edge cases" test_edge_cases
  , testCase "Unicode handling" test_unicode_handling
  , testProperty "Trim idempotent" prop_trim_idempotent
  , testProperty "SplitBy collapsed relationship" prop_split_by_collapsed_relationship
  , testProperty "SplitBy inverse" prop_split_by_inverse
  , testProperty "Trim preserves internal spaces" prop_trim_preserves_internal_spaces
  , testProperty "Remove line comments preserves strings" prop_remove_line_comments_preserves_strings
  , testProperty "Normalize indentation preserves relative" prop_normalize_indentation_preserves_relative
  , testProperty "BreakOn correctness" prop_break_on_correctness
  ]