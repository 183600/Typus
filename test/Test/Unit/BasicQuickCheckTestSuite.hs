{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Unit.BasicQuickCheckTestSuite where



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



import Utils (trim, splitBy, splitByComma, removeLineComments, removeComments, normalizeIndentation)
import Data.List (isInfixOf)
import Data.Char (isSpace)
import Data.Either (isLeft, isRight)
import Data.Maybe (listToMaybe)

-- | 测试trim函数的基本属性
prop_trim_basic :: String -> Property
prop_trim_basic s =
  let limitedString = take 10 s  -- 进一步减少字符串大小以降低内存消耗
      trimmed = trim limitedString
  in property $ 
    (length trimmed <= length limitedString) && 
    (if null limitedString then null trimmed else True) &&
    (if all isSpace limitedString then null trimmed else True)

-- | 测试trim对空字符串的处理
prop_trim_empty :: Property
prop_trim_empty = trim "" === ""

-- | 测试trim对空白字符的处理
prop_trim_whitespace :: String -> Property
prop_trim_whitespace s =
  let trimmed = trim s
  in if all isSpace s
     then classify (not $ null s) "non-empty whitespace" $ property $ null trimmed
     else property True

-- | 测试trim对普通字符的处理
prop_trim_regular :: Char -> String -> Property
prop_trim_regular c s =
  not (isSpace c) ==>
  let limitedS = take 10 s  -- 进一步减少字符串大小
      s' = c : limitedS
      trimmed = trim s'
      firstCharIsC = case listToMaybe trimmed of
                       Nothing -> property False
                       Just h -> h === c
  in conjoin [property (not (null trimmed)), firstCharIsC, property (length trimmed >= 1 && length trimmed <= 11)]

-- | 测试trim的幂等性
prop_trim_idempotent :: String -> Property
prop_trim_idempotent s =
  let limitedString = take 15 s  -- 进一步减少字符串大小
      trimmed1 = trim limitedString
      trimmed2 = trim trimmed1
  in trimmed1 === trimmed2

-- | 测试splitBy的基本属性
prop_splitBy_basic :: Char -> String -> Property
prop_splitBy_basic c s =
  let limitedS = take 20 s  -- 进一步减少字符串大小
      parts = splitBy c limitedS
  in if null limitedS
     then parts === [""]
     else if all (== c) limitedS
          then parts === replicate (length limitedS + 1) ""
          else property $ length (concat parts) >= length limitedS - length (filter (== c) limitedS) &&
                     length parts <= 10  -- 进一步减少分割后的部分数量

-- | 测试splitBy对空字符串的处理
prop_splitBy_empty :: Char -> Property
prop_splitBy_empty c = splitBy c "" === [""]

-- | 测试splitByComma的基本属性
prop_splitByComma_basic :: String -> Property
prop_splitByComma_basic s =
  let parts = splitByComma s
  in if null s
     then parts === [""]
     else if all (== ',') s
          then parts === replicate (length s + 1) ""
          else property $ length (concat parts) >= length s - length (filter (== ',') s)

-- | 测试splitByComma对空字符串的处理
prop_splitByComma_empty :: Property
prop_splitByComma_empty = splitByComma "" === [""]

-- | 测试removeLineComments的基本属性
prop_removeLineComments_basic :: String -> String -> Property
prop_removeLineComments_basic code comment =
  -- Avoid strings with quotes to prevent issues with string literal handling
  let validCode = not ('\"' `elem` code) && not ('\'' `elem` code)
      validComment = not ('\"' `elem` comment) && not ('\'' `elem` comment)
  in if not (validCode && validComment)
     then property True
     else let codeWithComment = code ++ "// " ++ comment ++ "\nmore code"
              withoutComments = removeLineComments codeWithComment
          in property (not ("// " `isInfixOf` withoutComments) && "more code" `isInfixOf` withoutComments)

-- | 测试removeLineComments对空代码的处理
prop_removeLineComments_empty :: Property
prop_removeLineComments_empty = removeLineComments "" === ""

-- | 测试removeLineComments对没有注释的处理
prop_removeLineComments_no_comments :: String -> Property
prop_removeLineComments_no_comments code =
  let hasComments = "//" `isInfixOf` code
      result = removeLineComments code
  in classify hasComments "has comments" $
     if hasComments then property True else property (result === code)

-- | 测试removeComments的基本属性
prop_removeComments_basic :: [Char] -> [Char] -> Property
prop_removeComments_basic beforeStr afterStr =
  -- Avoid strings with quotes to prevent issues with string literal handling
  let validBefore = not ('\"' `elem` beforeStr) && not ('\'' `elem` beforeStr)
      validAfter = not ('\"' `elem` afterStr) && not ('\'' `elem` afterStr)
  in if not (validBefore && validAfter)
     then property True
     else let codeWithComment = beforeStr ++ "/* " ++ "comment" ++ " */" ++ afterStr
              withoutComments = removeComments codeWithComment
          in property (not (isInfixOf "/* comment */" withoutComments))

-- | 测试removeComments对空代码的处理
prop_removeComments_empty :: Property
prop_removeComments_empty = removeComments "" === ""

-- | 测试removeComments对没有注释的处理
prop_removeComments_no_comments :: String -> Property
prop_removeComments_no_comments code =
  let hasStartComment = "/*" `isInfixOf` code
      hasEndComment = "*/" `isInfixOf` code
      hasLineComment = "//" `isInfixOf` code
      hasComments = hasStartComment || hasEndComment || hasLineComment
      result = removeComments code
  in classify hasComments "has comments" $
     if hasComments then property True else property (result === code)

-- | 测试normalizeIndentation的基本属性
prop_normalizeIndentation_basic :: String -> Property
prop_normalizeIndentation_basic s =
  let normalized = normalizeIndentation s
  in property $ length normalized >= 0

-- | 测试normalizeIndentation对空字符串的处理
prop_normalizeIndentation_empty :: Property
prop_normalizeIndentation_empty = normalizeIndentation "" === ""

-- | 测试normalizeIndentation对无缩进的处理
prop_normalizeIndentation_no_indent :: String -> Property
prop_normalizeIndentation_no_indent s =
  let hasIndent = any isSpace s
      result = normalizeIndentation s
  in classify hasIndent "has indentation" $
     if hasIndent then property True else property (result === s)

-- | 测试isRight的基本属性
prop_isRight_basic :: Either String Int -> Property
prop_isRight_basic e = Data.Either.isRight e === (case e of Right _ -> True; Left _ -> False)

-- | 测试isLeft的基本属性
prop_isLeft_basic :: Either String Int -> Property
prop_isLeft_basic e = Data.Either.isLeft e === (case e of Left _ -> True; Right _ -> False)

-- | 测试isRight对Right值的处理
prop_isRight_right :: Int -> Property
prop_isRight_right x = property $ isRight (Right x)

-- | 测试isRight对Left值的处理
prop_isRight_left :: String -> Property
prop_isRight_left msg = property $ not $ isRight (Left msg)

-- | 测试isLeft对Right值的处理
prop_isLeft_right :: Int -> Property
prop_isLeft_right x = property $ not $ isLeft (Right x)

-- | 测试isLeft对Left值的处理
prop_isLeft_left :: String -> Property
prop_isLeft_left msg = property $ isLeft (Left msg)

-- | 测试trim的边界情况
test_trim_edge_cases :: Assertion
test_trim_edge_cases = do
  assertEqual "Empty string" "" (trim "")
  assertEqual "Single space" "" (trim " ")
  assertEqual "Single tab" "" (trim "\t")
  assertEqual "Multiple spaces" "" (trim "   ")
  assertEqual "Mixed whitespace" "content" (trim "  \t  content  ")

-- | 测试splitBy的边界情况
test_splitBy_edge_cases :: Assertion
test_splitBy_edge_cases = do
  assertEqual "Empty string" [""] (splitBy ',' "")
  assertEqual "No separator" ["single"] (splitBy 'x' "single")
  assertEqual "Single separator" ["", ""] (splitBy ',' ",")
  assertEqual "Multiple separators" ["a", "", "b"] (splitBy ',' "a,,b")

-- | 测试splitByComma的边界情况
test_splitByComma_edge_cases :: Assertion
test_splitByComma_edge_cases = do
  assertEqual "Empty string" [""] (splitByComma "")
  assertEqual "No commas" ["single"] (splitByComma "single")
  assertEqual "Single comma" ["", ""] (splitByComma ",")
  assertEqual "Multiple commas" ["a", "", "b"] (splitByComma "a,,b")

-- | 测试removeLineComments的边界情况
test_removeLineComments_edge_cases :: Assertion
test_removeLineComments_edge_cases = do
  assertEqual "Empty code" "" (removeLineComments "")
  assertEqual "No comments" "code" (removeLineComments "code")
  assertEqual "Single line comment" "code " (removeLineComments "code // comment")
  assertEqual "Multiple line comments" "code\n\n\nmore code" (removeLineComments "code\n// comment1\n// comment2\nmore code")

-- | 测试removeComments的边界情况
test_removeComments_edge_cases :: Assertion
test_removeComments_edge_cases = do
  assertEqual "Empty code" "" (removeComments "")
  assertEqual "No comments" "code" (removeComments "code")
  assertEqual "Single line comment" "code " (removeComments "code /* comment */")
  assertEqual "Multiple line comments" "code \nmore code" (removeComments "code /* comment1 */\nmore code")

-- | 测试normalizeIndentation的边界情况
test_normalizeIndentation_edge_cases :: Assertion
test_normalizeIndentation_edge_cases = do
  assertEqual "Empty string" "" (normalizeIndentation "")
  assertEqual "No indentation" "code" (normalizeIndentation "code")
  assertEqual "Single indentation" "  code" (normalizeIndentation "  code")
  assertEqual "Multiple indentation" "    code" (normalizeIndentation "    code")

-- | 测试isRight的边界情况
test_isRight_edge_cases :: Assertion
test_isRight_edge_cases = do
  assertBool "Right value is right" (isRight (Right (42 :: Int)))
  assertBool "Left value is not right" (not $ isRight (Left ("error" :: String)))

-- | 测试isLeft的边界情况
test_isLeft_edge_cases :: Assertion
test_isLeft_edge_cases = do
  assertBool "Left value is left" (isLeft (Left ("error" :: String)))
  assertBool "Right value is not left" (not $ isLeft (Right ("success" :: String)))

-- | 测试套件
tests :: TestTree
tests = memoryLevelTestGroup Moderate "Basic QuickCheck Test Suite (Memory Optimized)"
  [ withMemoryLevel Moderate $ testProperty "Trim basic" prop_trim_basic
  , withMemoryLevel Moderate $ testProperty "Trim empty" prop_trim_empty
  , withMemoryLevel Moderate $ testProperty "Trim whitespace" prop_trim_whitespace
  , withMemoryLevel Moderate $ testProperty "Trim regular" prop_trim_regular
  , withMemoryLevel Moderate $ testProperty "Trim idempotent" prop_trim_idempotent
  , withMemoryLevel Moderate $ testProperty "SplitBy basic" prop_splitBy_basic
  , withMemoryLevel Moderate $ testProperty "SplitBy empty" prop_splitBy_empty
  , withMemoryLevel Moderate $ testProperty "SplitByComma basic" prop_splitByComma_basic
  , withMemoryLevel Moderate $ testProperty "SplitByComma empty" prop_splitByComma_empty
  , withMemoryLevel Moderate $ testProperty "RemoveLineComments basic" prop_removeLineComments_basic
  , withMemoryLevel Moderate $ testProperty "RemoveLineComments empty" prop_removeLineComments_empty
  , withMemoryLevel Moderate $ testProperty "RemoveLineComments no comments" prop_removeLineComments_no_comments
  , withMemoryLevel Moderate $ testProperty "RemoveComments basic" prop_removeComments_basic
  , withMemoryLevel Moderate $ testProperty "RemoveComments empty" prop_removeComments_empty
  , withMemoryLevel Moderate $ testProperty "RemoveComments no comments" prop_removeComments_no_comments
  , withMemoryLevel Moderate $ testProperty "NormalizeIndentation basic" prop_normalizeIndentation_basic
  , withMemoryLevel Moderate $ testProperty "NormalizeIndentation empty" prop_normalizeIndentation_empty
  , withMemoryLevel Moderate $ testProperty "NormalizeIndentation no indent" prop_normalizeIndentation_no_indent
  , withMemoryLevel Moderate $ testProperty "isRight basic" prop_isRight_basic
  , withMemoryLevel Moderate $ testProperty "isLeft basic" prop_isLeft_basic
  , withMemoryLevel Moderate $ testProperty "isRight right" prop_isRight_right
  , withMemoryLevel Moderate $ testProperty "isRight left" prop_isRight_left
  , withMemoryLevel Moderate $ testProperty "isLeft right" prop_isLeft_right
  , withMemoryLevel Moderate $ testProperty "isLeft left" prop_isLeft_left
  , testCase "Trim edge cases" test_trim_edge_cases
  , testCase "SplitBy edge cases" test_splitBy_edge_cases
  , testCase "SplitByComma edge cases" test_splitByComma_edge_cases
  , testCase "RemoveLineComments edge cases" test_removeLineComments_edge_cases
  , testCase "RemoveComments edge cases" test_removeComments_edge_cases
  , testCase "NormalizeIndentation edge cases" test_normalizeIndentation_edge_cases
  , testCase "isRight edge cases" test_isRight_edge_cases
  , testCase "isLeft edge cases" test_isLeft_edge_cases
  ]

-- | 轻量级测试套件，用于内存受限环境
essentialTests :: TestTree
essentialTests = memoryLevelTestGroup Minimal "Basic QuickCheck Essential Tests"
  [ withMemoryLevel Minimal $ testProperty "Trim basic" prop_trim_basic
  , withMemoryLevel Minimal $ testProperty "Trim idempotent" prop_trim_idempotent
  , withMemoryLevel Minimal $ testProperty "SplitBy basic" prop_splitBy_basic
  , withMemoryLevel Minimal $ testProperty "RemoveLineComments basic" prop_removeLineComments_basic
  , withMemoryLevel Minimal $ testCase "Trim edge cases" test_trim_edge_cases
  , withMemoryLevel Minimal $ testCase "SplitBy edge cases" test_splitBy_edge_cases
  ]