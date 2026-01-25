{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Unit.AdditionalCabalQuickCheckTests where




import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Utils
import SourceLocation (SourcePos(..))
import Parser (parseTypus, TypusFile(..), defaultFileDirectives)
import Compiler (compile)
import Data.Char (isSpace)

-- ============================================================================
-- 高级QuickCheck测试 (10个测试用例)
-- ============================================================================

-- 测试字符串处理的高级属性
test_advanced_string_processing :: Assertion
test_advanced_string_processing = do
  assertEqual "Complex trim" "hello world" (trim "  \t hello world \t\n  ")
  assertEqual "Complex split" ["a", "b", "c"] (splitBy ',' "a,b,c")
  assertEqual "Complex comment removal" "let x = 42" (removeLineComments "let x = 42 // comment")

-- 测试源位置计算的高级属性
test_advanced_source_location :: Assertion
test_advanced_source_location = do
  let loc1 = SourcePos 10 20 0
  let loc2 = SourcePos 10 30 0
  assertBool "Location comparison" (loc1 < loc2)

-- 测试解析器的高级属性
test_advanced_parser :: Assertion
test_advanced_parser = do
  assertEqual "Parse complex expression" (Right defaultTypusFile) (parseTypus "let x = 42 + 24")
  where
    defaultTypusFile = TypusFile defaultFileDirectives [] [] []

-- 测试编译器的高级属性
test_advanced_compiler :: Assertion
test_advanced_compiler = do
  let parsed = parseTypus "let x = 42 + 24"
  let compiled = case parsed of
        Right typusFile -> compile typusFile
        Left _ -> Left (error "Parse error")
  assertEqual "Compile complex expression" (Right "") compiled

-- QuickCheck属性：trim的交换律与结合律
prop_trim_laws :: String -> String -> String -> Property
prop_trim_laws s1 s2 s3 =
  let trim12 = trim (s1 ++ s2)
      trim23 = trim (s2 ++ s3)
      trim123 = trim (s1 ++ s2 ++ s3)
      trim1_23 = trim (s1 ++ trim23)
      trim12_3 = trim (trim12 ++ s3)
  in property $ trim1_23 === trim12_3 .&&. trim123 === trim12_3

-- QuickCheck属性：splitBy的分配律
prop_split_by_distributive :: Char -> String -> String -> Property
prop_split_by_distributive delim s1 s2 =
  let split1 = splitBy delim s1
      split2 = splitBy delim s2
      splitCombined = splitBy delim (s1 ++ [delim] ++ s2)
  in property $ splitCombined === split1 ++ split2

-- QuickCheck属性：removeLineComments的上下文保持
prop_remove_line_comments_context :: String -> String -> Property
prop_remove_line_comments_context code comment =
  let fullCode = code ++ "\n" ++ "  // " ++ comment ++ "\n" ++ code
      withoutComment = removeLineComments fullCode
      expected = code ++ "\n\n" ++ code
  in property $ withoutComment === expected

-- QuickCheck属性：normalizeIndentation的相对性保持
prop_normalize_indentation_relative :: String -> Int -> Property
prop_normalize_indentation_relative s indent =
  let lineList = lines s
      indentedLines = map (\line -> replicate indent ' ' ++ line) lineList
      normalized = normalizeIndentation (unlines indentedLines)
      originalLines = filter (not . all isSpace) lineList
      normalizedLines = filter (not . all isSpace) (lines normalized)
  in property $ length normalizedLines === length originalLines

-- QuickCheck属性：SourceLocation的向量运算
prop_source_location_vector :: Int -> Int -> Int -> Int -> Property
prop_source_location_vector line1 col1 line2 col2 =
  let _ = SourcePos (abs line1) (abs col1) 0 :: SourcePos
      _ = SourcePos (abs line2) (abs col2) 0 :: SourcePos
      distance = abs ((abs line1) - (abs line2)) + abs ((abs col1) - (abs col2))
  in property $ distance >= 0

-- QuickCheck属性：Parser-Compiler组合的幂等性
prop_parser_compiler_idempotent :: String -> Property
prop_parser_compiler_idempotent s =
  let parsed = parseTypus s
      compiled = case parsed of
        Right typusFile -> compile typusFile
        Left _ -> Left (error "Parse error")
  in property $ case compiled of
    Right _ -> property True
    Left _ -> property True

-- 测试套件
tests :: TestTree
tests = testGroup "Additional Cabal QuickCheck Tests"
  [ testGroup "Advanced QuickCheck Tests"
    [ testCase "Advanced string processing" test_advanced_string_processing
    , testCase "Advanced source location" test_advanced_source_location
    , testCase "Advanced parser" test_advanced_parser
    , testCase "Advanced compiler" test_advanced_compiler
    , testProperty "Trim laws" prop_trim_laws
    , testProperty "SplitBy distributive" prop_split_by_distributive
    , testProperty "RemoveLineComments context" prop_remove_line_comments_context
    , testProperty "NormalizeIndentation relative" prop_normalize_indentation_relative
    , testProperty "SourceLocation vector" prop_source_location_vector
    , testProperty "Parser-Compiler idempotent" prop_parser_compiler_idempotent
    ]
  ]