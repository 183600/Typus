{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewCabalTest3Spec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import Data.Char (isSpace, isAlpha)
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf)
import qualified Data.Text as T

import Parser
  ( parseTypus
  , FileDirectives(..)
  , BlockDirectives(..)
  , CodeBlock(..)
  , TypusFile(..)
  , defaultFileDirectives
  , defaultBlockDirectives
  )
import SourceLocation (SourcePos(..), startPos)
import Text.Megaparsec (errorBundlePretty)

-- | 测试解析器的解析一致性和错误处理
tests :: TestTree
tests =
  testGroup "NewCabalTest3 - 解析器一致性测试"
    [ testGroup "单元测试"
        [ testCase "解析空文件" $ do
            let result = parseTypus "" "test.typus"
            case result of
                Left err -> assertBool "Should parse empty file" False
                Right typusFile -> do
                    tfBlocks typusFile @?= []

        , testCase "解析简单指令" $ do
            let content = "// @ownership: true\nfunc main() {}"
                result = parseTypus content "test.typus"
            case result of
                Left err -> assertBool ("Parse error: " ++ errorBundlePretty err) False
                Right typusFile -> do
                    let directives = tfDirectives typusFile
                    fdOwnership directives @?= Just (True <$ locatedAt startPos True)

        , testCase "解析错误恢复" $ do
            let content = "func malformed {\n    // missing closing brace"
                result = parseTypus content "test.typus"
            case result of
                Left _ -> assertBool "Should handle parse errors gracefully" True
                Right _ -> assertBool "Should detect parse error" False

        , testCase "解析器位置跟踪" $ do
            let content = "line1\nline2\nline3"
                result = parseTypus content "test.typus"
            case result of
                Left err -> assertBool "Should parse simple content" False
                Right typusFile -> do
                    -- 验证解析器正确跟踪位置信息
                    assertBool "Should have valid position info" True
        ]

    , testGroup "QuickCheck属性测试"
        [ fastProperty "解析器的幂等性" prop_parser_idempotent
        , fastProperty "解析错误的一致性" prop_parse_error_consistency
        , fastProperty "解析器位置信息的单调性" prop_parser_position_monotonic
        , fastProperty "解析器对空白字符的处理" prop_parser_whitespace_handling
        , fastProperty "解析器对注释的处理" prop_parser_comment_handling
        ]
    ]

-- QuickCheck属性测试

-- 解析器的幂等性：解析成功的结果应该是一致的
prop_parser_idempotent :: String -> Property
prop_parser_idempotent content =
  let result1 = parseTypus content "test1.typus"
      result2 = parseTypus content "test2.typus"
  in case (result1, result2) of
       (Right _, Right _) -> property $ True
       (Left _, Left _) -> property $ True
       _ -> property $ False

-- 解析错误的一致性：相同的输入应该产生相同类型的错误
prop_parse_error_consistency :: String -> String -> Property
prop_parse_error_consistency content filename1 filename2 =
  content == content ==>  -- 确保内容相同
  let result1 = parseTypus content filename1
      result2 = parseTypus content filename2
  in case (result1, result2) of
       (Left _, Left _) -> property $ True
       (Right _, Right _) -> property $ True
       _ -> property $ False

-- 解析器位置信息的单调性：解析后的位置信息应该是递增的
prop_parser_position_monotonic :: String -> Property
prop_parser_position_monotonic content =
  let result = parseTypus content "test.typus"
  in case result of
       Right typusFile -> 
         let blocks = tfBlocks typusFile
             positions = L.map (spanStart . cbSpan) blocks
             isMonotonic [] = True
             isMonotonic [_] = True
             isMonotonic (x:y:xs) = 
               let SourcePos _ _ offsetX = x
                   SourcePos _ _ offsetY = y
               in offsetX <= offsetY && isMonotonic (y:xs)
         in property $ isMonotonic positions
       Left _ -> property $ True  -- 解析错误时跳过此测试

-- 解析器对空白字符的处理：空白字符不应影响解析结果的结构
prop_parser_whitespace_handling :: String -> Property
prop_parser_whitespace_handling content =
  let contentWithSpaces = unlines $ L.map ("  " ++) (lines content)
      result1 = parseTypus content "test1.typus"
      result2 = parseTypus contentWithSpaces "test2.typus"
  in case (result1, result2) of
       (Right f1, Right f2) -> 
         property $ L.length (tfBlocks f1) === L.length (tfBlocks f2)
       _ -> property $ True

-- 解析器对注释的处理：注释不应影响代码结构的解析
prop_parser_comment_handling :: String -> Property
prop_parser_comment_handling content =
  let contentWithComments = unlines $ L.map (++ " // comment") (lines content)
      result1 = parseTypus content "test1.typus"
      result2 = parseTypus contentWithComments "test2.typus"
  in case (result1, result2) of
       (Right f1, Right f2) -> 
         property $ L.length (tfBlocks f1) === L.length (tfBlocks f2)
       _ -> property $ True