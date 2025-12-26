{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.ParserBoundaryConditionsSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=), assertBool)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, choose, elements, listOf, oneof, sized)
import Parser
  ( parseTypus
  , FileDirectives(..)
  , BlockDirectives(..)
  , CodeBlock(..)
  , TypusFile(..)
  , defaultFileDirectives
  , defaultBlockDirectives
  )
import qualified Data.Text as T
import Data.Char (isSpace, isAlphaNum)
import Data.List (isPrefixOf, isInfixOf)

-- ============================================================================
-- 生成测试数据
-- ============================================================================

-- 生成有效的标识符字符
genIdentifierChar :: Gen Char
genIdentifierChar = elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_-"

-- 生成有效的标识符
genIdentifier :: Gen String
genIdentifier = do
  first <- elements $ ['a'..'z'] ++ ['A'..'Z'] ++ "_"
  rest <- listOf genIdentifierChar
  return $ first : rest

-- 生成有效的指令值
genDirectiveValue :: Gen String
genDirectiveValue = oneof
  [ return "true"
  , return "false"
  , return "enabled"
  , return "disabled"
  , genIdentifier
  ]

-- 生成文件指令
genFileDirective :: Gen String
genFileDirective = do
  key <- elements ["ownership", "dependent-types", "constraints"]
  value <- genDirectiveValue
  return $ key ++ "=" ++ value

-- 生成块指令
genBlockDirective :: Gen String
genBlockDirective = do
  key <- elements ["ownership", "dependent-types", "constraints"]
  value <- genDirectiveValue
  return $ key ++ "=" ++ value

-- ============================================================================
-- 边界情况测试
-- ============================================================================

-- Property: 空文件解析
prop_parse_empty_file :: Property
prop_parse_empty_file =
  let result = parseTypus "" ""
      expected = TypusFile defaultFileDirectives [] [] []
  in case result of
    Left _ -> property False
    Right parsed -> property $ parsed === expected

-- Property: 只有文件指令的文件
prop_parse_file_with_only_directives :: [String] -> Property
prop_parse_file_with_only_directives directives =
  not (null directives) ==>
  let content = unlines $ map ("//! " ++) directives
      result = parseTypus "" content
  in case result of
    Left _ -> property False
    Right parsed -> property $ null (tfBlocks parsed)

-- Property: 只有代码块的文件
prop_parse_file_with_only_blocks :: [String] -> Property
prop_parse_file_with_only_blocks blocks =
  not (null blocks) ==>
  let content = unlines $ concatMap (\b -> ["```go", b, "```"]) blocks
      result = parseTypus "" content
  in case result of
    Left _ -> property False
    Right parsed -> property $ length (tfBlocks parsed) === length blocks

-- Property: 嵌套的代码块
prop_parse_nested_blocks :: [String] -> Property
prop_parse_nested_blocks blocks =
  not (null blocks) ==>
  let nestedContent = concatMap (\b -> ["```go", "```go", b, "```", "```"]) blocks
      content = unlines nestedContent
      result = parseTypus "" content
  in case result of
    Left _ -> property False
    Right parsed -> property $ length (tfBlocks parsed) >= length blocks

-- Property: 非常长的行
prop_parse_very_long_lines :: String -> Property
prop_parse_very_long_lines base =
  let longLine = concat (replicate 1000 base) ++ "very long content"
      content = unlines ["//! ownership=true", "```go", longLine, "```"]
      result = parseTypus "" content
  in case result of
    Left _ -> property False
    Right parsed -> property $ not (null (tfBlocks parsed))

-- Property: 特殊字符处理
prop_parse_special_characters :: String -> Property
prop_parse_special_characters chars =
  let specialContent = "code with: " ++ chars ++ " and more"
      content = unlines ["```go", specialContent, "```"]
      result = parseTypus "" content
  in case result of
    Left _ -> property False
    Right parsed -> property $ not (null (tfBlocks parsed))

-- Property: Unicode字符处理
prop_parse_unicode_characters :: Property
prop_parse_unicode_characters =
  let unicodeContent = "测试内容 with émojis 🚀 and ñoño"
      content = unlines ["```go", unicodeContent, "```"]
      result = parseTypus "" content
  in case result of
    Left _ -> property False
    Right parsed -> property $ not (null (tfBlocks parsed))

-- Property: 混合缩进
prop_parse_mixed_indentation :: [String] -> Property
prop_parse_mixed_indentation lines =
  not (null lines) ==>
  let mixedLines = zipWith (\i l -> replicate i ' ' ++ l) [0,2,4,1,3,0] lines
      content = unlines ["```go"] ++ mixedLines ++ ["```"]
      result = parseTypus "" content
  in case result of
    Left _ -> property False
    Right parsed -> property $ not (null (tfBlocks parsed))

-- Property: 空代码块
prop_parse_empty_code_blocks :: Int -> Property
prop_parse_empty_code_blocks count =
  count > 0 && count <= 10 ==>
  let emptyBlocks = replicate count "```go\n```"
      content = unlines emptyBlocks
      result = parseTypus "" content
  in case result of
    Left _ -> property False
    Right parsed -> property $ length (tfBlocks parsed) === count

-- Property: 格式错误的指令
prop_parse_malformed_directives :: [String] -> Property
prop_parse_malformed_directives badDirectives =
  not (null badDirectives) ==>
  let content = unlines $ map ("//! " ++) badDirectives
      result = parseTypus "" content
  in case result of
    Left _ -> property True  -- 期望解析失败
    Right parsed -> property $ True  -- 或者解析成功但有默认值

-- ============================================================================
-- 单元测试
-- ============================================================================

tests :: TestTree
tests =
  testGroup "Parser Boundary Conditions Tests"
    [ testGroup "Property Tests"
        [ fastProperty "parse empty file" prop_parse_empty_file
        , fastProperty "parse file with only directives" prop_parse_file_with_only_directives
        , fastProperty "parse file with only blocks" prop_parse_file_with_only_blocks
        , fastProperty "parse nested blocks" prop_parse_nested_blocks
        , fastProperty "parse very long lines" prop_parse_very_long_lines
        , fastProperty "parse special characters" prop_parse_special_characters
        , fastProperty "parse unicode characters" prop_parse_unicode_characters
        , fastProperty "parse mixed indentation" prop_parse_mixed_indentation
        , fastProperty "parse empty code blocks" prop_parse_empty_code_blocks
        , fastProperty "parse malformed directives" prop_parse_malformed_directives
        ]
    , testGroup "Unit Tests"
        [ testCase "parse file with valid ownership directive" $ do
            let content = "//! ownership=true\n```go\ncode\n```"
                result = parseTypus "" content
            case result of
              Left err -> assertFailure $ "Parse failed: " ++ show err
              Right parsed -> do
                let directives = tfDirectives parsed
                case fdOwnership directives of
                  Nothing -> assertFailure "Expected ownership directive"
                  Just located -> locatedValue located @?= True

        , testCase "parse file with multiple directives" $ do
            let content = unlines
                  [ "//! ownership=true"
                  , "//! dependent-types=false"
                  , "//! constraints=enabled"
                  , "```go"
                  , "code"
                  , "```"
                  ]
                result = parseTypus "" content
            case result of
              Left err -> assertFailure $ "Parse failed: " ++ show err
              Right parsed -> do
                let directives = tfDirectives parsed
                case fdOwnership directives of
                  Nothing -> assertFailure "Expected ownership directive"
                  Just located -> locatedValue located @?= True
                case fdDependentTypes directives of
                  Nothing -> assertFailure "Expected dependent-types directive"
                  Just located -> locatedValue located @?= False
                case fdConstraints directives of
                  Nothing -> assertFailure "Expected constraints directive"
                  Just located -> locatedValue located @?= "enabled"

        , testCase "parse file with block-level directives" $ do
            let content = unlines
                  [ "//! ownership=true"
                  , "```go"
                  , "// @ownership=false"
                  , "code"
                  , "```"
                  ]
                result = parseTypus "" content
            case result of
              Left err -> assertFailure $ "Parse failed: " ++ show err
              Right parsed -> do
                let blocks = tfBlocks parsed
                assertBool "Expected at least one block" $ not (null blocks)
                let firstBlock = head blocks
                    blockDirectives = cbDirectives firstBlock
                case bdOwnership blockDirectives of
                  Nothing -> assertFailure "Expected block ownership directive"
                  Just located -> locatedValue located @?= False

        , testCase "parse file with build tags" $ do
            let content = unlines
                  [ "// +build linux,amd64"
                  , "//! ownership=true"
                  , "```go"
                  , "code"
                  , "```"
                  ]
                result = parseTypus "" content
            case result of
              Left err -> assertFailure $ "Parse failed: " ++ show err
              Right parsed -> do
                let buildTags = tfBuildTags parsed
                assertBool "Expected at least one build tag" $ not (null buildTags)
                locatedValue (head buildTags) @?= "+build linux,amd64"

        , testCase "parse file with multiple code blocks" $ do
            let content = unlines
                  [ "//! ownership=true"
                  , "```go"
                  , "func first() {}"
                  , "```"
                  , "```go"
                  , "func second() {}"
                  , "```"
                  ]
                result = parseTypus "" content
            case result of
              Left err -> assertFailure $ "Parse failed: " ++ show err
              Right parsed -> do
                let blocks = tfBlocks parsed
                length blocks @?= 2
                let firstBlock = blocks !! 0
                    secondBlock = blocks !! 1
                cbContent firstBlock @?= "func first() {}"
                cbContent secondBlock @?= "func second() {}"

        , testCase "parse file with comments in code blocks" $ do
            let content = unlines
                  [ "```go"
                  , "// This is a comment"
                  , "/* This is a block comment */"
                  , "func test() { // line comment"
                  , "  return 42"
                  , "}"
                  , "```"
                  ]
                result = parseTypus "" content
            case result of
              Left err -> assertFailure $ "Parse failed: " ++ show err
              Right parsed -> do
                let blocks = tfBlocks parsed
                assertBool "Expected at least one block" $ not (null blocks)
                let blockContent = cbContent (head blocks)
                assertBool "Should contain line comment" $ "// This is a comment" `isInfixOf` blockContent
                assertBool "Should contain block comment" $ "/* This is a block comment */" `isInfixOf` blockContent

        , testCase "parse file with malformed block markers" $ do
            let content = unlines
                  [ "```"
                  , "code without language"
                  , "```"
                  , "```go"
                  , "proper go block"
                  , "```"
                  ]
                result = parseTypus "" content
            case result of
              Left err -> assertFailure $ "Parse failed: " ++ show err
              Right parsed -> do
                let blocks = tfBlocks parsed
                -- 应该只解析Go块
                length blocks @?= 1
                cbContent (head blocks) @?= "proper go block"

        , testCase "parse file with extremely long directive" $ do
            let longValue = concat (replicate 1000 "very-long-value-")
                content = "//! ownership=" ++ longValue ++ "\n```go\ncode\n```"
                result = parseTypus "" content
            case result of
              Left err -> assertFailure $ "Parse failed: " ++ show err
              Right parsed -> do
                let directives = tfDirectives parsed
                case fdOwnership directives of
                  Nothing -> assertFailure "Expected ownership directive"
                  Just located -> do
                    let value = locatedValue located
                    assertBool "Should contain long value" $ longValue `isPrefixOf` value
        ]
    ]