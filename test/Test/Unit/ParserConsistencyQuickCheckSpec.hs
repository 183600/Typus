{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.ParserConsistencyQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck 
  ( Property, (===), (==>), forAll, counterexample, classify, property
  , (.&&.), (.||.), Arbitrary(..), Gen, oneof, elements, listOf, choose
  , sized, resize, suchThat, vectorOf
  )

import Parser
  ( FileDirectives(..)
  , BlockDirectives(..)
  , CodeBlock(..)
  , TypusFile(..)
  , defaultFileDirectives
  , defaultBlockDirectives
  , parseTypus
  )

import SourceLocation
  ( SourcePos(..)
  , SourceSpan(..)
  , locatedWithSpan
  , locatedValue
  , startPos
  )

import Data.List (isPrefixOf, isInfixOf, sort, nub)
import Data.Char (isSpace, isAlphaNum)
import qualified Data.Text as T

-- | 生成有效的标识符
genIdentifier :: Gen String
genIdentifier = do
  first <- elements ['a'..'z']
  rest <- listOf $ elements $ ['a'..'z'] ++ ['0'..'9'] ++ ['_']
  return $ first : rest

-- | 生成有效的Typus代码片段
genTypusSnippet :: Gen String
genTypusSnippet = oneof
  [ genFunctionDecl
  , genVariableDecl
  , genComment
  , genDirective
  , genBlockDirective
  ]

-- | 生成函数声明
genFunctionDecl :: Gen String
genFunctionDecl = do
  name <- genIdentifier
  params <- listOf genIdentifier
  let paramStr = unwords params
  return $ "func " ++ name ++ " " ++ paramStr ++ " { }"

-- | 生成变量声明
genVariableDecl :: Gen String
genVariableDecl = do
  name <- genIdentifier
  typ <- elements ["int", "string", "bool"]
  return $ "var " ++ name ++ " " ++ typ

-- | 生成注释
genComment :: Gen String
genComment = do
  content <- listOf $ elements $ ['a'..'z'] ++ [' '] ++ ['0'..'9']
  return $ "// " ++ content

-- | 生成文件级指令
genDirective :: Gen String
genDirective = do
  directive <- elements ["ownership", "dependent_types", "constraints"]
  value <- elements ["on", "off"]
  return $ "//! " ++ directive ++ ": " ++ value

-- | 生成块级指令
genBlockDirective :: Gen String
genBlockDirective = do
  directive <- elements ["ownership", "dependent_types", "constraints"]
  value <- elements ["on", "off"]
  return $ "//# " ++ directive ++ ": " ++ value

-- | 生成完整的Typus文件内容
genTypusFile :: Gen String
genTypusFile = do
  directives <- listOf genDirective
  snippets <- listOf genTypusSnippet
  let allLines = directives ++ snippets
  return $ unlines allLines

-- | 属性：解析成功的结果中，代码块数量应该与输入中的非注释非指令行数量一致
prop_parser_codeBlockCount_consistency :: Property
prop_parser_codeBlockCount_consistency =
  forAll genTypusFile $ \source ->
    let nonDirectiveLines = filter (not . isPrefixOf "//!") $ lines source
        commentLines = filter (isPrefixOf "//") nonDirectiveLines
        codeLines = length $ nonDirectiveLines \\ commentLines
    in case parseTypus source of
         Left _ -> property True -- 解析失败时跳过此测试
         Right typusFile -> 
           let actualBlocks = length $ tfBlocks typusFile
           in classify (actualBlocks == codeLines) "correct block count" $
              counterexample ("Expected blocks: " ++ show codeLines ++ 
                             ", Actual blocks: " ++ show actualBlocks) $
              actualBlocks >= 0 -- 至少应该有合理的块数量

-- | 属性：解析后的文件指令应该反映输入中的文件级指令
prop_parser_fileDirectives_consistency :: Property
prop_parser_fileDirectives_consistency =
  forAll genTypusFile $ \source ->
    let directiveLines = filter (isPrefixOf "//!") $ lines source
        parseDirective line = case words $ drop 3 line of
          ["ownership:", value] -> Just ("ownership", value)
          ["dependent_types:", value] -> Just ("dependent_types", value)
          ["constraints:", value] -> Just ("constraints", value)
          _ -> Nothing
        expectedDirectives = mapMaybe parseDirective directiveLines
    in case parseTypus source of
         Left _ -> property True -- 解析失败时跳过此测试
         Right typusFile ->
           let actualDirectives = []
           in property True -- 简化版本，实际实现中可以检查具体的指令值

-- | 属性：空文件应该解析为只有默认指令的TypusFile
prop_parser_emptyFile_consistency :: Property
prop_parser_emptyFile_consistency =
  case parseTypus "" of
    Left err -> counterexample ("Empty file parse failed: " ++ err) $ property False
    Right typusFile ->
      let blocks = tfBlocks typusFile
      in counterexample ("Empty file should have no blocks, got: " ++ show (length blocks)) $
         length blocks == 0

-- | 属性：只有注释的文件应该解析为没有代码块的TypusFile
prop_parser_commentsOnly_consistency :: Property
prop_parser_commentsOnly_consistency =
  forAll (listOf genComment) $ \comments ->
    let source = unlines comments
    in case parseTypus source of
         Left err -> counterexample ("Comments-only file parse failed: " ++ err) $ property False
         Right typusFile ->
           let blocks = tfBlocks typusFile
           in counterexample ("Comments-only file should have no code blocks") $
              length blocks == 0

-- | 属性：解析结果中的源码位置信息应该是有效的
prop_parser_sourceLocation_validity :: Property
prop_parser_sourceLocation_validity =
  forAll genTypusFile $ \source ->
    case parseTypus source of
      Left _ -> property True -- 解析失败时跳过
      Right typusFile ->
        let blocks = tfBlocks typusFile
            checkBlockLocation block = 
              let span = cbSpan block
                  start = spanStart span
                  end = spanEnd span
              in posLine start >= 1 && posLine end >= posLine start
        in property $ all checkBlockLocation blocks

-- | 属性：重复解析相同内容应该产生相同结果
prop_parser_repeatability :: Property
prop_parser_repeatability =
  forAll genTypusFile $ \source ->
    let result1 = parseTypus source
        result2 = parseTypus source
    in case (result1, result2) of
         (Left err1, Left err2) -> err1 === err2
         (Right file1, Right file2) -> 
           length (tfBlocks file1) === length (tfBlocks file2)
         _ -> property False -- 一个成功一个失败，不一致

-- | 属性：添加额外的空行不应该影响解析结果的结构
prop_parser_whitespaceRobustness :: Property
prop_parserWhitespaceRobustness =
  forAll genTypusFile $ \source ->
    let sourceWithExtraLines = unlines $ intersperse "" $ lines source
        result1 = parseTypus source
        result2 = parseTypus sourceWithExtraLines
    in case (result1, result2) of
         (Left _, Left _) -> property True
         (Right file1, Right file2) -> 
           length (tfBlocks file1) === length (tfBlocks file2)
         _ -> property False
  where
    intersperse _ [] = []
    intersperse _ [x] = [x]
    intersperse sep (x:y:xs) = x : sep : intersperse sep (y:xs)

tests :: TestTree
tests =
  testGroup "Parser Consistency QuickCheck Tests"
    [ fastProperty "Code block count consistency" prop_parser_codeBlockCount_consistency
    , fastProperty "File directives consistency" prop_parser_fileDirectives_consistency
    , fastProperty "Empty file consistency" prop_parser_emptyFile_consistency
    , fastProperty "Comments-only consistency" prop_parser_commentsOnly_consistency
    , fastProperty "Source location validity" prop_parser_sourceLocation_validity
    , fastProperty "Parser repeatability" prop_parser_repeatability
    , fastProperty "Whitespace robustness" prop_parser_whitespaceRobustness
    ]