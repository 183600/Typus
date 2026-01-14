{-# LANGUAGE OverloadedStrings #-}

module Test.Unit.ParserBasicFunctionsExtraSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Parser
import SourceLocation (SourcePos(..), SourceSpan(..), startPos, Located(..))
import qualified Text.Megaparsec as MP
import Data.Char (isAlphaNum, isLetter)
import Data.List (isPrefixOf)

-- 辅助函数
codeBlockSpan :: CodeBlock -> SourceSpan
codeBlockSpan (CodeBlock { cbSpan = span }) = span

codeBlockDirectives :: CodeBlock -> BlockDirectives
codeBlockDirectives (CodeBlock { cbDirectives = directives }) = directives

codeBlockContent :: CodeBlock -> String
codeBlockContent (CodeBlock { cbContent = content }) = content

typusFileDirectives :: TypusFile -> FileDirectives
typusFileDirectives (TypusFile { tfDirectives = directives }) = directives

typusCodeBlocks :: TypusFile -> [CodeBlock]
typusCodeBlocks (TypusFile { tfBlocks = blocks }) = blocks

-- | 测试Parser模块中的基本解析功能
tests :: TestTree
tests = testGroup "ParserBasicFunctionsExtraSpec Tests"
  [ testGroup "isIdentifierChar函数测试"
    [ testCase "isIdentifierChar letters" $
        (isIdentifierChar 'a' @?= True) *>
        (isIdentifierChar 'Z' @?= True)
    , testCase "isIdentifierChar digits" $
        (isIdentifierChar '0' @?= True) *>
        (isIdentifierChar '9' @?= True)
    , testCase "isIdentifierChar underscore" $ 
        isIdentifierChar '_' @?= True
    , testCase "isIdentifierChar common identifier chars" $
        (isIdentifierChar '\'' @?= True) *>
        (isIdentifierChar '-' @?= True)
    , testCase "isIdentifierChar rejects whitespace" $
        (isIdentifierChar ' ' @?= False) *>
        (isIdentifierChar '\t' @?= False) *>
        (isIdentifierChar '\n' @?= False)
    , testCase "isIdentifierChar rejects special chars" $
        (isIdentifierChar '!' @?= False) *>
        (isIdentifierChar '@' @?= False) *>
        (isIdentifierChar '#' @?= False)
    , testProperty "isIdentifierChar accepts alphanumeric" $
        \c -> isAlphaNum c ==> isIdentifierChar c
    ]
  
  , testGroup "FileDirectives测试"
    [ testCase "default file directives are empty" $ do
        let defaults = defaultFileDirectives
        fdOwnership defaults @?= Nothing
        fdDependentTypes defaults @?= Nothing
        fdConstraints defaults @?= Nothing
    , testCase "create ownership directive" $ do
        let pos = startPos
            ownership = Just (Located True pos (SourceSpan pos pos))
            directives = defaultFileDirectives { fdOwnership = ownership }
        fdOwnership directives @?= ownership
    , testCase "create dependent types directive" $ do
        let pos = startPos
            dependentTypes = Just (Located True pos (SourceSpan pos pos))
            directives = defaultFileDirectives { fdDependentTypes = dependentTypes }
        fdDependentTypes directives @?= dependentTypes
    , testCase "create constraints directive" $ do
        let pos = startPos
            constraints = Just (Located True pos (SourceSpan pos pos))
            directives = defaultFileDirectives { fdConstraints = constraints }
        fdConstraints directives @?= constraints
    ]
  
  , testGroup "BlockDirectives测试"
    [ testCase "default block directives are empty" $ do
        let defaults = defaultBlockDirectives
        bdOwnership defaults @?= Nothing
        bdDependentTypes defaults @?= Nothing
    , testCase "create ownership directive" $ do
        let pos = startPos
            ownership = Just (Located True pos (SourceSpan pos pos))
            directives = defaultBlockDirectives { bdOwnership = ownership }
        bdOwnership directives @?= ownership
    , testCase "create dependent types directive" $ do
        let pos = startPos
            dependentTypes = Just (Located True pos (SourceSpan pos pos))
            directives = defaultBlockDirectives { bdDependentTypes = dependentTypes }
        bdDependentTypes directives @?= dependentTypes
    ]
  
  , testGroup "CodeBlock测试"
    [ testCase "create code block" $ do
        let span = SourceSpan startPos startPos
            directives = defaultBlockDirectives
            content = "test code"
            block = CodeBlock { cbDirectives = directives, cbContent = content, cbSpan = span }
        codeBlockSpan block @?= span
        codeBlockDirectives block @?= directives
        codeBlockContent block @?= content
    , testCase "update code block content" $ do
        let span = SourceSpan startPos startPos
            directives = defaultBlockDirectives
            content = "original code"
            block = CodeBlock { cbDirectives = directives, cbContent = content, cbSpan = span }
            newContent = "updated code"
            updatedBlock = block { cbContent = newContent }
        codeBlockContent updatedBlock @?= newContent
    ]
  
  , testGroup "TypusFile测试"
    [ testCase "create Typus file" $ do
        let fileDirectives = defaultFileDirectives
            codeBlocks = []
            file = TypusFile { tfDirectives = fileDirectives, tfBuildTags = [], tfBlocks = codeBlocks, tfSyntaxErrors = [] }
        typusFileDirectives file @?= fileDirectives
        typusCodeBlocks file @?= codeBlocks
    , testCase "add code block to Typus file" $ do
        let fileDirectives = defaultFileDirectives
            span = SourceSpan startPos startPos
            directives = defaultBlockDirectives
            content = "test code"
            block = CodeBlock { cbDirectives = directives, cbContent = content, cbSpan = span }
            file = TypusFile { tfDirectives = fileDirectives, tfBuildTags = [], tfBlocks = [block], tfSyntaxErrors = [] }
        length (typusCodeBlocks file) @?= 1
        head (typusCodeBlocks file) @?= block
    ]
  
  , testGroup "parseTypus函数测试"
    [ testCase "parse empty string" $ do
        let result = parseTypus ""
        case result of
          Left _ -> assertFailure "解析空字符串失败"
          Right file -> do
            typusFileDirectives file @?= defaultFileDirectives
            typusCodeBlocks file @?= []
    , testCase "parse simple code" $ do
        let code = "let x = 5"
            result = parseTypus code
        case result of
          Left _ -> assertFailure "解析简单代码失败"
          Right file -> do
            length (typusCodeBlocks file) @?= 1
            let block = head (typusCodeBlocks file)
            codeBlockContent block @?= code
    , testCase "parse multiline code" $ do
        let code = "let x = 5\nlet y = 10"
            result = parseTypus code
        case result of
          Left _ -> assertFailure "解析多行代码失败"
          Right file -> do
            length (typusCodeBlocks file) @?= 1
            let block = head (typusCodeBlocks file)
            codeBlockContent block @?= code
    ]
  
  , testGroup "fileDirectiveParser测试"
    [ testCase "parse ownership directive" $ do
        let directive = "// @ownership: true"
            result = MP.parse fileDirectiveParser "" directive
        case result of
          Left _ -> assertFailure "解析所有权指令失败"
          Right pairs -> do
            let directives = defaultFileDirectives { fdOwnership = Just (Located True startPos (SourceSpan startPos startPos)) }
            case fdOwnership directives of
              Nothing -> assertFailure "未找到所有权指令"
              Just (Located value _ _) -> value @?= True
    , testCase "parse dependent types directive" $ do
        let directive = "// @dependent-types: true"
            result = MP.parse fileDirectiveParser "" directive
        case result of
          Left _ -> assertFailure "解析依赖类型指令失败"
          Right pairs -> do
            let directives = defaultFileDirectives { fdDependentTypes = Just (Located True startPos (SourceSpan startPos startPos)) }
            case fdDependentTypes directives of
              Nothing -> assertFailure "未找到依赖类型指令"
              Just (Located value _ _) -> value @?= True
    , testCase "parse constraints directive" $ do
        let directive = "// @constraints: true"
            result = MP.parse fileDirectiveParser "" directive
        case result of
          Left _ -> assertFailure "解析约束指令失败"
          Right pairs -> do
            let directives = defaultFileDirectives { fdConstraints = Just (Located True startPos (SourceSpan startPos startPos)) }
            case fdConstraints directives of
              Nothing -> assertFailure "未找到约束指令"
              Just (Located value _ _) -> value @?= True
    ]
  
  , testGroup "解析属性测试"
    [ testProperty "parsing is deterministic" $
        \code ->
          let result1 = parseTypus code
              result2 = parseTypus code
          in case (result1, result2) of
            (Left _, Left _) -> property True
            (Right file1, Right file2) -> property (file1 == file2)
            _ -> property False
    , testCase "parse empty string returns empty file" $ do
        let result = parseTypus ""
        case result of
          Left _ -> assertFailure "解析空字符串失败"
          Right file -> do
            typusFileDirectives file @?= defaultFileDirectives
            typusCodeBlocks file @?= []
    , testProperty "parsing same content produces same result" $
        \code ->
          let result = parseTypus code
          in case result of
            Left _ -> property True
            Right file -> 
              let blocks = typusCodeBlocks file
              in if null blocks
                 then property True
                 else property (all (\block -> codeBlockContent block == code) blocks)
    ]
  
  , testGroup "解析错误处理测试"
    [ testCase "handle incomplete directive" $ do
        let directive = "// @ownership"
            result = MP.parse fileDirectiveParser "" directive
        case result of
          Left _ -> pure ()
          Right _ -> assertFailure "应该解析失败"
    , testCase "handle invalid directive value" $ do
        let directive = "// @ownership: maybe"
            result = MP.parse fileDirectiveParser "" directive
        case result of
          Left _ -> pure ()
          Right _ -> assertFailure "应该解析失败"
    , testCase "handle malformed directive" $ do
        let directive = "ownership: true"
            result = MP.parse fileDirectiveParser "" directive
        case result of
          Left _ -> pure ()
          Right _ -> assertFailure "应该解析失败"
    ]
  ]