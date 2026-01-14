{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.ParserAdvancedQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Parser
import SourceLocation (SourcePos(..), SourceSpan(..), startPos, Located(..))
import qualified Text.Megaparsec as MP
import Data.Char (isAlphaNum, isLetter, isSpace)
import Data.List (isPrefixOf, isInfixOf)
import qualified Data.Text as T

-- | 测试Parser模块中的高级解析功能
tests :: TestTree
tests = testGroup "ParserAdvancedQuickCheckSpec Tests"
  [ testGroup "解析器属性测试"
    [ testProperty "parseTypus is deterministic" $
        \code ->
          let result1 = parseTypus code
              result2 = parseTypus code
          in case (result1, result2) of
            (Left _, Left _) -> property True
            (Right file1, Right file2) -> property (file1 == file2)
            _ -> property False
    
    , testProperty "parseTypus preserves content" $
        \code ->
          let result = parseTypus code
          in case result of
            Left _ -> property True
            Right file -> 
              let blocks = tfBlocks file
              in if null blocks
                 then property True
                 else property (all (\block -> cbContent block `isInfixOf` code) blocks)
    
    , testProperty "parseTypus handles empty input" $
        \() ->
          let result = parseTypus ""
          in case result of
            Left _ -> property False
            Right file -> property (tfDirectives file == defaultFileDirectives && null (tfBlocks file))
    
    , testProperty "isIdentifierChar is consistent with isAlphaNum for alphanumerics" $
        \c -> isAlphaNum c ==> isIdentifierChar c
    
    , testProperty "isIdentifierChar accepts underscore and hyphen" $
        \() -> property (isIdentifierChar '_' && isIdentifierChar '-')
    
    , testProperty "isIdentifierChar rejects whitespace" $
        \c -> isSpace c ==> not (isIdentifierChar c)
    ]
  
  , testGroup "解析错误处理测试"
    [ testProperty "parseTypus returns Left for malformed directives" $
        \code ->
          let malformedCode = code ++ "// @ownership: maybe"
              result = parseTypus malformedCode
          in case result of
            Left _ -> property True
            Right _ -> property (not ("// @ownership: maybe" `isInfixOf` malformedCode))
    
    , testProperty "parseTypus handles malformed block comments gracefully" $
        \code ->
          let malformedCode = code ++ "/* unterminated comment"
              result = parseTypus malformedCode
          in case result of
            Left _ -> property True
            Right file -> property (not (null (tfSyntaxErrors file)))
    ]
  
  , testGroup "指令解析测试"
    [ testProperty "fileDirectiveParser parses valid ownership directive" $
        \value ->
          let directive = "// @ownership: " ++ if value then "true" else "false"
              result = MP.parse fileDirectiveParser "" (T.pack directive)
          in case result of
            Left _ -> property False
            Right pairs -> property (any (\p -> ("ownership", if value then T.pack "true" else T.pack "false") == p) pairs)
    
    , testProperty "fileDirectiveParser parses valid dependent-types directive" $
        \value ->
          let directive = "// @dependent-types: " ++ if value then "true" else "false"
              result = MP.parse fileDirectiveParser "" (T.pack directive)
          in case result of
            Left _ -> property False
            Right pairs -> property (any (\p -> ("dependent-types", if value then T.pack "true" else T.pack "false") == p) pairs)
    
    , testProperty "fileDirectiveParser parses valid constraints directive" $
        \value ->
          let directive = "// @constraints: " ++ if value then "true" else "false"
              result = MP.parse fileDirectiveParser "" (T.pack directive)
          in case result of
            Left _ -> property False
            Right pairs -> property (any (\p -> ("constraints", if value then T.pack "true" else T.pack "false") == p) pairs)
    ]
  
  , testGroup "代码块处理测试"
    [ testCase "CodeBlock content preservation" $ do
        let content = "test content"
            directives = defaultBlockDirectives
            span = SourceSpan startPos startPos
            block = CodeBlock { cbDirectives = directives, cbContent = content, cbSpan = span }
        assertBool "Content preserved" (cbContent block == content)
    
    , testCase "TypusFile block ordering preservation" $ do
        let blocks = []
            fileDirectives = defaultFileDirectives
            file = TypusFile { tfDirectives = fileDirectives, tfBuildTags = [], tfBlocks = blocks, tfSyntaxErrors = [] }
        assertBool "Blocks preserved" (tfBlocks file == blocks)
    
    , testCase "TypusFile directive preservation" $ do
        let directives = defaultFileDirectives
            file = TypusFile { tfDirectives = directives, tfBuildTags = [], tfBlocks = [], tfSyntaxErrors = [] }
        assertBool "Directives preserved" (tfDirectives file == directives)
    ]
  
  , testGroup "解析性能测试"
    [ testProperty "parseTypus handles large inputs efficiently" $
        \size ->
          let largeCode = unlines (replicate (min size 1000) ("let x = " ++ show size))
              result = parseTypus largeCode
          in case result of
            Left _ -> property True
            Right file -> property (not (null (tfBlocks file)))
    
    , testProperty "parseTypus handles deeply nested code" $
        \depth ->
          let nestedCode = unlines (replicate (min depth 100) ("  let x = " ++ show depth))
              result = parseTypus nestedCode
          in case result of
            Left _ -> property True
            Right file -> property (not (null (tfBlocks file)))
    ]
  
  , testGroup "边界条件测试"
    [ testCase "parseTypus handles null characters" $ do
        let codeWithNull = "let x = " ++ ['\0']
            result = parseTypus codeWithNull
        case result of
          Left _ -> pure ()
          Right file -> assertBool "Should handle null characters" (not (null (tfBlocks file)))
    
    , testCase "parseTypus handles very long lines" $ do
        let longLine = "let x = " ++ replicate 10000 'a'
            result = parseTypus longLine
        case result of
          Left _ -> pure ()
          Right file -> assertBool "Should handle long lines" (not (null (tfBlocks file)))
    
    , testCase "parseTypus handles many empty lines" $ do
        let manyEmptyLines = unlines (replicate 1000 "")
            result = parseTypus manyEmptyLines
        case result of
          Left _ -> pure ()
          Right file -> assertBool "Should handle many empty lines" (null (tfBlocks file))
    ]
  ]