module Test.Unit.ParserBoundarySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, assertFailure, testCase)

import Parser
  ( BlockDirectives(..)
  , CodeBlock(..)
  , FileDirectives(..)
  , TypusFile(..)
  , parseTypus
  , defaultFileDirectives
  , defaultBlockDirectives
  )
import SourceLocation
  ( Located(..)
  , SourcePos(..)
  , SourceSpan(..)
  , locatedValue
  , spanEnd
  , spanStart
  )
import qualified Data.Text as T

-- | 测试解析器在边界条件下的行为
tests :: TestTree
tests =
  testGroup "Parser Boundary Tests"
    [ -- 空文件和最小输入测试
      testCase "parses empty file" $ do
        let source = ""
            result = parseTypus source
        case result of
          Left err -> assertFailure $ "Failed to parse empty file: " ++ show err
          Right typusFile -> do
            tfFileDirectives typusFile @?= defaultFileDirectives
            tfCodeBlocks typusFile @?= []

    , testCase "parses file with only whitespace" $ do
        let source = "   \n  \t \n   "
            result = parseTypus source
        case result of
          Left err -> assertFailure $ "Failed to parse whitespace-only file: " ++ show err
          Right typusFile -> do
            tfFileDirectives typusFile @?= defaultFileDirectives
            tfCodeBlocks typusFile @?= []

    , testCase "parses file with only comments" $ do
        let source = unlines
              [ "// This is a comment"
              , "/* This is a block comment */"
              , "// Another comment"
              ]
            result = parseTypus source
        case result of
          Left err -> assertFailure $ "Failed to parse comment-only file: " ++ show err
          Right typusFile -> do
            tfFileDirectives typusFile @?= defaultFileDirectives
            tfCodeBlocks typusFile @?= []

    -- 指令解析边界测试
    , testCase "parses malformed directives gracefully" $ do
        let source = unlines
              [ "//! ownership: maybe"
              , "//! dependent_types: sometimes"
              , "package main"
              , "func main() {}"
              ]
            result = parseTypus source
        -- 应该能解析，但指令可能被忽略或设为默认值
        case result of
          Left err -> assertFailure $ "Failed to parse malformed directives: " ++ show err
          Right _ -> return ()

    , testCase "handles directives with extra spaces" $ do
        let source = unlines
              [ "//!   ownership   :   on   "
              , "//!dependent_types:off"
              , "package main"
              , "func main() {}"
              ]
            result = parseTypus source
        case result of
          Left err -> assertFailure $ "Failed to parse directives with extra spaces: " ++ show err
          Right typusFile -> do
            let fd = tfFileDirectives typusFile
            case fdOwnership fd of
              Just (Located _ True) -> return ()
              _ -> assertFailure "Expected ownership to be True"
            case fdDependentTypes fd of
              Just (Located _ False) -> return ()
              _ -> assertFailure "Expected dependent_types to be False"

    -- 代码块边界测试
    , testCase "parses code blocks with various indentations" $ do
        let source = unlines
              [ "package main"
              , ""
              , "func main() {"
              , "  println(\"Hello\")"
              , "    println(\"Indented\")"
              , "\tprintln(\"Tabbed\")"
              , "}"
              ]
            result = parseTypus source
        case result of
          Left err -> assertFailure $ "Failed to parse mixed indentation: " ++ show err
          Right typusFile -> do
            let blocks = tfCodeBlocks typusFile
            assertBool "Expected at least one code block" (not (null blocks))
            let mainBlock = head blocks
                blockContent = cbContent mainBlock
            assertBool "Expected package declaration" ("package main" `isInfixOf` blockContent)
            assertBool "Expected main function" ("func main()" `isInfixOf` blockContent)

    , testCase "handles unmatched braces gracefully" $ do
        let source = unlines
              [ "package main"
              , "func main() {"
              , "  println(\"Hello\")"
              , "  // Missing closing brace"
              ]
            result = parseTypus source
        -- 应该能解析，但可能有语法错误
        case result of
          Left err -> assertFailure $ "Failed to parse unmatched braces: " ++ show err
          Right _ -> return ()

    -- Unicode和特殊字符测试
    , testCase "handles Unicode characters in comments" $ do
        let source = unlines
              [ "// 你好世界"
              , "/* 这是一个测试 */"
              , "package main"
              , "func main() {"
              , "  // 输出: Hello, 世界!"
              , "  println(\"Hello, 世界!\")"
              , "}"
              ]
            result = parseTypus source
        case result of
          Left err -> assertFailure $ "Failed to parse Unicode comments: " ++ show err
          Right typusFile -> do
            let blocks = tfCodeBlocks typusFile
            assertBool "Expected at least one code block" (not (null blocks))
            let mainBlock = head blocks
                blockContent = cbContent mainBlock
            assertBool "Expected Unicode string literal" ("世界" `isInfixOf` blockContent)

    , testCase "handles special characters in strings" $ do
        let source = unlines
              [ "package main"
              , "func main() {"
              , "  println(\"Line 1\nLine 2\tTabbed\")"
              , "  println(\"Quote: \\\" and backslash: \\\")"
              , "}"
              ]
            result = parseTypus source
        case result of
          Left err -> assertFailure $ "Failed to parse escaped characters: " ++ show err
          Right typusFile -> do
            let blocks = tfCodeBlocks typusFile
            let mainBlock = head blocks
                blockContent = cbContent mainBlock
            assertBool "Expected escaped newline" ("\\n" `isInfixOf` blockContent)
            assertBool "Expected escaped quote" ("\\\"" `isInfixOf` blockContent)

    -- 大文件性能测试
    , testCase "handles large files efficiently" $ do
        let largeFunction = "  println(\"Test line\")\n"
            source = unlines $
              [ "package main"
              , "func main() {"
              ] ++ replicate 1000 largeFunction ++
              [ "}"
              ]
            result = parseTypus source
        case result of
          Left err -> assertFailure $ "Failed to parse large file: " ++ show err
          Right typusFile -> do
            let blocks = tfCodeBlocks typusFile
            assertBool "Expected at least one code block" (not (null blocks))
            let mainBlock = head blocks
                blockContent = cbContent mainBlock
            -- 检查是否包含了大部分内容
            let lineCount = length $ lines blockContent
            assertBool "Expected many lines in large file" (lineCount > 900)

    -- 嵌套结构测试
    , testCase "handles deeply nested structures" $ do
        let source = unlines
              [ "package main"
              , "func main() {"
              , "  if true {"
              , "    for i := 0; i < 10; i++ {"
              , "      switch i {"
              , "        case 1:"
              , "          if false {"
              , "            println(\"Deeply nested\")"
              , "          }"
              , "      }"
              , "    }"
              , "  }"
              , "}"
              ]
            result = parseTypus source
        case result of
          Left err -> assertFailure $ "Failed to parse nested structures: " ++ show err
          Right typusFile -> do
            let blocks = tfCodeBlocks typusFile
            let mainBlock = head blocks
                blockContent = cbContent mainBlock
            assertBool "Expected nested if" ("if true" `isInfixOf` blockContent)
            assertBool "Expected nested for" ("for i := 0" `isInfixOf` blockContent)
            assertBool "Expected nested switch" ("switch i" `isInfixOf` blockContent)
            assertBool "Expected deeply nested println" ("Deeply nested" `isInfixOf` blockContent)
    ]
  where
    isInfixOf needle haystack = needle `T.isInfixOf` T.pack haystack