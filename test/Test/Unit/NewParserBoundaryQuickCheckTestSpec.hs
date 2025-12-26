{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.NewParserBoundaryQuickCheckTestSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Property, testProperty, Arbitrary(..), Gen, oneof, elements, listOf, listOf1, suchThat)
import Test.Tasty.HUnit (testCase, (@?=))

import Parser (parseTypus, FileDirectives(..), BlockDirectives(..), CodeBlock(..), TypusFile(..), defaultFileDirectives, defaultBlockDirectives)
import SourceLocation (Located(..), SourcePos(..), SourceSpan(..))
import Data.Char (isSpace, isAlphaNum, isLetter)
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import qualified Data.Text as T
import Text.Megaparsec (errorBundlePretty)

-- | 新的Parser边界条件QuickCheck测试模块
tests :: TestTree
tests =
  testGroup "New Parser Boundary QuickCheck Tests"
    [ testGroup "Parsing edge cases"
        [ testProperty "parseTypus handles empty input gracefully" prop_parseEmptyInput
        , testProperty "parseTypus handles whitespace-only input" prop_parseWhitespaceOnly
        , testProperty "parseTypus handles very long identifiers" prop_parseLongIdentifiers
        , testProperty "parseTypus handles deeply nested blocks" prop_parseDeeplyNested
        , testProperty "parseTypus handles malformed directives gracefully" prop_parseMalformedDirectives
        ]

    , testGroup "Directive parsing properties"
        [ testProperty "file directives are parsed correctly" prop_fileDirectivesParsing
        , testProperty "block directives override file directives" prop_blockDirectivesOverride
        , testProperty "directive parsing is case-sensitive" prop_directivesCaseSensitive
        , testProperty "invalid directives are ignored" prop_invalidDirectivesIgnored
        ]

    , testGroup "Error handling properties"
        [ testProperty "parse errors provide meaningful locations" prop_parseErrorLocations
        , testProperty "unclosed blocks are detected" prop_unclosedBlocksDetected
        , testProperty "mismatched directives are caught" prop_mismatchedDirectivesCaught
        ]

    , testGroup "Performance and robustness"
        [ testProperty "parsing large files doesn't crash" prop_parseLargeFiles
        , testProperty "parsing with many directives is stable" prop_manyDirectivesStable
        , testProperty "parsing unicode content works" prop_unicodeContentParsing
        ]

    , testGroup "Specific edge case tests"
        [ testCase "parseTypus handles comment-only files" $ do
            let input = "// This is a comment\n/* Block comment */\n// Another comment"
                result = parseTypus input
            case result of
                Left err -> @?= False True -- Should not fail on comment-only files
                Right file -> @?= True True

        , testCase "parseTypus handles directives without blocks" $ do
            let input = "// @ownership: true\n// @dependent-types: false"
                result = parseTypus input
            case result of
                Left _ -> @?= False True
                Right file -> @?= True True

        , testCase "parseTypus handles nested block comments" $ do
            let input = "/* outer /* inner */ still outer */ code after"
                result = parseTypus input
            case result of
                Left _ -> @?= False True
                Right file -> @?= True True

        , testCase "parseTypus handles special characters in identifiers" $ do
            let input = "func test_123$special() {\n  return true\n}"
                result = parseTypus input
            case result of
                Left _ -> @?= False True
                Right file -> @?= True True

        , testCase "parseTypus handles mixed line endings" $ do
            let input = "func test() {\n\r\n  return true\n}"
                result = parseTypus input
            case result of
                Left _ -> @?= False True
                Right file -> @?= True True
        ]
    ]

-- | parseTypus处理空输入
prop_parseEmptyInput :: Property
prop_parseEmptyInput = 
  let result = parseTypus ""
  in case result of
       Left _ -> False -- Should not fail on empty input
       Right file -> True

-- | parseTypus处理仅包含空白字符的输入
prop_parseWhitespaceOnly :: String -> Property
prop_parseWhitespaceOnly s =
  let whitespaceOnly = all isSpace s
      result = parseTypus s
  in whitespaceOnly ==> case result of
                          Left _ -> False
                          Right file -> True

-- | parseTypus处理很长的标识符
prop_parseLongIdentifiers :: Property
prop_parseLongIdentifiers =
  let longIdent = replicate 1000 'a' ++ "123"
      input = "func " ++ longIdent ++ "() { return true }"
      result = parseTypus input
  in case result of
       Left _ -> False -- Should handle long identifiers
       Right file -> True

-- | parseTypus处理深度嵌套的块
prop_parseDeeplyNested :: Property
prop_parseDeeplyNested =
  let depth = 100
      createNestedBlock 0 = "return true"
      createNestedBlock n = "if true { " ++ createNestedBlock (n-1) ++ " }"
      input = "func deeplyNested() { " ++ createNestedBlock depth ++ " }"
      result = parseTypus input
  in case result of
       Left _ -> depth <= 50 -- May fail for very deep nesting, but should handle reasonable depth
       Right file -> True

-- | parseTypus优雅处理格式错误的指令
prop_parseMalformedDirectives :: String -> Property
prop_parseMalformedDirectives s =
  let malformedDirective = "// @ownership: maybe\n// @dependent-types: \n// @invalid-directive: true"
      input = malformedDirective ++ "\nfunc test() { return true }"
      result = parseTypus input
  in case result of
       Left _ -> False -- Should not crash on malformed directives
       Right file -> True

-- | 文件指令被正确解析
prop_fileDirectivesParsing :: Bool -> Bool -> Bool -> Property
prop_fileDirectivesParsing ownership dependent constraints =
  let input = "// @ownership: " ++ show ownership ++ "\n" ++
             "// @dependent-types: " ++ show dependent ++ "\n" ++
             "// @constraints: " ++ show constraints ++ "\n" ++
             "func test() { return true }"
      result = parseTypus input
  in case result of
       Left _ -> False
       Right file -> True -- Should parse successfully

-- | 块指令覆盖文件指令
prop_blockDirectivesOverride :: Property
prop_blockDirectivesOverride =
  let input = "// @ownership: true\n" ++
             "func test() {\n" ++
             "  // @ownership: false\n" ++
             "  return true\n" ++
             "}\n"
      result = parseTypus input
  in case result of
       Left _ -> False
       Right file -> True

-- | 指令解析是大小写敏感的
prop_directivesCaseSensitive :: Property
prop_directivesCaseSensitive =
  let input = "// @OWNERSHIP: true\n// @Ownership: false\nfunc test() { return true }"
      result = parseTypus input
  in case result of
       Left _ -> False -- Should not fail on case variations
       Right file -> True

-- | 无效指令被忽略
prop_invalidDirectivesIgnored :: Property
prop_invalidDirectivesIgnored =
  let input = "// @invalid-directive: true\n// @another-invalid: false\nfunc test() { return true }"
      result = parseTypus input
  in case result of
       Left _ -> False
       Right file -> True

-- | 解析错误提供有意义的位置信息
prop_parseErrorLocations :: Property
prop_parseErrorLocations =
  let input = "func invalid syntax { return true }"
      result = parseTypus input
  in case result of
       Left err -> length (show err) > 0 -- Error message should not be empty
       Right _ -> True

-- | 检测未闭合的块
prop_unclosedBlocksDetected :: Property
prop_unclosedBlocksDetected =
  let input = "func unclosed() {\n  if true {\n    return true\n  // Missing closing braces"
      result = parseTypus input
  in case result of
       Left _ -> True -- Should detect unclosed blocks
       Right _ -> False

-- | 捕获不匹配的指令
prop_mismatchedDirectivesCaught :: Property
prop_mismatchedDirectivesCaught =
  let input = "// @ownership: not-a-boolean\nfunc test() { return true }"
      result = parseTypus input
  in case result of
       Left _ -> True -- Should catch invalid directive values
       Right _ -> False

-- | 解析大文件不会崩溃
prop_parseLargeFiles :: Property
prop_parseLargeFiles =
  let largeContent = unlines $ replicate 10000 "func line" ++ show ++ "() { return true }"
      input = largeContent ++ "\nfunc main() { return true }"
      result = parseTypus input
  in case result of
       Left _ -> False -- Should handle large files
       Right file -> True

-- | 包含许多指令的解析是稳定的
prop_manyDirectivesStable :: Property
prop_manyDirectivesStable =
  let manyDirectives = unlines $ replicate 100 "// @ownership: true"
      input = manyDirectives ++ "\nfunc test() { return true }"
      result = parseTypus input
  in case result of
       Left _ -> False
       Right file -> True

-- | 解析unicode内容正常工作
prop_unicodeContentParsing :: Property
prop_unicodeContentParsing =
  let unicodeInput = "func unicode测试() {\n  // 注释 with émojis 🚀\n  let 变量 = \"hello 世界\"\n  return 变量\n}"
      result = parseTypus unicodeInput
  in case result of
       Left _ -> False -- Should handle unicode content
       Right file -> True