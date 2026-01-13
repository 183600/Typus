module Test.Unit.ParserAdvancedQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Parser
  ( parseTypus
  , FileDirectives(..)
  , BlockDirectives(..)
  , CodeBlock(..)
  , TypusFile(..)
  , defaultFileDirectives
  , defaultBlockDirectives
  )
import SourceLocation (SourcePos(..), SourceSpan(..))
import Utils (trim)
import Data.List (isPrefixOf, isInfixOf)
import qualified Data.Text as T

-- | 生成简单的Typus代码
newtype SimpleTypusCode = SimpleTypusCode { getSimpleTypusCode :: String }
  deriving Show

instance Arbitrary SimpleTypusCode where
  arbitrary = do
    numLines <- choose (1, 5)
    lines <- vectorOf numLines $ do
      lineType <- elements ["var", "func", "comment", "empty"]
      case lineType of
        "var" -> do
          varName <- elements ["x", "y", "z", "result", "value"]
          value <- elements ["0", "1", "42", "100"]
          return $ "let " ++ varName ++ " = " ++ value
        "func" -> do
          funcName <- elements ["foo", "bar", "baz", "test"]
          return $ "func " ++ funcName ++ "() { }"
        "comment" -> do
          comment <- elements ["// line comment", "/* block comment */"]
          return comment
        "empty" -> return ""
        _ -> return ""
    return $ SimpleTypusCode $ unlines lines

-- | 生成包含指令的Typus代码
newtype DirectiveTypusCode = DirectiveTypusCode { getDirectiveTypusCode :: String }
  deriving Show

instance Arbitrary DirectiveTypusCode where
  arbitrary = do
    hasFileDirective <- arbitrary
    hasBlockDirective <- arbitrary
    let fileDirective = if hasFileDirective
                       then "//! ownership: on, dependent_types: on\n"
                       else ""
        blockDirective = if hasBlockDirective
                        then "{//! ownership: on, dependent_types: on\nlet x = 42\n}\n"
                        else "let x = 42\n"
    return $ DirectiveTypusCode $ fileDirective ++ blockDirective

-- | 生成可能包含语法错误的Typus代码
newtype ErrorTypusCode = ErrorTypusCode { getErrorTypusCode :: String }
  deriving Show

instance Arbitrary ErrorTypusCode where
  arbitrary = do
    errorType <- elements ["unclosed_brace", "invalid_directive", "bad_syntax"]
    case errorType of
      "unclosed_brace" -> do
        return $ ErrorTypusCode "func test() {  // missing closing brace"
      "invalid_directive" -> do
        return $ ErrorTypusCode "//! invalid_key: invalid_value"
      "bad_syntax" -> do
        return $ ErrorTypusCode "if condition {  // missing condition body"
      _ -> return $ ErrorTypusCode ""

-- | 测试parseTypus函数的基本属性
prop_parse_typus_empty_string :: Property
prop_parse_typus_empty_string =
  case parseTypus "" of
    Left _ -> property False
    Right file -> 
      tfDirectives file === defaultFileDirectives .&&.
      null (tfBuildTags file) .&&.
      null (tfBlocks file)

prop_parse_typus_simple_code :: SimpleTypusCode -> Property
prop_parse_typus_simple_code (SimpleTypusCode code) =
  case parseTypus code of
    Left _ -> property False
    Right file -> 
      let numLines = length $ filter (not . null . trim) $ lines code
          numBlocks = length $ tfBlocks file
      in numBlocks >= 0 .&&. numBlocks <= numLines

prop_parse_typus_preserves_non_comment_lines :: SimpleTypusCode -> Property
prop_parse_typus_preserves_non_comment_lines (SimpleTypusCode code) =
  case parseTypus code of
    Left _ -> property False
    Right file -> 
      let originalLines = filter (not . isCommentLine . trim) $ lines code
          blockContents = map cbContent $ tfBlocks file
          nonEmptyBlocks = filter (not . null . trim) blockContents
      in length nonEmptyBlocks <= length originalLines
  where
    isCommentLine line = "//" `isPrefixOf` line || "/*" `isPrefixOf` line

prop_parse_typus_directives :: DirectiveTypusCode -> Property
prop_parse_typus_directives (DirectiveTypusCode code) =
  case parseTypus code of
    Left _ -> property False
    Right file -> 
      let hasFileDirective = "//! ownership: on" `isInfixOf` code
          hasBlockDirective = "{//! ownership: on" `isInfixOf` code
          fileOwnership = fdOwnership $ tfDirectives file
          blockOwnership = if null (tfBlocks file) 
                          then Nothing 
                          else bdOwnership $ cbDirectives (head $ tfBlocks file)
      in if hasFileDirective
         then fileOwnership /= Nothing
         else property True .&&.
              if hasBlockDirective
              then blockOwnership /= Nothing
              else property True

prop_parse_typus_error_handling :: ErrorTypusCode -> Property
prop_parse_typus_error_handling (ErrorTypusCode code) =
  case parseTypus code of
    Left err -> not (null err)
    Right _ -> property True

prop_parse_typus_multiline_comments :: Property
prop_parse_typus_multiline_comments =
  let code = "/* This is a\n   multiline comment */\nlet x = 42"
  in case parseTypus code of
    Left _ -> property False
    Right file -> 
      let blocks = tfBlocks file
      in not (null blocks) ==> 
         let content = cbContent $ head blocks
         in "let x = 42" `isInfixOf` content

prop_parse_typus_line_comments :: Property
prop_parse_typus_line_comments =
  let code = "// This is a line comment\nlet x = 42 // another comment"
  in case parseTypus code of
    Left _ -> property False
    Right file -> 
      let blocks = tfBlocks file
      in not (null blocks) ==> 
         let content = cbContent $ head blocks
         in "let x = 42" `isInfixOf` content && 
            not ("// another comment" `isInfixOf` content)

prop_parse_typus_string_literals :: Property
prop_parse_typus_string_literals =
  let code = "let s = \"This is a // string, not a comment\"\nlet x = 42"
  in case parseTypus code of
    Left _ -> property False
    Right file -> 
      let blocks = tfBlocks file
      in not (null blocks) ==> 
         let content = cbContent $ head blocks
         in "\"This is a // string, not a comment\"" `isInfixOf` content

prop_parse_typus_build_tags :: Property
prop_parse_typus_build_tags =
  let code = "//go:build linux\n// +build amd64\n\nlet x = 42"
  in case parseTypus code of
    Left _ -> property False
    Right file -> 
      let buildTags = tfBuildTags file
      in length buildTags === 2 .&&.
         "//go:build linux" `elem` map locatedValue buildTags .&&.
         "// +build amd64" `elem` map locatedValue buildTags

prop_parse_typus_multiple_packages :: Property
prop_parse_typus_multiple_packages =
  let code = "package main\npackage other"
  in case parseTypus code of
    Left err -> "Multiple package declarations found" `isInfixOf` err
    Right _ -> property False

prop_parse_typus_missing_brace_after_if :: Property
prop_parse_typus_missing_brace_after_if =
  let code = "if condition\n    let x = 42"
  in case parseTypus code of
    Left err -> "missing opening brace after if statement" `isInfixOf` err
    Right _ -> property False

prop_parse_typus_incomplete_expression :: Property
prop_parse_typus_incomplete_expression =
  let code = "let x ="
  in case parseTypus code of
    Left err -> "Incomplete expression" `isInfixOf` err
    Right _ -> property False

prop_parse_typus_function_declaration_not_incomplete :: Property
prop_parse_typus_function_declaration_not_incomplete =
  let code = "func test() { }"
  in case parseTypus code of
    Left _ -> property False
    Right file -> 
      let blocks = tfBlocks file
      in not (null blocks)

-- | 测试FileDirectives的属性
prop_file_directives_default :: Property
prop_file_directives_default =
  let fd = defaultFileDirectives
  in fdOwnership fd === Nothing .&&.
     fdDependentTypes fd === Nothing .&&.
     fdConstraints fd === Nothing

-- | 测试BlockDirectives的属性
prop_block_directives_default :: Property
prop_block_directives_default =
  let bd = defaultBlockDirectives
  in bdOwnership bd === Nothing .&&.
     bdDependentTypes bd === Nothing .&&.
     bdConstraints bd === Nothing

-- | 测试TypusFile的属性
prop_typus_file_default_structure :: SimpleTypusCode -> Property
prop_typus_file_default_structure (SimpleTypusCode code) =
  case parseTypus code of
    Left _ -> property False
    Right file -> 
      let directives = tfDirectives file
          buildTags = tfBuildTags file
          blocks = tfBlocks file
          syntaxErrors = tfSyntaxErrors file
      in directives === defaultFileDirectives .&&.
         null syntaxErrors

-- | 测试CodeBlock的属性
prop_code_block_content_non_empty :: SimpleTypusCode -> Property
prop_code_block_content_non_empty (SimpleTypusCode code) =
  case parseTypus code of
    Left _ -> property False
    Right file -> 
      let blocks = tfBlocks file
      in if null blocks
         then property True
         else all (not . null . cbContent) blocks

prop_code_block_directives_default :: SimpleTypusCode -> Property
prop_code_block_directives_default (SimpleTypusCode code) =
  case parseTypus code of
    Left _ -> property False
    Right file -> 
      let blocks = tfBlocks file
      in if null blocks
         then property True
         else all ((== defaultBlockDirectives) . cbDirectives) blocks

-- | 测试解析器对特殊字符的处理
prop_parse_typus_special_characters :: Property
prop_parse_typus_special_characters =
  let code = "let 中文 = \"hello\" // 注释\nlet x = 42"
  in case parseTypus code of
    Left _ -> property False
    Right file -> 
      let blocks = tfBlocks file
      in not (null blocks) ==> 
         let content = cbContent $ head blocks
         in "let 中文 = \"hello\"" `isInfixOf` content

-- | 测试解析器对空行的处理
prop_parse_typus_empty_lines :: Property
prop_parse_typus_empty_lines =
  let code = "\n\nlet x = 42\n\n\nlet y = 24\n\n"
  in case parseTypus code of
    Left _ -> property False
    Right file -> 
      let blocks = tfBlocks file
      in length blocks >= 1

-- | 测试解析器对嵌套注释的处理
prop_parse_typus_nested_comments :: Property
prop_parse_typus_nested_comments =
  let code = "/* outer /* inner */ still outer */ let x = 42"
  in case parseTypus code of
    Left _ -> property False
    Right file -> 
      let blocks = tfBlocks file
      in not (null blocks) ==> 
         let content = cbContent $ head blocks
         in "let x = 42" `isInfixOf` content

-- | 测试解析器对字符串中注释符号的处理
prop_parse_typus_comments_in_strings :: Property
prop_parse_typus_comments_in_strings =
  let code = "let s = \"/* not a comment */\"\nlet x = 42 // real comment"
  in case parseTypus code of
    Left _ -> property False
    Right file -> 
      let blocks = tfBlocks file
      in not (null blocks) ==> 
         let content = cbContent $ head blocks
         in "\"/* not a comment */\"" `isInfixOf` content .&&.
            not ("// real comment" `isInfixOf` content)

tests :: TestTree
tests = testGroup "Parser Advanced QuickCheck Tests"
  -- Basic parsing tests
  [ testProperty "parseTypus empty string" prop_parse_typus_empty_string
  , testProperty "parseTypus simple code" prop_parse_typus_simple_code
  , testProperty "parseTypus preserves non-comment lines" prop_parse_typus_preserves_non_comment_lines
  
  -- Directive parsing tests
  , testProperty "parseTypus directives" prop_parse_typus_directives
  
  -- Error handling tests
  , testProperty "parseTypus error handling" prop_parse_typus_error_handling
  , testProperty "parseTypus multiple packages" prop_parse_typus_multiple_packages
  , testProperty "parseTypus missing brace after if" prop_parse_typus_missing_brace_after_if
  , testProperty "parseTypus incomplete expression" prop_parse_typus_incomplete_expression
  , testProperty "parseTypus function declaration not incomplete" prop_parse_typus_function_declaration_not_incomplete
  
  -- Comment handling tests
  , testProperty "parseTypus multiline comments" prop_parse_typus_multiline_comments
  , testProperty "parseTypus line comments" prop_parse_typus_line_comments
  , testProperty "parseTypus nested comments" prop_parse_typus_nested_comments
  , testProperty "parseTypus comments in strings" prop_parse_typus_comments_in_strings
  
  -- String literal tests
  , testProperty "parseTypus string literals" prop_parse_typus_string_literals
  
  -- Build tag tests
  , testProperty "parseTypus build tags" prop_parse_typus_build_tags
  
  -- Special character tests
  , testProperty "parseTypus special characters" prop_parse_typus_special_characters
  
  -- Empty line tests
  , testProperty "parseTypus empty lines" prop_parse_typus_empty_lines
  
  -- Data structure tests
  , testProperty "FileDirectives default" prop_file_directives_default
  , testProperty "BlockDirectives default" prop_block_directives_default
  , testProperty "TypusFile default structure" prop_typus_file_default_structure
  , testProperty "CodeBlock content non-empty" prop_code_block_content_non_empty
  , testProperty "CodeBlock directives default" prop_code_block_directives_default
  ]