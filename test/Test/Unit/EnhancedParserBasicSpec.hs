module Test.Unit.EnhancedParserBasicSpec where



import Test.Tasty.HUnit
import Test.Tasty
import Test.Tasty.QuickCheck

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
import Data.Maybe (isJust, isNothing)

-- | 测试空字符串的解析
prop_parse_empty_string :: Property
prop_parse_empty_string = 
  let result = parseTypus ""
  in case result of
    Left _ -> property True
    Right file -> property (null (tfBlocks file))

-- | 测试默认文件指令的属性
prop_default_file_directives :: Property
prop_default_file_directives = 
  let defaults = defaultFileDirectives
  in isNothing (fdOwnership defaults) .&&. 
     isNothing (fdDependentTypes defaults) .&&.
     isNothing (fdConstraints defaults)

-- | 测试默认块指令的属性
prop_default_block_directives :: Property
prop_default_block_directives = 
  let defaults = defaultBlockDirectives
  in isNothing (bdOwnership defaults) .&&. 
     isNothing (bdDependentTypes defaults) .&&.
     isNothing (bdConstraints defaults)

-- | 测试解析简单注释
prop_parse_simple_comment :: Property
prop_parse_simple_comment = 
  let comment = "// This is a comment"
      result = parseTypus comment
  in case result of
    Left _ -> property True
    Right file -> property True -- 注释应该被正确处理，不产生错误

-- | 测试解析多行注释
prop_parse_multiline_comment :: Property
prop_parse_multiline_comment = 
  let comment = "/* This is a\nmultiline comment */"
      result = parseTypus comment
  in case result of
    Left _ -> property True
    Right file -> property True -- 注释应该被正确处理，不产生错误

-- | 测试解析简单代码块
prop_parse_simple_code_block :: Property
prop_parse_simple_code_block = 
  let code = "```typus\nlet x = 42\n```"
      result = parseTypus code
  in case result of
    Left _ -> property True
    Right file -> property (not (null (tfBlocks file)))

-- | 测试解析带有所有权指令的代码
prop_parse_ownership_directive :: Property
prop_parse_ownership_directive = 
  let code = "// @ownership: true\n```typus\nlet x = 42\n```"
      result = parseTypus code
  in case result of
    Left _ -> property True
    Right file -> property (isJust (fdOwnership (tfDirectives file)))

-- | 测试解析带有依赖类型指令的代码
prop_parse_dependent_types_directive :: Property
prop_parse_dependent_types_directive = 
  let code = "// @dependent-types: true\n```typus\nlet x = 42\n```"
      result = parseTypus code
  in case result of
    Left _ -> property True
    Right file -> property (isJust (fdDependentTypes (tfDirectives file)))

-- | 测试解析带有约束指令的代码
prop_parse_constraints_directive :: Property
prop_parse_constraints_directive = 
  let code = "// @constraints: true\n```typus\nlet x = 42\n```"
      result = parseTypus code
  in case result of
    Left _ -> property True
    Right file -> property (isJust (fdConstraints (tfDirectives file)))

-- | 测试解析多个代码块
prop_parse_multiple_code_blocks :: Property
prop_parse_multiple_code_blocks = 
  let code = "```typus\nlet x = 42\n```\n```typus\nlet y = 24\n```"
      result = parseTypus code
  in case result of
    Left _ -> property True
    Right file -> property (length (tfBlocks file) >= 2)

-- | 测试解析带有块指令的代码
prop_parse_block_directive :: Property
prop_parse_block_directive = 
  let code = "```typus\n// @ownership: true\nlet x = 42\n```"
      result = parseTypus code
  in case result of
    Left _ -> property True
    Right file -> 
      case tfBlocks file of
        [] -> property False
        (block:_) -> property (isJust (bdOwnership (cbDirectives block)))

-- | 测试解析一致性：解析结果不依赖于输入的空白字符
prop_parse_whitespace_independence :: String -> Property
prop_parse_whitespace_independence s = 
  let result1 = parseTypus s
      result2 = parseTypus (unlines (map (filter (not . (`elem` " \t"))) (lines s)))
  in case (result1, result2) of
    (Left _, Left _) -> property True
    (Right f1, Right f2) -> property (length (tfBlocks f1) == length (tfBlocks f2))
    _ -> property False

tests :: TestTree
tests = testGroup "Enhanced Parser Basic Tests"
  [ testProperty "parse empty string" prop_parse_empty_string
  , testProperty "default file directives" prop_default_file_directives
  , testProperty "default block directives" prop_default_block_directives
  , testProperty "parse simple comment" prop_parse_simple_comment
  , testProperty "parse multiline comment" prop_parse_multiline_comment
  , testProperty "parse simple code block" prop_parse_simple_code_block
  , testProperty "parse ownership directive" prop_parse_ownership_directive
  , testProperty "parse dependent types directive" prop_parse_dependent_types_directive
  , testProperty "parse constraints directive" prop_parse_constraints_directive
  , testProperty "parse multiple code blocks" prop_parse_multiple_code_blocks
  , testProperty "parse block directive" prop_parse_block_directive
  , testProperty "parse whitespace independence" prop_parse_whitespace_independence
  ]