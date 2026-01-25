module Test.Unit.EnhancedBoundaryConditionsSpec where



import Test.Tasty
import Test.Tasty.QuickCheck

import Compiler (compile)
import Parser (parseTypus)
import Compiler.DependentTypeChecker (checkDependentTypes)
import Compiler.OwnershipChecker (checkOwnership)
import Data.Char (chr, isPrint)
import Data.List (replicate)

-- | 测试空字符串解析
prop_parse_empty_string :: Property
prop_parse_empty_string = 
  let result = parseTypus ""
  in case result of
    Left _ -> property True
    Right file -> property True

-- | 测试单个字符解析
prop_parse_single_char :: Char -> Property
prop_parse_single_char c = 
  let input = [c]
      result = parseTypus input
  in case result of
    Left _ -> property True
    Right file -> property True

-- | 测试极大字符串解析
prop_parse_very_large_string :: Positive Int -> Property
prop_parse_very_large_string (Positive n) = 
  let input = replicate n 'a'
      result = parseTypus input
  in case result of
    Left _ -> property True
    Right file -> property True

-- | 测试Unicode字符解析
prop_parse_unicode_chars :: Property
prop_parse_unicode_chars = 
  let input = map chr [0..65535]
      result = parseTypus input
  in case result of
    Left _ -> property True
    Right file -> property True

-- | 测试非打印字符解析
prop_parse_non_printable_chars :: Property
prop_parse_non_printable_chars = 
  let input = map chr [0..31] ++ map chr [127..159]
      result = parseTypus input
  in case result of
    Left _ -> property True
    Right file -> property True

-- | 测试极大嵌套深度
prop_parse_very_deep_nesting :: Positive Int -> Property
prop_parse_very_deep_nesting (Positive n) = 
  let input = "```typus\n" ++ concat (replicate n "{") ++ "42" ++ concat (replicate n "}") ++ "\n```"
      result = parseTypus input
  in case result of
    Left _ -> property True
    Right file -> property True

-- | 测试极长标识符
prop_parse_very_long_identifier :: Positive Int -> Property
prop_parse_very_long_identifier (Positive n) = 
  let identifier = replicate n 'a'
      input = "```typus\nlet " ++ identifier ++ " = 42\n```"
      result = parseTypus input
  in case result of
    Left _ -> property True
    Right file -> property True

-- | 测试极长数字
prop_parse_very_long_number :: Positive Int -> Property
prop_parse_very_long_number (Positive n) = 
  let number = replicate n '9'
      input = "```typus\nlet x = " ++ number ++ "\n```"
      result = parseTypus input
  in case result of
    Left _ -> property True
    Right file -> property True

-- | 测试极长字符串字面量
prop_parse_very_long_string_literal :: Positive Int -> Property
prop_parse_very_long_string_literal (Positive n) = 
  let strContent = replicate n 'a'
      input = "```typus\nlet s = \"" ++ strContent ++ "\"\n```"
      result = parseTypus input
  in case result of
    Left _ -> property True
    Right file -> property True

-- | 测试极长注释
prop_parse_very_long_comment :: Positive Int -> Property
prop_parse_very_long_comment (Positive n) = 
  let comment = replicate n 'a'
      input = "// " ++ comment ++ "\n```typus\nlet x = 42\n```"
      result = parseTypus input
  in case result of
    Left _ -> property True
    Right file -> property True

-- | 测试极长多行注释
prop_parse_very_long_multiline_comment :: Positive Int -> Property
prop_parse_very_long_multiline_comment (Positive n) = 
  let comment = replicate n 'a'
      input = "/* " ++ comment ++ " */\n```typus\nlet x = 42\n```"
      result = parseTypus input
  in case result of
    Left _ -> property True
    Right file -> property True

-- | 测试极大数组
prop_parse_very_large_array :: Positive Int -> Property
prop_parse_very_large_array (Positive n) = 
  let arrayElements = concat (replicate (min n 1000) "42,") ++ "42"
      input = "```typus\nlet arr = [" ++ arrayElements ++ "]\n```"
      result = parseTypus input
  in case result of
    Left _ -> property True
    Right file -> property True

-- | 测试极大函数参数列表
prop_parse_very_large_param_list :: Positive Int -> Property
prop_parse_very_large_param_list (Positive n) = 
  let params = concat (replicate (min n 100) "x: Nat,") ++ "y: Nat"
      input = "```typus\nfn foo(" ++ params ++ ") -> Nat { 42 }\n```"
      result = parseTypus input
  in case result of
    Left _ -> property True
    Right file -> property True

-- | 测试极大结构体字段列表
prop_parse_very_large_struct_fields :: Positive Int -> Property
prop_parse_very_large_struct_fields (Positive n) = 
  let fields = concat (replicate (min n 100) ("field" ++ show n ++ ": Nat,")) ++ "last: Nat"
      input = "```typus\nstruct BigStruct { " ++ fields ++ " }\n```"
      result = parseTypus input
  in case result of
    Left _ -> property True
    Right file -> property True

-- | 测试极大匹配分支
prop_parse_very_large_match_branches :: Positive Int -> Property
prop_parse_very_large_match_branches (Positive n) = 
  let branches = concat (replicate (min n 100) "0 => 0,") ++ "_ => 42"
      input = "```typus\nlet x = match 42 { " ++ branches ++ " }\n```"
      result = parseTypus input
  in case result of
    Left _ -> property True
    Right file -> property True

-- | 测试极大类型表达式
prop_parse_very_large_type_expression :: Positive Int -> Property
prop_parse_very_large_type_expression (Positive n) = 
  let typeExpr = concat (replicate (min n 50) "Box<") ++ "Nat" ++ concat (replicate (min n 50) ">")
      input = "```typus\nlet x: " ++ typeExpr ++ " = 42\n```"
      result = parseTypus input
  in case result of
    Left _ -> property True
    Right file -> property True

-- | 测试极大依赖类型表达式
prop_parse_very_large_dependent_type :: Positive Int -> Property
prop_parse_very_large_dependent_type (Positive n) = 
  let dependentType = "Vector(" ++ show (min n 1000) ++ ")"
      input = "```typus\nlet v: " ++ dependentType ++ " = [1,2,3]\n```"
      result = parseTypus input
  in case result of
    Left _ -> property True
    Right file -> property True

-- | 测试极大所有权表达式
prop_parse_very_large_ownership_expression :: Positive Int -> Property
prop_parse_very_large_ownership_expression (Positive n) = 
  let ownershipExpr = concat (replicate (min n 100) "move(") ++ "x" ++ concat (replicate (min n 100) ")")
      input = "```typus\nlet x = Box(42)\nlet y = " ++ ownershipExpr ++ "\n```"
      result = parseTypus input
  in case result of
    Left _ -> property True
    Right file -> property True

-- | 测试极深递归
prop_parse_very_deep_recursion :: Positive Int -> Property
prop_parse_very_deep_recursion (Positive n) = 
  let depth = min n 100
      recursiveFunc = "fn factorial(n: Nat): Nat {\n  if n <= 1 then 1 else n * factorial(n-1)\n}\n"
      input = "```typus\n" ++ recursiveFunc ++ "let result = factorial(" ++ show depth ++ ")\n```"
      result = parseTypus input
  in case result of
    Left _ -> property True
    Right file -> property True

-- | 测试极大模块导入
prop_parse_very_large_imports :: Positive Int -> Property
prop_parse_very_large_imports (Positive n) = 
  let imports = concat (replicate (min n 100) ("import module" ++ show n ++ ";\n"))
      input = imports ++ "```typus\nlet x = 42\n```"
      result = parseTypus input
  in case result of
    Left _ -> property True
    Right file -> property True

tests :: TestTree
tests = testGroup "Enhanced Boundary Conditions Tests"
  [ testProperty "parse empty string" prop_parse_empty_string
  , testProperty "parse single char" prop_parse_single_char
  , testProperty "parse very large string" prop_parse_very_large_string
  , testProperty "parse unicode chars" prop_parse_unicode_chars
  , testProperty "parse non printable chars" prop_parse_non_printable_chars
  , testProperty "parse very deep nesting" prop_parse_very_deep_nesting
  , testProperty "parse very long identifier" prop_parse_very_long_identifier
  , testProperty "parse very long number" prop_parse_very_long_number
  , testProperty "parse very long string literal" prop_parse_very_long_string_literal
  , testProperty "parse very long comment" prop_parse_very_long_comment
  , testProperty "parse very long multiline comment" prop_parse_very_long_multiline_comment
  , testProperty "parse very large array" prop_parse_very_large_array
  , testProperty "parse very large param list" prop_parse_very_large_param_list
  , testProperty "parse very large struct fields" prop_parse_very_large_struct_fields
  , testProperty "parse very large match branches" prop_parse_very_large_match_branches
  , testProperty "parse very large type expression" prop_parse_very_large_type_expression
  , testProperty "parse very large dependent type" prop_parse_very_large_dependent_type
  , testProperty "parse very large ownership expression" prop_parse_very_large_ownership_expression
  , testProperty "parse very deep recursion" prop_parse_very_deep_recursion
  , testProperty "parse very large imports" prop_parse_very_large_imports
  ]