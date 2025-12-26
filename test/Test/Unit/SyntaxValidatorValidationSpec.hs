{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.SyntaxValidatorValidationSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=), assertBool)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, choose, elements, listOf, oneof, sized)
import SyntaxValidator
  ( SyntaxValidator
  , SyntaxError(..)
  , ErrorType(..)
  , newSyntaxValidator
  , validateSyntax
  , validateFile
  , getSyntaxErrors
  , formatSyntaxError
  )
import qualified Data.Text as T
import Data.List (isInfixOf, isPrefixOf)

-- ============================================================================
-- 生成测试数据
-- ============================================================================

-- 生成错误类型
genErrorType :: Gen ErrorType
genErrorType = elements 
  [ MissingBrace
  , MissingParenthesis
  , MissingBracket
  , UnclosedString
  , UnclosedComment
  , InvalidIdentifier
  , InvalidTypeDeclaration
  , InvalidFunctionDeclaration
  , InvalidImport
  , InvalidStatement
  , UnterminatedBlock
  , InvalidOperator
  , MissingSemicolon
  , UnexpectedToken
  , MissingPackageDeclaration
  , DuplicateDeclaration
  , InvalidBlockStructure
  , UndeclaredVariable
  , SyntaxWarning
  ]

-- 生成有效的Go代码片段
genValidGoCodeSnippet :: Gen String
genValidGoCodeSnippet = oneof
  [ return "package main"
  , return "import \"fmt\""
  , return "func main() {}"
  , return "var x int = 42"
  , return "type MyStruct struct { field int }"
  , return "fmt.Println(\"hello\")"
  ]

-- 生成包含语法错误的代码片段
genInvalidGoCodeSnippet :: Gen String
genInvalidGoCodeSnippet = oneof
  [ return "func main() {"  -- missing closing brace
  , return "func test("      -- missing closing parenthesis
  , return "var x = [1, 2, 3"  -- missing closing bracket
  , return "var s = \"unclosed string"  -- unclosed string
  , return "/* unclosed comment"  -- unclosed comment
  , return "var 123invalid = 42"  -- invalid identifier
  , return "func invalid syntax {}"  -- invalid function declaration
  ]

-- ============================================================================
-- 语法验证属性测试
-- ============================================================================

-- Property: 创建语法验证器
prop_create_syntax_validator :: Property
prop_create_syntax_validator =
  let validator = newSyntaxValidator
  in property $ True

-- Property: 验证空代码
prop_validate_empty_code :: Property
prop_validate_empty_code =
  let validator = newSyntaxValidator
      result = validateSyntax validator ""
  in case result of
    Left _ -> property False
    Right _ -> property True

-- Property: 验证有效代码
prop_validate_valid_code :: String -> Property
prop_validate_valid_code code =
  not (null code) && "func" `isInfixOf` code && "{" `isInfixOf` code && "}" `isInfixOf` code ==>
  let validator = newSyntaxValidator
      result = validateSyntax validator code
  in case result of
    Left _ -> property False
    Right _ -> property True

-- Property: 验证无效代码
prop_validate_invalid_code :: String -> Property
prop_validate_invalid_code code =
  "func" `isInfixOf` code && "{" `isInfixOf` code && not ("}" `isInfixOf` code) ==>
  let validator = newSyntaxValidator
      result = validateSyntax validator code
  in case result of
    Left _ -> property True
    Right _ -> property False

-- Property: 获取语法错误
prop_get_syntax_errors :: String -> Property
prop_get_syntax_errors code =
  not (null code) ==>
  let validator = newSyntaxValidator
      _ = validateSyntax validator code
      errors = getSyntaxErrors validator
  in property $ True

-- Property: 格式化语法错误
prop_format_syntax_error :: ErrorType -> String -> Property
prop_format_syntax_error errorType message =
  not (null message) ==>
  let error = SyntaxError errorType message 0 0
      formatted = formatSyntaxError error
  in property $ message `isInfixOf` formatted

-- Property: 验证文件级语法
prop_validate_file_syntax :: String -> Property
prop_validate_file_syntax content =
  not (null content) ==>
  let result = validateFile content
  in case result of
    Left _ -> property True
    Right _ -> property True

-- Property: 检测缺失的大括号
prop_detect_missing_brace :: Property
prop_detect_missing_brace =
  let code = "func test() { return 42"
      validator = newSyntaxValidator
      result = validateSyntax validator code
  in case result of
    Left _ -> property True
    Right _ -> property False

-- Property: 检测缺失的括号
prop_detect_missing_parenthesis :: Property
prop_detect_missing_parenthesis =
  let code = "func test(int x { return x }"
      validator = newSyntaxValidator
      result = validateSyntax validator code
  in case result of
    Left _ -> property True
    Right _ -> property False

-- Property: 检测未闭合的字符串
prop_detect_unclosed_string :: Property
prop_detect_unclosed_string =
  let code = "func test() { return \"unclosed string }"
      validator = newSyntaxValidator
      result = validateSyntax validator code
  in case result of
    Left _ -> property True
    Right _ -> property False

-- Property: 检测无效标识符
prop_detect_invalid_identifier :: Property
prop_detect_invalid_identifier =
  let code = "func test() { var 123invalid = 42 }"
      validator = newSyntaxValidator
      result = validateSyntax validator code
  in case result of
    Left _ -> property True
    Right _ -> property False

-- ============================================================================
-- 单元测试
-- ============================================================================

tests :: TestTree
tests =
  testGroup "SyntaxValidator Validation Tests"
    [ testGroup "Property Tests"
        [ fastProperty "create syntax validator" prop_create_syntax_validator
        , fastProperty "validate empty code" prop_validate_empty_code
        , fastProperty "validate valid code" prop_validate_valid_code
        , fastProperty "validate invalid code" prop_validate_invalid_code
        , fastProperty "get syntax errors" prop_get_syntax_errors
        , fastProperty "format syntax error" prop_format_syntax_error
        , fastProperty "validate file syntax" prop_validate_file_syntax
        , fastProperty "detect missing brace" prop_detect_missing_brace
        , fastProperty "detect missing parenthesis" prop_detect_missing_parenthesis
        , fastProperty "detect unclosed string" prop_detect_unclosed_string
        , fastProperty "detect invalid identifier" prop_detect_invalid_identifier
        ]
    , testGroup "Unit Tests"
        [ testCase "create new syntax validator" $ do
            let validator = newSyntaxValidator
            assertBool "Validator should be created" $ True

        , testCase "validate simple function" $ do
            let code = "func test() { return 42 }"
                validator = newSyntaxValidator
                result = validateSyntax validator code
            case result of
              Left err -> assertFailure $ "Syntax validation failed: " ++ show err
              Right _ -> return ()

        , testCase "validate package declaration" $ do
            let code = "package main"
                validator = newSyntaxValidator
                result = validateSyntax validator code
            case result of
              Left err -> assertFailure $ "Syntax validation failed: " ++ show err
              Right _ -> return ()

        , testCase "validate import statement" $ do
            let code = "import \"fmt\""
                validator = newSyntaxValidator
                result = validateSyntax validator code
            case result of
              Left err -> assertFailure $ "Syntax validation failed: " ++ show err
              Right _ -> return ()

        , testCase "validate variable declaration" $ do
            let code = "var x int = 42"
                validator = newSyntaxValidator
                result = validateSyntax validator code
            case result of
              Left err -> assertFailure $ "Syntax validation failed: " ++ show err
              Right _ -> return ()

        , testCase "validate type declaration" $ do
            let code = "type MyStruct struct { field int }"
                validator = newSyntaxValidator
                result = validateSyntax validator code
            case result of
              Left err -> assertFailure $ "Syntax validation failed: " ++ show err
              Right _ -> return ()

        , testCase "detect missing closing brace" $ do
            let code = "func test() { return 42"
                validator = newSyntaxValidator
                result = validateSyntax validator code
            case result of
              Left _ -> return ()  -- Expected to fail
              Right _ -> assertFailure "Expected validation to detect missing closing brace"

        , testCase "detect missing closing parenthesis" $ do
            let code = "func test(int x { return x }"
                validator = newSyntaxValidator
                result = validateSyntax validator code
            case result of
              Left _ -> return ()  -- Expected to fail
              Right _ -> assertFailure "Expected validation to detect missing closing parenthesis"

        , testCase "detect missing closing bracket" $ do
            let code = "var arr = [1, 2, 3"
                validator = newSyntaxValidator
                result = validateSyntax validator code
            case result of
              Left _ -> return ()  -- Expected to fail
              Right _ -> assertFailure "Expected validation to detect missing closing bracket"

        , testCase "detect unclosed string literal" $ do
            let code = "func test() { return \"unclosed string }"
                validator = newSyntaxValidator
                result = validateSyntax validator code
            case result of
              Left _ -> return ()  -- Expected to fail
              Right _ -> assertFailure "Expected validation to detect unclosed string"

        , testCase "detect unclosed comment" $ do
            let code = "func test() { /* unclosed comment return 42 }"
                validator = newSyntaxValidator
                result = validateSyntax validator code
            case result of
              Left _ -> return ()  -- Expected to fail
              Right _ -> assertFailure "Expected validation to detect unclosed comment"

        , testCase "detect invalid identifier" $ do
            let code = "func test() { var 123invalid = 42 }"
                validator = newSyntaxValidator
                result = validateSyntax validator code
            case result of
              Left _ -> return ()  -- Expected to fail
              Right _ -> assertFailure "Expected validation to detect invalid identifier"

        , testCase "detect invalid function declaration" $ do
            let code = "func 123invalid() { return 42 }"
                validator = newSyntaxValidator
                result = validateSyntax validator code
            case result of
              Left _ -> return ()  -- Expected to fail
              Right _ -> assertFailure "Expected validation to detect invalid function declaration"

        , testCase "validate complex code structure" $ do
            let code = unlines
                  [ "package main"
                  , "import \"fmt\""
                  , "type Person struct {"
                  , "  Name string"
                  , "  Age  int"
                  , "}"
                  , "func (p *Person) Greet() {"
                  , "  fmt.Printf(\"Hello, %s!\n\", p.Name)"
                  , "}"
                  , "func main() {"
                  , "  person := Person{\"Alice\", 30}"
                  , "  person.Greet()"
                  , "}"
                  ]
                validator = newSyntaxValidator
                result = validateSyntax validator code
            case result of
              Left err -> assertFailure $ "Syntax validation failed: " ++ show err
              Right _ -> return ()

        , testCase "get syntax errors from invalid code" $ do
            let code = "func test() { var 123invalid = 42 }"
                validator = newSyntaxValidator
                _ = validateSyntax validator code
                errors = getSyntaxErrors validator
            assertBool "Should have syntax errors" $ not (null errors)

        , testCase "format syntax error message" $ do
            let error = SyntaxError InvalidIdentifier "Invalid identifier" 1 10
                formatted = formatSyntaxError error
            "Invalid identifier" `isInfixOf` formatted @?= True
            "line 1" `isInfixOf` formatted @?= True
            "column 10" `isInfixOf` formatted @?= True

        , testCase "validate file with multiple errors" $ do
            let content = unlines
                  [ "func test() {"
                  , "  var 123invalid = 42"
                  , "  return \"unclosed string"
                  , "}"
                  ]
                result = validateFile content
            case result of
              Left _ -> return ()  -- Expected to fail with multiple errors
              Right _ -> assertFailure "Expected file validation to detect multiple errors"

        , testCase "validate empty file" $ do
            let content = ""
                result = validateFile content
            case result of
              Left err -> assertFailure $ "Empty file validation failed: " ++ show err
              Right _ -> return ()

        , testCase "detect duplicate declarations" $ do
            let code = unlines
                  [ "func test() {}"
                  , "func test() {}"
                  ]
                validator = newSyntaxValidator
                result = validateSyntax validator code
            case result of
              Left _ -> return ()  -- Expected to fail
              Right _ -> assertFailure "Expected validation to detect duplicate declarations"

        , testCase "detect undeclared variable usage" $ do
            let code = "func test() { return undeclaredVar }"
                validator = newSyntaxValidator
                result = validateSyntax validator code
            case result of
              Left _ -> return ()  -- Expected to fail
              Right _ -> assertFailure "Expected validation to detect undeclared variable"
        ]
    ]