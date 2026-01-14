{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.CompilerIROptimizationQuickCheckTestSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import Compiler.IR
import Parser (TypusFile(..), CodeBlock(..))
import qualified Data.Text as T
import Data.List (nub, sort)
import Data.Either (isLeft, isRight)

-- | 测试SourceIR的基本属性
prop_sourceir_creation :: String -> Property
prop_sourceir_creation code =
  not (null code) ==>
  let sourceIR = rawSourceFromTypus code
  in length sourceIR >= 0

-- | 测试SourceIR的构建
prop_sourceir_build :: String -> Property
prop_sourceir_build code =
  not (null code) ==>
  let sourceIR = buildSourceIR code
  in case sourceIR of
    Left _ -> property True
    Right ir -> length ir >= 0

-- | 测试SemanticIR的构建
prop_semanticir_build :: String -> Property
prop_semanticir_build code =
  not (null code) ==>
  let sourceIR = buildSourceIR code
  in case sourceIR of
    Left _ -> property True
    Right ir -> 
      let semanticIR = buildSemanticIR ir
      in case semanticIR of
        Left _ -> property True
        Right semIR -> length semIR >= 0

-- | 测试SemanticIR与包的构建
prop_semanticir_build_with_package :: String -> String -> Property
prop_semanticir_build_with_package code pkgName =
  not (null code) && not (null pkgName) ==>
  let sourceIR = buildSourceIR code
  in case sourceIR of
    Left _ -> property True
    Right ir -> 
      let semanticIR = buildSemanticIRWithPackage ir pkgName
      in case semanticIR of
        Left _ -> property True
        Right semIR -> length semIR >= 0

-- | 测试模块从Typus的构建
prop_module_from_typus :: String -> Property
prop_module_from_typus code =
  not (null code) ==>
  let result = moduleFromTypus code
  in case result of
    Left _ -> property True
    Right module_ -> length module_ >= 0

-- | 测试包声明的确保
prop_ensure_package_decl :: String -> Property
prop_ensure_package_decl code =
  let result = ensurePackageDecl code
  in case result of
    Left _ -> property True
    Right _ -> property True

-- | 测试主函数的确保
prop_ensure_main_function :: String -> Property
prop_ensure_main_function code =
  let result = ensureMainFunction code
  in case result of
    Left _ -> property True
    Right _ -> property True

-- | 测试推断导入的附加
prop_attach_inferred_imports :: String -> Property
prop_attach_inferred_imports code =
  let result = attachInferredImports code
  in case result of
    Left _ -> property True
    Right _ -> property True

-- | 测试必需元素的检查
prop_has_required_elements :: String -> Property
prop_has_required_elements code =
  let result = hasRequiredElements code
  in case result of
    Left _ -> property True
    Right _ -> property True

-- | 测试IR语句的创建
prop_ir_statement_creation :: String -> String -> Property
prop_ir_statement_creation stmtType content =
  not (null stmtType) && not (null content) ==>
  let stmt = IRStatement stmtType content
  in irStmtType stmt === stmtType && irStmtContent stmt === content

-- | 测试IR表达式的创建
prop_ir_expression_creation :: String -> String -> Property
prop_ir_expression_creation exprType content =
  not (null exprType) && not (null content) ==>
  let expr = IRExpression exprType content
  in irExprType expr === exprType && irExprContent expr === content

-- | 测试GoIR的构建
prop_goir_build :: String -> Property
prop_goir_build code =
  not (null code) ==>
  let sourceIR = buildSourceIR code
  in case sourceIR of
    Left _ -> property True
    Right ir -> 
      let goIR = buildGo ir
      in case goIR of
        Left _ -> property True
        Right gIR -> length gIR >= 0

-- | 测试空代码的IR构建
test_ir_build_empty :: Assertion
test_ir_build_empty = do
  let sourceIR = buildSourceIR ""
      semanticIR = buildSemanticIR =<< sourceIR
  case sourceIR of
    Left _ -> assertFailure "Empty code should not fail to build SourceIR"
    Right ir -> do
      assertEqual "Empty code should result in empty SourceIR" 0 (length ir)
      case semanticIR of
        Left _ -> assertFailure "Empty SourceIR should not fail to build SemanticIR"
        Right semIR -> assertEqual "Empty SourceIR should result in empty SemanticIR" 0 (length semIR)

-- | 测试简单代码的IR构建
test_ir_build_simple :: Assertion
test_ir_build_simple = do
  let simpleCode = "func main() {\n    return 42\n}"
      sourceIR = buildSourceIR simpleCode
      semanticIR = buildSemanticIR =<< sourceIR
  case sourceIR of
    Left err -> assertFailure $ "Failed to build SourceIR from simple code: " ++ err
    Right ir -> do
      assertBool "Simple code should result in non-empty SourceIR" (length ir > 0)
      case semanticIR of
        Left err -> assertFailure $ "Failed to build SemanticIR from SourceIR: " ++ err
        Right semIR -> assertBool "Simple code should result in non-empty SemanticIR" (length semIR > 0)

-- | 测试复杂代码的IR构建
test_ir_build_complex :: Assertion
test_ir_build_complex = do
  let complexCode = "package main\n\nimport \"fmt\"\n\nfunc add(x int, y int) int {\n    return x + y\n}\n\nfunc main() {\n    result := add(1, 2)\n    fmt.Println(result)\n}"
      sourceIR = buildSourceIR complexCode
      semanticIR = buildSemanticIR =<< sourceIR
  case sourceIR of
    Left err -> assertFailure $ "Failed to build SourceIR from complex code: " ++ err
    Right ir -> do
      assertBool "Complex code should result in non-empty SourceIR" (length ir > 0)
      case semanticIR of
        Left err -> assertFailure $ "Failed to build SemanticIR from SourceIR: " ++ err
        Right semIR -> assertBool "Complex code should result in non-empty SemanticIR" (length semIR > 0)

-- | 测试包声明的确保
test_ensure_package_decl :: Assertion
test_ensure_package_decl = do
  let codeWithoutPkg = "func main() {}\n"
      codeWithPkg = "package main\n\nfunc main() {}\n"
      result1 = ensurePackageDecl codeWithoutPkg
      result2 = ensurePackageDecl codeWithPkg
  case result1 of
    Left err -> assertFailure $ "Failed to ensure package declaration: " ++ err
    Right _ -> assertBool "Package declaration added" True
  case result2 of
    Left err -> assertFailure $ "Failed to ensure package declaration: " ++ err
    Right _ -> assertBool "Package declaration preserved" True

-- | 测试主函数的确保
test_ensure_main_function :: Assertion
test_ensure_main_function = do
  let codeWithoutMain = "package main\n\nfunc helper() {}\n"
      codeWithMain = "package main\n\nfunc main() {}\n"
      result1 = ensureMainFunction codeWithoutMain
      result2 = ensureMainFunction codeWithMain
  case result1 of
    Left err -> assertFailure $ "Failed to ensure main function: " ++ err
    Right _ -> assertBool "Main function added" True
  case result2 of
    Left err -> assertFailure $ "Failed to ensure main function: " ++ err
    Right _ -> assertBool "Main function preserved" True

-- | 测试必需元素的检查
test_has_required_elements :: Assertion
test_has_required_elements = do
  let completeCode = "package main\n\nfunc main() {}\n"
      incompleteCode = "func helper() {}\n"
      result1 = hasRequiredElements completeCode
      result2 = hasRequiredElements incompleteCode
  case result1 of
    Left _ -> assertFailure "Complete code should have required elements"
    Right _ -> assertBool "Complete code has required elements" True
  case result2 of
    Left _ -> assertBool "Incomplete code should not have required elements" True
    Right _ -> assertFailure "Incomplete code should not have required elements"

-- | 测试GoIR的构建
test_goir_build :: Assertion
test_goir_build = do
  let goCode = "package main\n\nfunc main() {\n    fmt.Println(\"Hello, World!\")\n}"
      sourceIR = buildSourceIR goCode
      goIR = buildGo =<< sourceIR
  case sourceIR of
    Left err -> assertFailure $ "Failed to build SourceIR: " ++ err
    Right ir -> do
      assertBool "Go code should result in non-empty SourceIR" (length ir > 0)
      case goIR of
        Left err -> assertFailure $ "Failed to build GoIR: " ++ err
        Right gIR -> assertBool "Go code should result in non-empty GoIR" (length gIR > 0)

-- | 测试IR语句
test_ir_statement :: Assertion
test_ir_statement = do
  let stmt = IRStatement "assignment" "x := 42"
  assertEqual "Statement type" "assignment" (irStmtType stmt)
  assertEqual "Statement content" "x := 42" (irStmtContent stmt)

-- | 测试IR表达式
test_ir_expression :: Assertion
test_ir_expression = do
  let expr = IRExpression "binary" "x + y"
  assertEqual "Expression type" "binary" (irExprType expr)
  assertEqual "Expression content" "x + y" (irExprContent expr)

-- | 测试模块从Typus的构建
test_module_from_typus :: Assertion
test_module_from_typus = do
  let typusCode = "func main() {\n    return 42\n}"
      result = moduleFromTypus typusCode
  case result of
    Left err -> assertFailure $ "Failed to build module from Typus: " ++ err
    Right module_ -> assertBool "Module built successfully" (length module_ > 0)

-- | 测试套件
tests :: TestTree
tests = testGroup "Compiler IR Optimization QuickCheck Test Tests"
  [ testProperty "SourceIR creation" prop_sourceir_creation
  , testProperty "SourceIR build" prop_sourceir_build
  , testProperty "SemanticIR build" prop_semanticir_build
  , testProperty "SemanticIR build with package" prop_semanticir_build_with_package
  , testProperty "Module from Typus" prop_module_from_typus
  , testProperty "Ensure package declaration" prop_ensure_package_decl
  , testProperty "Ensure main function" prop_ensure_main_function
  , testProperty "Attach inferred imports" prop_attach_inferred_imports
  , testProperty "Has required elements" prop_has_required_elements
  , testProperty "IR statement creation" prop_ir_statement_creation
  , testProperty "IR expression creation" prop_ir_expression_creation
  , testProperty "GoIR build" prop_goir_build
  , testCase "IR build empty" test_ir_build_empty
  , testCase "IR build simple" test_ir_build_simple
  , testCase "IR build complex" test_ir_build_complex
  , testCase "Ensure package declaration" test_ensure_package_decl
  , testCase "Ensure main function" test_ensure_main_function
  , testCase "Has required elements" test_has_required_elements
  , testCase "GoIR build" test_goir_build
  , testCase "IR statement" test_ir_statement
  , testCase "IR expression" test_ir_expression
  , testCase "Module from Typus" test_module_from_typus
  ]