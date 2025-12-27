{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.CompilerGoParsingQuickCheckTestSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import qualified Data.Map as Map
import Data.List (sort, nub)

import Compiler.GoParsing
import Compiler.GoLexer
import Compiler.GoAst
import SourceLocation (SourcePos(..), SourceSpan(..), startPos)
import TestSupport.Arbitrary ()

tests :: TestTree
tests = testGroup "Compiler GoParsing QuickCheck Tests"
  [ parsingExpressionTests
  , parsingStatementTests
  , parsingDeclarationTests
  , parsingFunctionTests
  , parsingTypeTests
  , parsingPackageTests
  , parsingImportTests
  , parsingControlFlowTests
  , parsingErrorTests
  , parsingValidationTests
  ]

-- | 1. 表达式解析测试
parsingExpressionTests :: TestTree
parsingExpressionTests = testGroup "Parsing Expression Tests"
  [ testCase "Parse simple identifier" =
      let result = parseGoExpression "x"
      in case result of
           Right expr -> case expr of
                          GoIdentifierExpr name -> name @?= "x"
                          _ -> "Expected identifier" @?= "Got something else"
           Left _ -> "Expected successful parse" @?= "Got error"
  
  , testCase "Parse integer literal" =
      let result = parseGoExpression "42"
      in case result of
           Right expr -> case expr of
                          GoIntegerLiteralExpr n -> n @?= 42
                          _ -> "Expected integer literal" @?= "Got something else"
           Left _ -> "Expected successful parse" @?= "Got error"
  
  , testCase "Parse binary expression" =
      let result = parseGoExpression "1 + 2"
      in case result of
           Right expr -> case expr of
                          GoBinaryExpr GoPlus (GoIntegerLiteralExpr 1) (GoIntegerLiteralExpr 2) -> True @?= True
                          _ -> "Expected binary expression" @?= "Got something else"
           Left _ -> "Expected successful parse" @?= "Got error"
  
  , fastProperty "Integer literal parsing" =
      \n -> let result = parseGoExpression (show n)
            in case result of
                 Right (GoIntegerLiteralExpr val) -> val == n
                 _ -> False
  ]

-- | 2. 语句解析测试
parsingStatementTests :: TestTree
parsingStatementTests = testGroup "Parsing Statement Tests"
  [ testCase "Parse variable declaration" =
      let result = parseGoStatement "var x int = 42"
      in case result of
           Right stmt -> case stmt of
                          GoVarDeclStmt "x" (Just GoIntType) (Just (GoIntegerLiteralExpr 42)) -> True @?= True
                          _ -> "Expected variable declaration" @?= "Got something else"
           Left _ -> "Expected successful parse" @?= "Got error"
  
  , testCase "Parse assignment statement" =
      let result = parseGoStatement "x = 42"
      in case result of
           Right stmt -> case stmt of
                          GoAssignStmt [GoIdentifierExpr "x"] [GoIntegerLiteralExpr 42] -> True @?= True
                          _ -> "Expected assignment" @?= "Got something else"
           Left _ -> "Expected successful parse" @?= "Got error"
  
  , testCase "Parse return statement" =
      let result = parseGoStatement "return 42"
      in case result of
           Right stmt -> case stmt of
                          GoReturnStmt (Just (GoIntegerLiteralExpr 42)) -> True @?= True
                          _ -> "Expected return statement" @?= "Got something else"
           Left _ -> "Expected successful parse" @?= "Got error"
  
  , fastProperty "Simple assignment parsing" =
      \varName value -> let stmt = varName ++ " = " ++ show value
                            result = parseGoStatement stmt
                        in case result of
                             Right (GoAssignStmt [GoIdentifierExpr name] [GoIntegerLiteralExpr val]) -> name == varName && val == value
                             _ -> False
  ]

-- | 3. 声明解析测试
parsingDeclarationTests :: TestTree
parsingDeclarationTests = testGroup "Parsing Declaration Tests"
  [ testCase "Parse function declaration" =
      let result = parseGoDeclaration "func test() int { return 42 }"
      in case result of
           Right decl -> case decl of
                          GoFunctionDecl "test" [] GoIntType [GoReturnStmt (Just (GoIntegerLiteralExpr 42))] -> True @?= True
                          _ -> "Expected function declaration" @?= "Got something else"
           Left _ -> "Expected successful parse" @?= "Got error"
  
  , testCase "Parse variable declaration" =
      let result = parseGoDeclaration "var x int = 42"
      in case result of
           Right decl -> case decl of
                          GoVarDecl "x" (Just GoIntType) (Just (GoIntegerLiteralExpr 42)) -> True @?= True
                          _ -> "Expected variable declaration" @?= "Got something else"
           Left _ -> "Expected successful parse" @?= "Got error"
  
  , testCase "Parse type declaration" =
      let result = parseGoDeclaration "type MyInt int"
      in case result of
           Right decl -> case decl of
                          GoTypeDecl "MyInt" GoIntType -> True @?= True
                          _ -> "Expected type declaration" @?= "Got something else"
           Left _ -> "Expected successful parse" @?= "Got error"
  
  , fastProperty "Simple function declaration parsing" =
      \funcName -> let decl = "func " ++ funcName ++ "() int { return 0 }"
                       result = parseGoDeclaration decl
                   in case result of
                        Right (GoFunctionDecl name [] GoIntType _) -> name == funcName
                        _ -> False
  ]

-- | 4. 函数解析测试
parsingFunctionTests :: TestTree
parsingFunctionTests = testGroup "Parsing Function Tests"
  [ testCase "Parse function with parameters" =
      let result = parseGoFunction "func add(x int, y int) int { return x + y }"
      in case result of
           Right func -> case func of
                          GoFunctionDecl "add" [GoParam "x" GoIntType, GoParam "y" GoIntType] GoIntType _ -> True @?= True
                          _ -> "Expected function with parameters" @?= "Got something else"
           Left _ -> "Expected successful parse" @?= "Got error"
  
  , testCase "Parse function with multiple statements" =
      let result = parseGoFunction "func test() int { x := 42; return x }"
      in case result of
           Right func -> case func of
                          GoFunctionDecl "test" [] GoIntType [_] -> True @?= True
                          _ -> "Expected function with multiple statements" @?= "Got something else"
           Left _ -> "Expected successful parse" @?= "Got error"
  
  , testCase "Parse function without return type" =
      let result = parseGoFunction "func test() { println(\"hello\") }"
      in case result of
           Right func -> case func of
                          GoFunctionDecl "test" [] GoVoidType _ -> True @?= True
                          _ -> "Expected function without return type" @?= "Got something else"
           Left _ -> "Expected successful parse" @?= "Got error"
  
  , fastProperty "Function name consistency" =
      \funcName -> let func = "func " ++ funcName ++ "() {}"
                       result = parseGoFunction func
                   in case result of
                        Right (GoFunctionDecl name [] GoVoidType _) -> name == funcName
                        _ -> False
  ]

-- | 5. 类型解析测试
parsingTypeTests :: TestTree
parsingTypeTests = testGroup "Parsing Type Tests"
  [ testCase "Parse basic type int" =
      let result = parseGoType "int"
      in case result of
           Right GoIntType -> True @?= True
           Left _ -> "Expected successful parse" @?= "Got error"
  
  , testCase "Parse basic type string" =
      let result = parseGoType "string"
      in case result of
           Right GoStringType -> True @?= True
           Left _ -> "Expected successful parse" @?= "Got error"
  
  , testCase "Parse slice type" =
      let result = parseGoType "[]int"
      in case result of
           Right (GoSliceType GoIntType) -> True @?= True
           Left _ -> "Expected successful parse" @?= "Got error"
  
  , testCase "Parse function type" =
      let result = parseGoType "func(int) string"
      in case result of
           Right (GoFunctionType [GoIntType] GoStringType) -> True @?= True
           Left _ -> "Expected successful parse" @?= "Got error"
  
  , fastProperty "Basic type parsing" =
      \typeName -> let result = parseGoType typeName
                   in case result of
                        Right _ -> True
                        Left _ -> False
  ]

-- | 6. 包解析测试
parsingPackageTests :: TestTree
parsingPackageTests = testGroup "Parsing Package Tests"
  [ testCase "Parse package declaration" =
      let result = parseGoPackage "package main"
      in case result of
           Right pkg -> case pkg of
                          GoPackage "main" [] -> True @?= True
                          _ -> "Expected package main" @?= "Got something else"
           Left _ -> "Expected successful parse" @?= "Got error"
  
  , testCase "Parse package with function" =
      let result = parseGoPackage "package main\n\nfunc main() {}"
      in case result of
           Right pkg -> case pkg of
                          GoPackage "main" [_] -> True @?= True
                          _ -> "Expected package with function" @?= "Got something else"
           Left _ -> "Expected successful parse" @?= "Got error"
  
  , fastProperty "Package name consistency" =
      \pkgName -> let pkg = "package " ++ pkgName
                      result = parseGoPackage pkg
                  in case result of
                       Right (GoPackage name _) -> name == pkgName
                       _ -> False
  ]

-- | 7. 导入解析测试
parsingImportTests :: TestTree
parsingImportTests = testGroup "Parsing Import Tests"
  [ testCase "Parse simple import" =
      let result = parseGoImport "import \"fmt\""
      in case result of
           Right (GoImport "fmt" Nothing) -> True @?= True
           Left _ -> "Expected successful parse" @?= "Got error"
  
  , testCase "Parse import with alias" =
      let result = parseGoImport "import f \"fmt\""
      in case result of
           Right (GoImport "fmt" (Just "f")) -> True @?= True
           Left _ -> "Expected successful parse" @?= "Got error"
  
  , fastProperty "Import path consistency" =
      \importPath -> let imp = "import \"" ++ importPath ++ "\""
                         result = parseGoImport imp
                     in case result of
                          Right (GoImport path _) -> path == importPath
                          _ -> False
  ]

-- | 8. 控制流解析测试
parsingControlFlowTests :: TestTree
parsingControlFlowTests = testGroup "Parsing Control Flow Tests"
  [ testCase "Parse if statement" =
      let result = parseGoStatement "if x > 0 { return 1 }"
      in case result of
           Right stmt -> case stmt of
                          GoIfStmt _ [_] [] -> True @?= True
                          _ -> "Expected if statement" @?= "Got something else"
           Left _ -> "Expected successful parse" @?= "Got error"
  
  , testCase "Parse for loop" =
      let result = parseGoStatement "for i := 0; i < 10; i++ { }"
      in case result of
           Right stmt -> case stmt of
                          GoForStmt _ _ _ [] -> True @?= True
                          _ -> "Expected for loop" @?= "Got something else"
           Left _ -> "Expected successful parse" @?= "Got error"
  
  , testCase "Parse switch statement" =
      let result = parseGoStatement "switch x { case 1: return 1 }"
      in case result of
           Right stmt -> case stmt of
                          GoSwitchStmt _ [_] -> True @?= True
                          _ -> "Expected switch statement" @?= "Got something else"
           Left _ -> "Expected successful parse" @?= "Got error"
  
  , fastProperty "Simple if statement parsing" =
      \condition -> let stmt = "if " ++ condition ++ " { }"
                        result = parseGoStatement stmt
                    in case result of
                         Right (GoIfStmt _ [] []) -> True
                         _ -> False
  ]

-- | 9. 错误处理测试
parsingErrorTests :: TestTree
parsingErrorTests = testGroup "Parsing Error Tests"
  [ testCase "Parse invalid syntax" =
      let result = parseGoExpression "x + + y"
      in case result of
           Left _ -> True @?= True
           Right _ -> "Expected parse error" @?= "Got success"
  
  , testCase "Parse incomplete function" =
      let result = parseGoFunction "func test("
      in case result of
           Left _ -> True @?= True
           Right _ -> "Expected parse error" @?= "Got success"
  
  , testCase "Parse invalid type" =
      let result = parseGoType "invalidtype"
      in case result of
           Left _ -> True @?= True
           Right _ -> "Expected parse error" @?= "Got success"
  
  , fastProperty "Error detection" =
      \invalidInput -> let result = parseGoExpression invalidInput
                       in case result of
                            Left _ -> True
                            Right _ -> False
  ]

-- | 10. 解析验证测试
parsingValidationTests :: TestTree
parsingValidationTests = testGroup "Parsing Validation Tests"
  [ testCase "Validate parsed expression" =
      let result = parseGoExpression "x + 42"
      in case result of
           Right expr -> validateGoExpression expr @?= True
           Left _ -> "Expected successful parse" @?= "Got error"
  
  , testCase "Validate parsed statement" =
      let result = parseGoStatement "x := 42"
      in case result of
           Right stmt -> validateGoStatement stmt @?= True
           Left _ -> "Expected successful parse" @?= "Got error"
  
  , testCase "Validate parsed function" =
      let result = parseGoFunction "func test() int { return 42 }"
      in case result of
           Right func -> validateGoFunction func @?= True
           Left _ -> "Expected successful parse" @?= "Got error"
  
  , fastProperty "Expression validation" =
      \expr -> let result = parseGoExpression expr
               in case result of
                    Right e -> validateGoExpression e
                    Left _ -> False
  ]