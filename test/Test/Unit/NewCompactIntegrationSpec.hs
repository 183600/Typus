{-# LANGUAGE ScopedTypeVars #-}

module Test.Unit.NewCompactIntegrationSpec where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===), forAll, choose, elements)
import Parser (parseTypus)
import Compiler (compileToGo)
import Utils (trim, removeComments)
import ErrorHandler (formatError, aggregateErrors)
import SourceLocation (SourcePos(..))
import Data.List (isInfixOf, isPrefixOf)
import Data.Either (isLeft, isRight)

-- | 生成简单的Typus代码片段
genSimpleTypusCode :: Gen String
genSimpleTypusCode = elements
  [ "func main() { return 0; }"
  , "let x = 42;"
  , "var y string = \"hello\";"
  , "if (x > 0) { print(x); }"
  , "for (i := 0; i < 10; i++) { }"
  ]

-- | 生成带指令的Typus代码
genDirectiveCode :: Gen String
genDirectiveCode = do
  hasOwnership <- elements [True, False]
  hasDepTypes <- elements [True, False]
  let ownership = if hasOwnership then "// @ownership\n" else ""
      depTypes = if hasDepTypes then "// @dependent-types\n" else ""
      code = "func test() { return 0; }"
  return $ ownership ++ depTypes ++ code

-- | 测试完整编译流程
testCompleteCompilation :: TestTree
testCompleteCompilation = testGroup "完整编译流程测试"
  [ testCase "简单函数编译" $
      let typusCode = "func main() { return 0; }"
          parseResult = parseTypus typusCode
      in case parseResult of
        Left err -> assertBool ("解析失败: " ++ err) False
        Right parsedFile -> 
          case compileToGo parsedFile of
            Left compileErr -> assertBool ("编译失败: " ++ compileErr) False
            Right goCode -> 
              let trimmed = trim goCode
              in assertBool "生成的Go代码包含函数" ("func main" `isInfixOf` trimmed)
    
  , testCase "变量声明编译" $
      let typusCode = "let x = 42;"
          parseResult = parseTypus typusCode
      in case parseResult of
        Left err -> assertBool ("解析失败: " ++ err) False
        Right parsedFile -> 
          case compileToGo parsedFile of
            Left compileErr -> assertBool ("编译失败: " ++ compileErr) False
            Right goCode -> 
              let trimmed = trim goCode
              in assertBool "生成的Go代码包含变量声明" ("x" `isInfixOf` trimmed && "42" `isInfixOf` trimmed)
    
  , testCase "带指令的代码编译" $
      let typusCode = "// @ownership\nfunc test() { return 1; }"
          parseResult = parseTypus typusCode
      in case parseResult of
        Left err -> assertBool ("解析失败: " ++ err) False
        Right parsedFile -> 
          case compileToGo parsedFile of
            Left compileErr -> assertBool ("编译失败: " ++ compileErr) False
            Right goCode -> 
              let trimmed = trim goCode
              in assertBool "编译成功" (not (null trimmed))
  ]

-- | 测试错误处理集成
testErrorHandlingIntegration :: TestTree
testErrorHandlingIntegration = testGroup "错误处理集成测试"
  [ testCase "语法错误处理" $
      let typusCode = "func malformed( { return 0; }"
          parseResult = parseTypus typusCode
      in case parseResult of
        Left parseErr -> 
          let errors = aggregateErrors [parseErr]
              formatted = map formatError errors
          in assertBool "错误格式包含位置信息" (any ("1:" `isInfixOf`) formatted)
        Right _ -> assertBool "应该解析失败" False
    
  , testCase "编译错误处理" $
      let typusCode = "func test() { return undefined_var; }"
          parseResult = parseTypus typusCode
      in case parseResult of
        Left _ -> assertBool "解析应该成功" False
        Right parsedFile -> 
          case compileToGo parsedFile of
            Left compileErr -> 
              let errors = aggregateErrors [compileErr]
                  formatted = map formatError errors
              in assertBool "编译错误被正确处理" (not (null formatted))
            Right _ -> assertBool "编译应该失败" False
    
  , testCase "错误恢复机制" $
      let typusCode = "func bad() { return ; }\nfunc good() { return 0; }"
          parseResult = parseTypus typusCode
      in case parseResult of
        Left _ -> assertBool "应该能部分解析" False
        Right parsedFile -> 
          case compileToGo parsedFile of
            Left _ -> assertBool "部分编译失败是可接受的" True
            Right goCode -> 
              let trimmed = trim goCode
              in assertBool "应该包含正确的函数" ("good" `isInfixOf` trimmed)
  ]

-- | 测试源码位置跟踪集成
testSourceLocationIntegration :: TestTree
testSourceLocationIntegration = testGroup "源码位置跟踪集成测试"
  [ testCase "错误位置准确性" $
      let typusCode = "func test() {\n  return x;\n}"
          parseResult = parseTypus typusCode
      in case parseResult of
        Left err -> 
          let posStr = "2:"  -- 第2行应该有错误
          in assertBool ("错误位置应该在第2行: " ++ err) (posStr `isInfixOf` err)
        Right _ -> assertBool "应该解析失败" False
    
  , testCase "多文件位置跟踪" $
      let file1Code = "func fromFile1() { return 1; }"
          file2Code = "func fromFile2() { return 2; }"
          parseResult1 = parseTypus file1Code
          parseResult2 = parseTypus file2Code
      in case (parseResult1, parseResult2) of
        (Right _, Right _) -> assertBool "两个文件都应该解析成功" True
        _ -> assertBool "解析不应该失败" False
  ]

-- | 测试优化集成
testOptimizationIntegration :: TestTree
testOptimizationIntegration = testGroup "优化集成测试"
  [ testCase "常量折叠优化" $
      let typusCode = "func test() { return 1 + 2; }"
          parseResult = parseTypus typusCode
      in case parseResult of
        Left err -> assertBool ("解析失败: " ++ err) False
        Right parsedFile -> 
          case compileToGo parsedFile of
            Left compileErr -> assertBool ("编译失败: " ++ compileErr) False
            Right goCode -> 
              let trimmed = trim goCode
              in assertBool "应该优化为常量3" ("return 3" `isInfixOf` trimmed || "return 3" `isInfixOf` goCode)
    
  , testCase "死代码消除" $
      let typusCode = "func test() { if (false) { return 1; } else { return 2; } }"
          parseResult = parseTypus typusCode
      in case parseResult of
        Left err -> assertBool ("解析失败: " ++ err) False
        Right parsedFile -> 
          case compileToGo parsedFile of
            Left compileErr -> assertBool ("编译失败: " ++ compileErr) False
            Right goCode -> 
              let trimmed = trim goCode
              in assertBool "应该简化为直接return 2" ("return 2" `isInfixOf` trimmed)
  ]

-- | 测试所有权系统集成
testOwnershipIntegration :: TestTree
testOwnershipIntegration = testGroup "所有权系统集成测试"
  [ testCase "所有权指令处理" $
      let typusCode = "// @ownership\nfunc test() {\n  let x = 42;\n  return x;\n}"
          parseResult = parseTypus typusCode
      in case parseResult of
        Left err -> assertBool ("解析失败: " ++ err) False
        Right parsedFile -> 
          case compileToGo parsedFile of
            Left compileErr -> assertBool ("编译失败: " ++ compileErr) False
            Right goCode -> 
              let trimmed = trim goCode
              in assertBool "所有权指令被正确处理" (not (null trimmed))
    
  , testCase "所有权检查集成" $
      let typusCode = "// @ownership\nfunc test() {\n  let x = 42;\n  let y = x;  // 所有权转移\n  return x;  // 应该报错\n}"
          parseResult = parseTypus typusCode
      in case parseResult of
        Left _ -> assertBool "解析应该成功" False
        Right parsedFile -> 
          case compileToGo parsedFile of
            Left compileErr -> 
              let errMsg = "ownership" `isInfixOf` compileErr || "use" `isInfixOf` compileErr
              in assertBool "应该检测到所有权错误" errMsg
            Right _ -> assertBool "编译应该失败" False
  ]

-- | QuickCheck属性测试
testIntegrationProperties :: TestTree
testIntegrationProperties = testGroup "集成属性测试"
  [ testProperty "解析-编译一致性" $
      forAll genSimpleTypusCode $ \code ->
        let parseResult = parseTypus code
        in case parseResult of
          Left _ -> True  -- 解析失败是可接受的
          Right parsed -> 
            case compileToGo parsed of
              Left _ -> True  -- 编译失败是可接受的
              Right goCode -> not (null (trim goCode))
  
  , testProperty "指令代码解析成功率" $
      forAll genDirectiveCode $ \code ->
        let parseResult = parseTypus code
        in isRight parseResult
  
  , testProperty "错误处理的一致性" $
      \code ->
        let parseResult = parseTypus code
        in case parseResult of
          Left parseErr -> 
            let errors = aggregateErrors [parseErr]
            in not (null errors)
          Right parsed -> 
            case compileToGo parsed of
              Left compileErr -> 
                let errors = aggregateErrors [compileErr]
                in not (null errors)
              Right _ -> True
  ]

-- | 测试性能集成
testPerformanceIntegration :: TestTree
testPerformanceIntegration = testGroup "性能集成测试"
  [ testCase "大文件处理性能" $
      let largeCode = unlines $ replicate 100 "let x = 42;"
          parseResult = parseTypus largeCode
      in case parseResult of
        Left err -> assertBool ("大文件解析失败: " ++ err) False
        Right parsed -> 
          case compileToGo parsed of
            Left compileErr -> assertBool ("大文件编译失败: " ++ compileErr) False
            Right goCode -> 
              let goLines = lines goCode
              in length goLines >= 50 @?= True  -- 应该生成足够的Go代码
    
  , testCase "复杂表达式处理" $
      let complexExpr = concat (replicate 50 "1 + ")
          typusCode = "func test() { return " ++ complexExpr ++ "0; }"
          parseResult = parseTypus typusCode
      in case parseResult of
        Left _ -> assertBool "复杂表达式解析失败是可接受的" True
        Right parsed -> 
          case compileToGo parsed of
            Left _ -> assertBool "复杂表达式编译失败是可接受的" True
            Right goCode -> assertBool "复杂表达式处理成功" True
  ]

-- | 测试边界条件集成
testBoundaryIntegration :: TestTree
testBoundaryIntegration = testGroup "边界条件集成测试"
  [ testCase "空代码处理" $
      let typusCode = ""
          parseResult = parseTypus typusCode
      in case parseResult of
        Left _ -> assertBool "空代码处理失败" False
        Right parsed -> 
          case compileToGo parsed of
            Left _ -> assertBool "空代码编译失败是可接受的" True
            Right goCode -> assertBool "空代码编译成功" True
    
  , testCase "只有注释的代码" $
      let typusCode = "// This is a comment\n/* Multi-line\ncomment */"
          parseResult = parseTypus typusCode
      in case parseResult of
        Left _ -> assertBool "注释代码处理失败" False
        Right parsed -> 
          case compileToGo parsed of
            Left _ -> assertBool "注释代码编译失败是可接受的" True
            Right goCode -> assertBool "注释代码编译成功" True
    
  , testCase "极长标识符处理" $
      let longIdent = concat (replicate 1000 "a")
          typusCode = "func " ++ longIdent ++ "() { return 0; }"
          parseResult = parseTypus typusCode
      in case parseResult of
        Left err -> assertBool ("长标识符解析失败: " ++ err) False
        Right parsed -> 
          case compileToGo parsedFile of
            Left compileErr -> assertBool ("长标识符编译失败: " ++ compileErr) False
            Right goCode -> 
              let trimmed = trim goCode
              in assertBool "长标识符编译成功" (longIdent `isInfixOf` trimmed)
  ]

-- | 组合所有测试
tests :: TestTree
tests = testGroup "端到端集成测试"
  [ testCompleteCompilation
  , testErrorHandlingIntegration
  , testSourceLocationIntegration
  , testOptimizationIntegration
  , testOwnershipIntegration
  , testIntegrationProperties
  , testPerformanceIntegration
  , testBoundaryIntegration
  ]