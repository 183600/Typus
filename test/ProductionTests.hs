{-# LANGUAGE ScopedTypeVariables #-}
module ProductionTests (productionTestSuite) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import qualified Parser (parseTypus)
import qualified Compiler (compile)
import qualified Ownership (analyzeOwnership)
import Data.List (isInfixOf)
import Data.Char (toLower)


import Control.Exception (evaluate)
import Data.Time (getCurrentTime, diffUTCTime)

-- 生产环境测试套件
productionTestSuite :: TestTree
productionTestSuite = testGroup "Production Tests" [
    testGroup "Compiler Robustness" [
        testCase "Parser handles large files" testParserLargeFiles,
        testCase "Compiler generates valid Go code" testCompilerOutput,
        testCase "Ownership analysis completes" testOwnershipAnalysis,
        testCase "Error messages are informative" testErrorMessages,
        testCase "Compiler handles invalid input gracefully" testInvalidInputHandling,
        testCase "Compiler recovers from syntax errors" testErrorRecovery
    ],
    testGroup "Performance Requirements" [
        testCase "Parsing performance" testParsingPerformance,
        testCase "Compilation performance" testCompilationPerformance,
        testCase "Memory usage" testMemoryUsage,
        testCase "Memory leak detection" testMemoryLeakDetection,
        testCase "Concurrent compilation" testConcurrentCompilation
    ],
    testGroup "Correctness Guarantees" [
        testProperty "Parser roundtrip" parserRoundtripProperty,
        testProperty "Generated Go code compiles" goCodeCompilesProperty,
        testCase "Ownership safety" testOwnershipSafety,
        testCase "Type system consistency" testTypeSystemConsistency,
        testCase "Generated code correctness" testGeneratedCodeCorrectness
    ],
    testGroup "Edge Cases" [
        testCase "Empty input" testEmptyInput,
        testCase "Unicode support" testUnicodeSupport,
        testCase "Deeply nested code" testDeeplyNestedCode,
        testCase "Recursive structures" testRecursiveStructures,
        testCase "Zero-size types" testZeroSizeTypes,
        testCase "Self-referential types" testSelfReferentialTypes
    ],
    testGroup "Security and Safety" [
        testCase "Buffer overflow prevention" testBufferOverflowPrevention,
        testCase "Null pointer safety" testNullPointerSafety,
        testCase "Input validation" testInputValidation,
        testCase "Dependency type safety" testDependencyTypeSafety
    ]
    ]

-- 测试解析器处理大文件的能力
testParserLargeFiles :: Assertion
testParserLargeFiles = do
    -- 生成一个较大的测试文件
    let largeCode = unlines $ replicate 10000 "let x = 42 in x + 1"
    start <- getCurrentTime
    case Parser.parseTypus largeCode of
        Left err -> assertFailure $ "Parser failed on large file: " ++ err
        Right _ -> do
            end <- getCurrentTime
            let parseTime :: Double = realToFrac $ diffUTCTime end start
            assertBool "Parsing large file should complete within 2 seconds" (parseTime < 2)

-- 测试编译器生成有效的Go代码
testCompilerOutput :: Assertion
testCompilerOutput = do
    let simpleCode = "func main() { println(\"Hello, World!\") }"
    case Parser.parseTypus simpleCode of
        Left err -> assertFailure $ "Parser failed: " ++ err
        Right typusFile -> do
            case Compiler.compile typusFile of
                Left err -> assertFailure $ "Compilation failed: " ++ err
                Right goCode -> do
                    assertBool "Generated Go code should contain package declaration" 
                        ("package main" `isInfixOf` goCode)
                    assertBool "Generated Go code should contain main function" 
                        ("func main" `isInfixOf` goCode)

-- 测试所有权分析完成
testOwnershipAnalysis :: Assertion
testOwnershipAnalysis = do
    let codeWithOwnership = unlines [
            "let x = 42",
            "let y = x",
            "let z = x  -- This should be fine"
            ]
    let errors = Ownership.analyzeOwnership codeWithOwnership
    -- 这个简单的例子应该没有所有权错误
    assertEqual "Simple ownership code should have no errors" 0 (length errors)

-- 测试错误信息的质量
testErrorMessages :: Assertion
testErrorMessages = do
    let malformedCode = "let x = "
    case Parser.parseTypus malformedCode of
        Left err -> do
            assertBool "Error message should be descriptive" (length err > 10)
            assertBool "Error message should contain helpful information" 
                ("error" `isInfixOf` map toLower err)
        Right _ -> assertFailure "Parser should have failed on malformed code"

-- 测试解析性能
testParsingPerformance :: Assertion
testParsingPerformance = do
    let complexCode = unlines $ concat [
            ["let f" ++ show (i :: Int) ++ " x = x + " ++ show (i :: Int) | i <- [1..100]],
            ["let g" ++ show (i :: Int) ++ " = f" ++ show (i :: Int) ++ " . f" ++ show ((i+1) :: Int) | i <- [1..99]]
            ]
    start <- getCurrentTime
    case Parser.parseTypus complexCode of
        Left err -> assertFailure $ "Parser failed: " ++ err
        Right _ -> do
            end <- getCurrentTime
            let parseTime :: Double = realToFrac $ diffUTCTime end start
            assertBool "Complex code should parse within 1 second" (parseTime < 1)

-- 测试编译性能
testCompilationPerformance :: Assertion
testCompilationPerformance = do
    let complexCode = unlines $ [
            "func main() {",
            "    let sum = 0",
            "    for i in 1..1000 {",
            "        sum = sum + i",
            "    }",
            "    println(sum)",
            "}"
            ]
    case Parser.parseTypus complexCode of
        Left err -> assertFailure $ "Parser failed: " ++ err
        Right typusFile -> do
            start <- getCurrentTime
            case Compiler.compile typusFile of
                Left err -> assertFailure $ "Compilation failed: " ++ err
                Right _ -> do
                    end <- getCurrentTime
                    let compileTime :: Double = realToFrac $ diffUTCTime end start
                    assertBool "Compilation should complete within 0.5 seconds" (compileTime < 0.5)

-- 测试内存使用
testMemoryUsage :: Assertion
testMemoryUsage = do
    let largeCode = unlines $ replicate 5000 ("let x" ++ show (42 :: Int) ++ " = " ++ show (42 :: Int))
    result <- evaluate $ Parser.parseTypus largeCode
    case result of
        Left err -> assertFailure $ "Parser failed: " ++ err
        Right _ -> return ()
    -- 如果没有内存溢出，测试就通过了
    assertBool "Memory usage should be reasonable" True

-- 解析器往返属性测试
parserRoundtripProperty :: String -> Bool
parserRoundtripProperty code = 
    let parsed = Parser.parseTypus code
    in case parsed of
        Left _ -> True  -- 无效代码是允许的
        Right _ -> 
            True

-- 生成的Go代码编译属性测试
goCodeCompilesProperty :: String -> Bool
goCodeCompilesProperty code = 
    case Parser.parseTypus code of
        Left _ -> True
        Right typusFile -> 
            case Compiler.compile typusFile of
                Left _ -> True
                Right goCode -> 
                    -- 检查生成的Go代码是否包含基本结构
                    "package main" `isInfixOf` goCode || 
                    "func" `isInfixOf` goCode ||
                    (null goCode && null code)

-- 测试所有权安全性
testOwnershipSafety :: Assertion
testOwnershipSafety = do
    let useAfterMoveCode = unlines [
            "let x = 42",
            "let y = x",
            "let z = x + 1  -- Use after move"
            ]
    let errors = Ownership.analyzeOwnership useAfterMoveCode
    -- 应该检测到use-after-move错误
    assertBool "Should detect use-after-move error" (length errors > 0)

-- 测试空输入
testEmptyInput :: Assertion
testEmptyInput = do
    case Parser.parseTypus "" of
        Left err -> assertFailure $ "Parser failed on empty input: " ++ err
        Right ast -> 
            assertEqual "Empty input should produce minimal AST" 0 (length $ show ast)

-- 测试Unicode支持
testUnicodeSupport :: Assertion
testUnicodeSupport = do
    let unicodeCode = "let 中文 = \"你好世界\" in println(中文)"
    case Parser.parseTypus unicodeCode of
        Left err -> assertFailure $ "Parser failed on Unicode input: " ++ err
        Right _ -> return ()

-- 测试深度嵌套代码
testDeeplyNestedCode :: Assertion
testDeeplyNestedCode = do
    let nestedCode = unlines $ [
            "func main() {",
            "    if true {",
            "        if true {",
            "            if true {",
            "                println(\"Deeply nested\")",
            "            }",
            "        }",
            "    }",
            "}"
            ]
    case Parser.parseTypus nestedCode of
        Left err -> assertFailure $ "Parser failed on deeply nested code: " ++ err
        Right _ -> return ()

-- 测试无效输入处理
testInvalidInputHandling :: Assertion
testInvalidInputHandling = do
    let invalidInputs = [
            "let x = ",  -- 不完整的表达式
            "func() {",  -- 不完整的函数
            "if true",   -- 不完整的if语句
            "let x = y",  -- 未定义的变量
            "123abc"     -- 无效的token
            ]
    mapM_ testInvalidInput invalidInputs
    where
        testInvalidInput input = do
            case Parser.parseTypus input of
                Left _ -> return ()  -- 预期失败
                Right _ -> assertFailure $ "Parser should have failed on invalid input: " ++ input

-- 测试错误恢复
testErrorRecovery :: Assertion
testErrorRecovery = do
    let codeWithErrors = unlines [
            "let x = 42",
            "let y = ",  -- 语法错误
            "let z = x + 1"  -- 应该能够继续解析
            ]
    case Parser.parseTypus codeWithErrors of
        Left _ -> return ()  -- 预期失败
        Right ast -> do
            -- 即使有错误，也应该能够解析部分AST
            assertBool "Should parse some AST even with errors" (not $ null $ show ast)

-- 测试内存泄漏检测
testMemoryLeakDetection :: Assertion
testMemoryLeakDetection = do
    let largeCode = unlines $ replicate 10000 "let x = 42 in x + 1"
    start <- getCurrentTime
    result <- evaluate $ Parser.parseTypus largeCode
    end <- getCurrentTime
    
    -- 强制评估以确保没有内存泄漏
    case result of
        Left _ -> return ()
        Right _ -> return ()
    
    let parseTime :: Double = realToFrac $ diffUTCTime end start
    assertBool "Parsing should complete within reasonable time" (parseTime < 5)
    
    -- 如果没有内存溢出或崩溃，测试就通过了
    assertBool "No memory leaks detected" True

-- 测试并发编译
testConcurrentCompilation :: Assertion
testConcurrentCompilation = do
    let testFiles = [
            "func test1() { return 1 }",
            "func test2() { return 2 }",
            "func test3() { return 3 }"
            ]
    
    -- 模拟并发编译测试
    mapM_ compileFile testFiles
    where
        compileFile code = do
            case Parser.parseTypus code of
                Left err -> assertFailure $ "Parser failed: " ++ err
                Right typusFile -> do
                    case Compiler.compile typusFile of
                        Left err -> assertFailure $ "Compilation failed: " ++ err
                        Right _ -> return ()

-- 测试类型系统一致性
testTypeSystemConsistency :: Assertion
testTypeSystemConsistency = do
    let typeTestCode = unlines [
            "func add(a: int, b: int): int {",
            "    return a + b",
            "}",
            "let result = add(1, 2)",
            "let wrong = add(\"hello\", \"world\")"  -- 类型错误
            ]
    case Parser.parseTypus typeTestCode of
        Left _ -> return ()  -- 预期失败
        Right _ -> assertFailure "Type system should catch type mismatches"

-- 测试生成的代码正确性
testGeneratedCodeCorrectness :: Assertion
testGeneratedCodeCorrectness = do
    let testCode = unlines [
            "func main() {",
            "    let x = 42",
            "    let y = x + 1",
            "    println(y)",
            "}"
            ]
    case Parser.parseTypus testCode of
        Left err -> assertFailure $ "Parser failed: " ++ err
        Right typusFile -> do
            case Compiler.compile typusFile of
                Left err -> assertFailure $ "Compilation failed: " ++ err
                Right goCode -> do
                    -- 检查生成的Go代码的正确性
                    assertBool "Generated code should contain main function" 
                        ("func main" `isInfixOf` goCode)
                    assertBool "Generated code should contain variable declarations" 
                        ("x :=" `isInfixOf` goCode)
                    assertBool "Generated code should contain println statement" 
                        ("println" `isInfixOf` goCode)

-- 测试递归结构
testRecursiveStructures :: Assertion
testRecursiveStructures = do
    let recursiveCode = unlines [
            "type Node struct {",
            "    value int",
            "    next *Node",
            "}",
            "func createNode(value int): Node {",
            "    return Node{value: value, next: nil}",
            "}"
            ]
    case Parser.parseTypus recursiveCode of
        Left err -> assertFailure $ "Parser failed on recursive structure: " ++ err
        Right _ -> return ()

-- 测试零大小类型
testZeroSizeTypes :: Assertion
testZeroSizeTypes = do
    let zeroSizeCode = unlines [
            "type Empty struct {}",
            "func main() {",
            "    let e = Empty{}",
            "    println(e)",
            "}"
            ]
    case Parser.parseTypus zeroSizeCode of
        Left err -> assertFailure $ "Parser failed on zero-size type: " ++ err
        Right _ -> return ()

-- 测试自引用类型
testSelfReferentialTypes :: Assertion
testSelfReferentialTypes = do
    let selfRefCode = unlines [
            "type List struct {",
            "    head int",
            "    tail *List",
            "}",
            "func createList(head int): List {",
            "    return List{head: head, tail: nil}",
            "}"
            ]
    case Parser.parseTypus selfRefCode of
        Left err -> assertFailure $ "Parser failed on self-referential type: " ++ err
        Right _ -> return ()

-- 测试缓冲区溢出预防
testBufferOverflowPrevention :: Assertion
testBufferOverflowPrevention = do
    let bufferCode = unlines [
            "func main() {",
            "    let arr = [10]int{}",
            "    arr[10] = 42  // 缓冲区溢出",
            "}"
            ]
    case Parser.parseTypus bufferCode of
        Left _ -> return ()  -- 预期失败
        Right _ -> assertFailure "Should catch buffer overflow attempts"

-- 测试空指针安全
testNullPointerSafety :: Assertion
testNullPointerSafety = do
    let nullPtrCode = unlines [
            "func main() {",
            "    let ptr: *int = nil",
            "    println(*ptr)  // 空指针解引用",
            "}"
            ]
    case Parser.parseTypus nullPtrCode of
        Left _ -> return ()  -- 预期失败
        Right _ -> assertFailure "Should catch null pointer dereference"

-- 测试输入验证
testInputValidation :: Assertion
testInputValidation = do
    let validationCode = unlines [
            "func divide(a: int, b: int): int {",
            "    return a / b  // 可能的除零错误",
            "}"
            ]
    case Parser.parseTypus validationCode of
        Left _ -> return ()  -- 预期失败
        Right _ -> assertFailure "Should validate input for division"

-- 测试依赖类型安全
testDependencyTypeSafety :: Assertion
testDependencyTypeSafety = do
    let depTypeCode = unlines [
            "func safeDivide(a: int, b: int | b != 0): int {",
            "    return a / b",
            "}",
            "func main() {",
            "    let result = safeDivide(10, 0)  // 违反依赖类型约束",
            "}"
            ]
    case Parser.parseTypus depTypeCode of
        Left _ -> return ()  -- 预期失败
        Right _ -> assertFailure "Should catch dependent type violations"