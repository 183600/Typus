module Test.Unit.CompilerErrorBoundarySpec (tests) where

import Data.List (isInfixOf, isPrefixOf)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, assertFailure, testCase)

import qualified Compiler
import Compiler (CompilerError(..), CompilationPhase(..))
import qualified Parser
import Parser (TypusFile(..))

-- | 辅助函数：解析Typus代码
expectParse :: String -> IO TypusFile
expectParse source =
  case Parser.parseTypus source of
    Left err -> assertFailure $ "Failed to parse: " ++ show err
    Right typusFile -> return typusFile

-- | 辅助函数：期望编译失败
expectCompileFailure :: TypusFile -> IO CompilerError
expectCompileFailure typusFile =
  case Compiler.compile typusFile of
    Left err -> return err
    Right _ -> assertFailure "Expected compilation to fail"

-- | 测试编译器在错误条件和边界情况下的行为
tests :: TestTree
tests =
  testGroup "Compiler Error Boundary Tests"
    [ -- 空和最小输入测试
      testCase "handles empty file compilation" $ do
        let source = ""
        typusFile <- expectParse source
        result <- expectCompileFailure typusFile
        -- 应该产生关于缺少包声明的错误
        assertBool "Expected error about missing package" 
          (any (\msg -> "package" `isInfixOf` msg) (lines $ show result))

    , testCase "handles file with only package declaration" $ do
        let source = "package main\n"
        typusFile <- expectParse source
        result <- expectCompileFailure typusFile
        -- 应该产生关于缺少main函数的错误
        assertBool "Expected error about missing main function"
          (any (\msg -> "main" `isInfixOf` msg) (lines $ show result))

    -- 语法错误处理测试
    , testCase "detects invalid Go syntax" $ do
        let source = unlines
              [ "package main"
              , "func main() {"
              , "  if true"  -- 缺少花括号
              , "    println(\"hello\")"
              , "}"
              ]
        typusFile <- expectParse source
        result <- expectCompileFailure typusFile
        -- 应该检测到语法错误
        let errorMsg = show result
        assertBool "Expected syntax error detection" 
          (any (\msg -> "syntax" `isInfixOf` msg || "brace" `isInfixOf` msg) (lines errorMsg))

    , testCase "detects type mismatches" $ do
        let source = unlines
              [ "package main"
              , "func main() {"
              , "  var x int = \"hello\""  -- 类型不匹配
              , "  println(x)"
              , "}"
              ]
        typusFile <- expectParse source
        result <- expectCompileFailure typusFile
        -- 应该检测到类型错误
        let errorMsg = show result
        assertBool "Expected type error detection"
          (any (\msg -> "type" `isInfixOf` msg) (lines errorMsg))

    -- 依赖类型错误测试
    , testCase "detects dependent type violations" $ do
        let source = unlines
              [ "//! dependent_types: on"
              , "package main"
              , "func main() {"
              , "  // 故意创建一个违反依赖类型约束的情况"
              , "  var x Vector[int] = make(Vector[int], 5)"
              , "  var y int = x[10]"  -- 越界访问
              , "  println(y)"
              , "}"
              ]
        typusFile <- expectParse source
        result <- expectCompileFailure typusFile
        -- 应该检测到依赖类型错误
        let errorMsg = show result
        assertBool "Expected dependent type error"
          (any (\msg -> "dependent" `isInfixOf` msg || "bounds" `isInfixOf` msg) (lines errorMsg))

    -- 所有权错误测试
    , testCase "detects ownership violations" $ do
        let source = unlines
              [ "//! ownership: on"
              , "package main"
              , "func main() {"
              , "  var data = malloc(100)"
              , "  use(data)"
              , "  free(data)"
              , "  use(data)"  -- 使用已释放的内存
              , "}"
              , "func use(ptr *byte) {}"
              , "func free(ptr *byte) {}"
              , "func malloc(size int) *byte { return nil }"
              ]
        typusFile <- expectParse source
        result <- expectCompileFailure typusFile
        -- 应该检测到所有权错误
        let errorMsg = show result
        assertBool "Expected ownership error"
          (any (\msg -> "ownership" `isInfixOf` msg || "moved" `isInfixOf` msg) (lines errorMsg))

    -- 错误恢复测试
    , testCase "attempts error recovery with multiple errors" $ do
        let source = unlines
              [ "package main"
              , "func main() {"
              , "  var x int = \"hello\""  -- 错误1: 类型不匹配
              , "  var y bool = 123"      -- 错误2: 类型不匹配
              , "  println(x + y)"        -- 错误3: 操作符不匹配
              , "}"
              ]
        typusFile <- expectParse source
        result <- expectCompileFailure typusFile
        -- 应该检测到多个错误
        let errorMsg = show result
        let errorLines = lines errorMsg
        assertBool "Expected multiple errors" (length errorLines >= 2)
        assertBool "Expected type errors" 
          (any (\msg -> "type" `isInfixOf` msg) errorLines)

    -- 边界值测试
    , testCase "handles extreme values in compilation" $ do
        let source = unlines
              [ "package main"
              , "func main() {"
              , "  var maxInt int = 9223372036854775807"  // int64最大值
              , "  var minInt int = -9223372036854775808" // int64最小值
              , "  var bigUint uint = 18446744073709551615" // uint64最大值
              , "  println(maxInt, minInt, bigUint)"
              , "}"
              ]
        typusFile <- expectParse source
        result <- Compiler.compile typusFile
        case result of
          Left err -> assertFailure $ "Failed to compile extreme values: " ++ show err
          Right goCode -> do
            assertBool "Expected max int literal in output" ("9223372036854775807" `isInfixOf` goCode)
            assertBool "Expected min int literal in output" ("-9223372036854775808" `isInfixOf` goCode)
            assertBool "Expected max uint literal in output" ("18446744073709551615" `isInfixOf` goCode)

    -- 复杂嵌套结构测试
    , testCase "handles deeply nested structures" $ do
        let source = unlines
              [ "package main"
              , "type A struct { B *B }"
              , "type B struct { C *C }"
              , "type C struct { D *D }"
              , "type D struct { Value int }"
              , "func main() {"
              , "  var a A"
              , "  a.B = &B{C: &C{D: &D{Value: 42}}}"
              , "  println(a.B.C.D.Value)"
              , "}"
              ]
        typusFile <- expectParse source
        result <- Compiler.compile typusFile
        case result of
          Left err -> assertFailure $ "Failed to compile nested structures: " ++ show err
          Right goCode -> do
            assertBool "Expected struct definitions" ("type A struct" `isInfixOf` goCode)
            assertBool "Expected nested field access" ("a.B.C.D.Value" `isInfixOf` goCode)

    -- Unicode和特殊字符测试
    , testCase "handles Unicode in generated code" $ do
        let source = unlines
              [ "package main"
              , "func main() {"
              , "  var 问候 string = \"你好世界\""
              , "  var emoji string = \"🚀 Hello World 🌍\""
              , "  println(问候, emoji)"
              , "}"
              ]
        typusFile <- expectParse source
        result <- Compiler.compile typusFile
        case result of
          Left err -> assertFailure $ "Failed to compile Unicode strings: " ++ show err
          Right goCode -> do
            assertBool "Expected Unicode variable name" ("问候" `isInfixOf` goCode)
            assertBool "Expected Unicode string literal" ("你好世界" `isInfixOf` goCode)
            assertBool "Expected emoji in string" ("🚀" `isInfixOf` goCode)

    -- 性能边界测试
    , testCase "handles large functions" $ do
        let largeStatement = "  println(\"test line\")\n"
            source = unlines $
              [ "package main"
              , "func main() {"
              ] ++ replicate 500 largeStatement ++
              [ "}"
              ]
        typusFile <- expectParse source
        result <- Compiler.compile typusFile
        case result of
          Left err -> assertFailure $ "Failed to compile large function: " ++ show err
          Right goCode -> do
            let lineCount = length $ lines goCode
            assertBool "Expected many lines in compiled output" (lineCount > 400)

    -- 错误信息质量测试
    , testCase "provides helpful error messages" $ do
        let source = unlines
              [ "package main"
              , "func undefinedFunction() {"  -- 缺少闭合花括号
              , "  println(\"hello\")"
              ]
        typusFile <- expectParse source
        result <- expectCompileFailure typusFile
        let errorMsg = show result
        -- 错误信息应该包含有用的信息
        assertBool "Error message should contain line information" 
          (any (\msg -> "line" `isInfixOf` msg || "Line" `isInfixOf` msg) (lines errorMsg))
        assertBool "Error message should contain function name"
          (any (\msg -> "undefinedFunction" `isInfixOf` msg) (lines errorMsg))
    ]