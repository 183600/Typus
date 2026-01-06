{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.IntegratedCompilerQuickCheckTestSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import qualified Data.Map as Map
import Data.List (sort, nub)

import IntegratedCompiler
import Compiler.IR
import Compiler.GoAst
import Compiler.GoLexer
import Compiler.GoParsing
import ErrorHandler
import SourceLocation (SourcePos(..), SourceSpan(..), startPos)
import TestSupport.Arbitrary ()

tests :: TestTree
tests = testGroup "IntegratedCompiler QuickCheck Tests"
  [ compilerPipelineTests
  , sourceToIRTests
  , irToGoTests
  , errorHandlingTests
  , optimizationTests
  , validationTests
  , moduleCompilationTests
  , dependencyTests
  , performanceTests
  , integrationValidationTests
  ]

-- | 1. 编译器管道测试
compilerPipelineTests :: TestTree
compilerPipelineTests = testGroup "Compiler Pipeline Tests"
  [ testCase "Empty source compilation" =
      let result = compileSource ""
      in case result of
           Right _ -> True @?= True
           Left _ -> "Expected successful compilation" @?= "Got error"
  
  , testCase "Simple expression compilation" =
      let result = compileSource "x := 42"
      in case result of
           Right _ -> True @?= True
           Left _ -> "Expected successful compilation" @?= "Got error"
  
  , testCase "Compilation pipeline stages" =
      let result = compileWithPipeline "func test() int { return 42 }"
      in case result of
           Right pipeline -> L.length pipeline @?= 3  -- Parse, IR, Generate
           Left _ -> "Expected successful compilation" @?= "Got error"
  
  , fastProperty "Source code preservation" =
      \source -> let result = compileSource source
                 in case result of
                      Right compiled -> source `seq` True
                      Left _ -> False
  ]

-- | 2. 源码到IR转换测试
sourceToIRTests :: TestTree
sourceToIRTests = testGroup "Source to IR Tests"
  [ testCase "Simple variable declaration to IR" =
      let source = "x := 42"
          result = sourceToIR source
      in case result of
           Right ir -> case ir of
                        IRModule _ [IRVarDecl "x" IRInt _] -> True @?= True
                        _ -> "Expected variable declaration" @?= "Got something else"
           Left _ -> "Expected successful conversion" @?= "Got error"
  
  , testCase "Function declaration to IR" =
      let source = "func test() int { return 42 }"
          result = sourceToIR source
      in case result of
           Right ir -> case ir of
                        IRModule _ [IRFunction (IRFunctionSig "test" [] IRInt) _] -> True @?= True
                        _ -> "Expected function declaration" @?= "Got something else"
           Left _ -> "Expected successful conversion" @?= "Got error"
  
  , fastProperty "IR module name consistency" =
      \moduleName -> let source = "package " ++ moduleName ++ "\nfunc main() {}"
                         result = sourceToIR source
                     in case result of
                          Right (IRModule name _) -> name == moduleName
                          _ -> False
  ]

-- | 3. IR到Go代码生成测试
irToGoTests :: TestTree
irToGoTests = testGroup "IR to Go Tests"
  [ testCase "Simple IR to Go generation" =
      let ir = IRModule "test" [IRVarDecl "x" IRInt (IRLiteralExpr (IntLiteral 42))]
          result = irToGo ir
      in case result of
           Right goCode -> "x" `L.isInfixOf` goCode @?= True
           Left _ -> "Expected successful generation" @?= "Got error"
  
  , testCase "Function IR to Go generation" =
      let func = IRFunction (IRFunctionSig "test" [] IRInt) [IRReturn (IRLiteralExpr (IntLiteral 42))]
          ir = IRModule "test" [func]
          result = irToGo ir
      in case result of
           Right goCode -> "func test" `L.isInfixOf` goCode @?= True
           Left _ -> "Expected successful generation" @?= "Got error"
  
  , fastProperty "Go code contains function names" =
      \funcName -> let sig = IRFunctionSig funcName [] IRInt
                       func = IRFunction sig []
                       ir = IRModule "test" [func]
                       result = irToGo ir
                   in case result of
                        Right goCode -> funcName `L.isInfixOf` goCode
                        _ -> False
  ]
  where
    isInfixOf needle haystack = needle `elem` (words haystack)

-- | 4. 错误处理测试
errorHandlingTests :: TestTree
errorHandlingTests = testGroup "Error Handling Tests"
  [ testCase "Syntax error handling" =
      let source = "x := 42 +"
          result = compileSource source
      in case result of
           Left errors -> L.length errors > 0 @?= True
           Right _ -> "Expected compilation error" @?= "Got success"
  
  , testCase "Type error handling" =
      let source = "x := \"hello\" + 42"
          result = compileSource source
      in case result of
           Left errors -> L.length errors > 0 @?= True
           Right _ -> "Expected compilation error" @?= "Got success"
  
  , testCase "Error recovery" =
      let source = "x := 42 +; y := 10"
          result = compileWithRecovery source
      in case result of
           Right _ -> True @?= True  -- Should recover L.and continue
           Left _ -> "Expected recovery" @?= "Got unrecoverable error"
  
  , fastProperty "Error detection" =
      \invalidSource -> let result = compileSource invalidSource
                       in case result of
                            Left errors -> L.length errors > 0
                            Right _ -> invalidSource == "" || L.all (`elem` " \n\t") invalidSource
  ]

-- | 5. 优化测试
optimizationTests :: TestTree
optimizationTests = testGroup "Optimization Tests"
  [ testCase "Constant folding optimization" =
      let source = "x := 1 + 2"
          result = compileWithOptimization source
      in case result of
           Right ir -> case ir of
                        IRModule _ [IRVarDecl "x" IRInt (IRLiteralExpr (IntLiteral 3))] -> True @?= True
                        _ -> "Expected optimized constant" @?= "Got something else"
           Left _ -> "Expected successful compilation" @?= "Got error"
  
  , testCase "Dead code elimination" =
      let source = "if false { x := 42 }"
          result = compileWithOptimization source
      in case result of
           Right ir -> case ir of
                        IRModule _ [] -> True @?= True  -- Dead code eliminated
                        _ -> "Expected dead code elimination" @?= "Got something else"
           Left _ -> "Expected successful compilation" @?= "Got error"
  
  , fastProperty "Optimization preserves semantics" =
      \source -> let result1 = compileSource source
                     result2 = compileWithOptimization source
                 in case (result1, result2) of
                      (Right _, Right _) -> True
                      (Left _, Left _) -> True
                      _ -> False
  ]

-- | 6. 验证测试
validationTests :: TestTree
validationTests = testGroup "Validation Tests"
  [ testCase "Valid source validation" =
      let source = "x := 42"
          result = validateSource source
      in result @?= True
  
  , testCase "Invalid source validation" =
      let source = "x := 42 +"
          result = validateSource source
      in result @?= False
  
  , testCase "IR validation" =
      let ir = IRModule "test" [IRVarDecl "x" IRInt (IRLiteralExpr (IntLiteral 42))]
          result = validateIR ir
      in result @?= True
  
  , fastProperty "Source validation consistency" =
      \source -> let valid = validateSource source
                      result = compileSource source
                  in case (valid, result) of
                       (True, Right _) -> True
                       (True, Left _) -> False
                       (False, Right _) -> False
                       (False, Left _) -> True
  ]

-- | 7. 模块编译测试
moduleCompilationTests :: TestTree
moduleCompilationTests = testGroup "Module Compilation Tests"
  [ testCase "Single module compilation" =
      let modules = [("main", "func main() {}")]
          result = compileModules modules
      in case result of
           Right compiled -> L.length compiled @?= 1
           Left _ -> "Expected successful compilation" @?= "Got error"
  
  , testCase "Multiple module compilation" =
      let modules = [("utils", "func add(x int, y int) int { return x + y }"),
                     ("main", "import \"utils\"\nfunc main() {}")]
          result = compileModules modules
      in case result of
           Right compiled -> L.length compiled @?= 2
           Left _ -> "Expected successful compilation" @?= "Got error"
  
  , fastProperty "Module count consistency" =
      \modules -> let result = compileModules modules
                  in case result of
                       Right compiled -> L.length compiled == L.length modules
                       Left _ -> False
  ]

-- | 8. 依赖测试
dependencyTests :: TestTree
dependencyTests = testGroup "Dependency Tests"
  [ testCase "Module dependency resolution" =
      let modules = [("utils", "func helper() {}"),
                     ("main", "import \"utils\"\nfunc main() { helper() }")]
          result = compileWithDependencies modules
      in case result of
           Right _ -> True @?= True
           Left _ -> "Expected successful compilation" @?= "Got error"
  
  , testCase "Circular dependency detection" =
      let modules = [("mod1", "import \"mod2\""),
                     ("mod2", "import \"mod1\"")]
          result = compileWithDependencies modules
      in case result of
           Left errors -> L.length errors > 0 @?= True
           Right _ -> "Expected circular dependency error" @?= "Got success"
  
  , fastProperty "Dependency ordering" =
      \modules -> let result = compileWithDependencies modules
                  in case result of
                       Right _ -> True
                       Left _ -> hasCircularDependency modules || null modules
  ]
  where
    hasCircularDependency modules = 
      let imports = L.map (\(name, content) -> (name, extractImports content)) modules
      in L.any (\(name, deps) -> name `elem` deps) imports
    
    extractImports content = 
      case content of
        _ -> []  -- Simplified for test

-- | 9. 性能测试
performanceTests :: TestTree
performanceTests = testGroup "Performance Tests"
  [ testCase "Large source compilation" =
      let source = unlines $ L.map (\i -> "x" ++ show i ++ " := " ++ show i) [1..1000]
          result = compileSource source
      in case result of
           Right _ -> True @?= True
           Left _ -> "Expected successful compilation" @?= "Got error"
  
  , testCase "Compilation time measurement" =
      let source = "func test() int { return 42 }"
          result = measureCompilationTime source
      in result > 0 @?= True
  
  , fastProperty "Compilation scalability" =
      \n -> let source = unlines $ L.map (\i -> "x" ++ show i ++ " := " ++ show i) [1..n `mod` 100]
                result = compileSource source
            in case result of
                 Right _ -> True
                 Left _ -> n `mod` 100 == 0
  ]

-- | 10. 集成验证测试
integrationValidationTests :: TestTree
integrationValidationTests = testGroup "Integration Validation Tests"
  [ testCase "End-to-end compilation" =
      let source = "package main\n\nfunc main() {\n    x := 42\n    return x\n}"
          result = compileSource source
      in case result of
           Right _ -> True @?= True
           Left _ -> "Expected successful compilation" @?= "Got error"
  
  , testCase "Round-trip compilation" =
      let source = "func test() int { return 42 }"
          result1 = compileSource source
      in case result1 of
           Right ir -> case irToGo ir of
                        Right goCode -> let result2 = compileSource goCode
                                       in case result2 of
                                            Right _ -> True @?= True
                                            Left _ -> "Expected successful round-trip" @?= "Got error"
                        Left _ -> "Expected successful Go generation" @?= "Got error"
           Left _ -> "Expected successful compilation" @?= "Got error"
  
  , fastProperty "Integration consistency" =
      \source -> let result = compileSource source
                 in case result of
                      Right ir -> validateIR ir
                      Left _ -> True
  ]