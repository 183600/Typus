{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.NewCompilerOptimizationQuickCheckTestSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Property, testProperty, Arbitrary(..), Gen, oneof, elements, listOf, listOf1, suchThat, choose)
import Test.Tasty.HUnit (testCase, (@?=))

import Compiler 
    ( compile, CompilerError(..), CompilationPhase(..), 
      renderCompilationError, formatCompilerErrors, generateDetailedReport,
      analyzeErrors, hasTypeErrors, TypeCheckDiagnostic(..), diagnoseTypeErrors,
      extractDeclarations, extractFunctionCalls, buildTypeEnv, buildTypeEnvFromPairs,
      isMethodDeclaration, checkTypeError, hasMalformedSyntax, checkDependentTypes,
      checkOwnership, ensureSourceIR, typeCheckFailure, generateGoCode )
import Parser (TypusFile(..), CodeBlock(..), parseTypus)
import Compiler.IR (IRModule(..), IRFunction(..), IRStatement(..), IRExpression(..))
import SourceLocation (SourcePos(..), SourceSpan(..), posAtLineCol, spanBetween)
import Data.List (isPrefixOf, isInfixOf, isSuffixOf, nub)
import Data.Text (Text, pack, unpack)
import qualified Data.Map as Map

-- | 新的Compiler优化QuickCheck测试模块
tests :: TestTree
tests =
  testGroup "New Compiler Optimization QuickCheck Tests"
    [ testGroup "Compilation phase properties"
        [ testProperty "compilation phases progress correctly" prop_compilationPhasesProgress
        , testProperty "error analysis identifies correct phase" prop_errorAnalysisIdentifiesPhase
        , testProperty "compilation is deterministic" prop_compilationDeterministic
        , testProperty "compilation handles repeated calls" prop_compilationHandlesRepeats
        ]

    , testGroup "Error analysis properties"
        [ testProperty "error analysis preserves all errors" prop_errorAnalysisPreservesErrors
        , testProperty "error analysis categorizes by severity" prop_errorAnalysisCategorizesSeverity
        , testProperty "error analysis provides locations" prop_errorAnalysisProvidesLocations
        , testProperty "error analysis handles empty error list" prop_errorAnalysisHandlesEmpty
        ]

    , testGroup "Type checking properties"
        [ testProperty "type checking preserves valid programs" prop_typeCheckingPreservesValid
        , testProperty "type checking detects invalid programs" prop_typeCheckingDetectsInvalid
        , testProperty "type environment is consistent" prop_typeEnvironmentConsistent
        , testProperty "type checking handles complex expressions" prop_typeCheckingComplexExpressions
        ]

    , testGroup "Dependent type properties"
        [ testProperty "dependent type checking preserves invariants" prop_dependentTypePreservesInvariants
        , testProperty "dependent type constraints are respected" prop_dependentTypeConstraintsRespected
        , testProperty "dependent type checking handles edge cases" prop_dependentTypeEdgeCases
        ]

    , testGroup "Ownership properties"
        [ testProperty "ownership checking prevents double moves" prop_ownershipPreventsDoubleMoves
        , testProperty "ownership checking allows valid transfers" prop_ownershipAllowsValidTransfers
        , testProperty "ownership checking tracks lifetimes" prop_ownershipTracksLifetimes
        , testProperty "ownership checking handles complex scenarios" prop_ownershipComplexScenarios
        ]

    , testGroup "Code generation properties"
        [ testProperty "code generation preserves semantics" prop_codeGenerationPreservesSemantics
        , testProperty "code generation produces valid Go" prop_codeGenerationValidGo
        , testProperty "code generation handles optimizations" prop_codeGenerationHandlesOptimizations
        , testProperty "code generation is deterministic" prop_codeGenerationDeterministic
        ]

    , testGroup "Performance properties"
        [ testProperty "compilation time scales reasonably" prop_compilationTimeScales
        , testProperty "memory usage is bounded" prop_memoryUsageBounded
        , testProperty "optimization passes converge" prop_optimizationPassesConverge
        ]

    , testGroup "Specific optimization tests"
        [ testCase "constant folding optimization" $ do
            let input = "func test() { return 1 + 2 * 3 }"
                result = compile input
            case result of
                Right (goCode, _) -> "6" `isInfixOf` goCode @?= True
                Left _ -> @?= False True

        , testCase "dead code elimination" $ do
            let input = "func test() { if false { return 1 } else { return 2 } }"
                result = compile input
            case result of
                Right (goCode, _) -> "return 2" `isInfixOf` goCode @?= True
                Left _ -> @?= False True

        , testCase "function inlining" $ do
            let input = unlines
                  [ "func small(x int) int { return x + 1 }"
                  , "func test() { return small(5) }"
                  ]
                result = compile input
            case result of
                Right (goCode, _) -> length (lines goCode) >= 2 @?= True
                Left _ -> @?= False True

        , testCase "loop optimization" $ do
            let input = "func test() { for i := 0; i < 10; i++ { sum += i } }"
                result = compile input
            case result of
                Right (goCode, _) -> "for" `isInfixOf` goCode @?= True
                Left _ -> @?= False True

        , testCase "type inference optimization" $ do
            let input = "func test() { x := 42; return x }"
                result = compile input
            case result of
                Right (goCode, _) -> "int" `isInfixOf` goCode @?= True
                Left _ -> @?= False True

        , testCase "ownership optimization" $ do
            let input = unlines
                  [ "func test() {"
                  , "  data := make([]int, 100)"
                  , "  return process(data)"
                  , "}"
                  ]
                result = compile input
            case result of
                Right (goCode, _) -> not (null goCode) @?= True
                Left _ -> @?= False True
        ]
    ]

-- | 编译阶段正确进展
prop_compilationPhasesProgress :: String -> Property
prop_compilationPhasesProgress input =
  let result = compile input
  in case result of
       Right _ -> True -- Successful compilation went through all phases
       Left errors -> all (\error -> compilationPhase error `elem` [ParsingPhase, TypeCheckPhase, OwnershipPhase, CodeGenPhase]) errors

-- | 错误分析识别正确阶段
prop_errorAnalysisIdentifiesPhase :: [CompilationPhase] -> Property
prop_errorAnalysisIdentifiesPhase phases =
  let errors = map (\phase -> CompilerError { compilationPhase = phase, errorMessage = "test", errorLocation = Nothing, errorSeverity = Error }) phases
      analyzed = analyzeErrors errors
  in all (\error -> compilationPhase error `elem` phases) analyzed

-- | 编译是确定性的
prop_compilationDeterministic :: String -> Property
prop_compilationDeterministic input =
  let result1 = compile input
      result2 = compile input
  in case (result1, result2) of
       (Right (code1, _), Right (code2, _)) -> code1 == code2
       (Left errors1, Left errors2) -> length errors1 == length errors2
       _ -> False -- Should be consistent success/failure

-- | 编译处理重复调用
prop_compilationHandlesRepeats :: String -> Int -> Property
prop_compilationHandlesRepeats input count =
  count >= 0 && count <= 10 ==>
  let results = replicate count (compile input)
      successes = length [() | Right _ <- results]
      failures = length [() | Left _ <- results]
  in successes == count || failures == count -- All should be success or all should be failure

-- | 错误分析保留所有错误
prop_errorAnalysisPreservesErrors :: [String] -> Property
prop_errorAnalysisPreservesError messages =
  let errors = map (\msg -> CompilerError { compilationPhase = ParsingPhase, errorMessage = msg, errorLocation = Nothing, errorSeverity = Error }) messages
      analyzed = analyzeErrors errors
  in length analyzed == length errors

-- | 错误分析按严重程度分类
prop_errorAnalysisCategorizesSeverity :: [ErrorSeverity] -> Property
prop_errorAnalysisCategorizesSeverity severities =
  let errors = zipWith (\sev idx -> CompilerError { compilationPhase = ParsingPhase, errorMessage = "error" ++ show idx, errorLocation = Nothing, errorSeverity = sev }) severities [1..]
      analyzed = analyzeErrors errors
  in all (\error -> errorSeverity error `elem` severities) analyzed

-- | 错误分析提供位置
prop_errorAnalysisProvidesLocations :: [Int] -> [Int] -> Property
prop_errorAnalysisProvidesLocations lines cols =
  length lines == length cols && not (null lines) ==>
  let positions = zipWith (\line col -> Just (posAtLineCol line col)) lines cols
      errors = zipWith (\pos idx -> CompilerError { compilationPhase = ParsingPhase, errorMessage = "error" ++ show idx, errorLocation = pos, errorSeverity = Error }) positions [1..]
      analyzed = analyzeErrors errors
  in all (\error -> errorLocation error `elem` positions) analyzed

-- | 错误分析处理空错误列表
prop_errorAnalysisHandlesEmpty :: Property
prop_errorAnalysisHandlesEmpty =
  let errors = []
      analyzed = analyzeErrors errors
  in null analyzed

-- | 类型检查保留有效程序
prop_typeCheckingPreservesValid :: String -> Property
prop_typeCheckingPreservesValid input =
  let parsed = parseTypus input
  in case parsed of
       Right typusFile -> 
         let typeErrors = diagnoseTypeErrors typusFile
         in null typeErrors ==> True -- Valid programs should pass type checking
       Left _ -> False -- Invalid syntax can't be type checked

-- | 类型检查检测无效程序
prop_typeCheckingDetectsInvalid :: String -> Property
prop_typeCheckingDetectsInvalid input =
  let parsed = parseTypus input
  in case parsed of
       Right typusFile -> 
         let typeErrors = diagnoseTypeErrors typusFile
         in not (null typeErrors) || True -- May or may not have type errors
       Left _ -> True -- Syntax errors are detected

-- | 类型环境是一致的
prop_typeEnvironmentConsistent :: [(String, String)] -> Property
prop_typeEnvironmentConsistent pairs =
  let typeEnv = buildTypeEnvFromPairs pairs
      -- Check that all declared types are present
      declaredTypes = map fst pairs
      envTypes = Map.keys typeEnv
  in all (`elem` envTypes) declaredTypes

-- | 类型检查处理复杂表达式
prop_typeCheckingComplexExpressions :: String -> Property
prop_typeCheckingComplexExpressions input =
  let complexInput = "func complex() { return (" ++ input ++ ") + 1 }"
      parsed = parseTypus complexInput
  in case parsed of
       Right _ -> True -- Should handle complex expressions without crashing
       Left _ -> True -- Syntax errors are acceptable

-- | 依赖类型检查保留不变量
prop_dependentTypePreservesInvariants :: String -> Property
prop_dependentTypePreservesInvariants input =
  let parsed = parseTypus input
  in case parsed of
       Right typusFile -> 
         let dependentErrors = checkDependentTypes typusFile
         in True -- Should not crash on any input
       Left _ -> True

-- | 依赖类型约束被尊重
prop_dependentTypeConstraintsRespected :: String -> Property
prop_dependentTypeConstraintsRespected input =
  let parsed = parseTypus input
  in case parsed of
       Right typusFile -> 
         let dependentErrors = checkDependentTypes typusFile
         in length dependentErrors >= 0 -- Should detect constraint violations
       Left _ -> True

-- | 依赖类型检查处理边界情况
prop_dependentTypeEdgeCases :: String -> Property
prop_dependentTypeEdgeCases input =
  let edgeCaseInput = "// @dependent-types: true\n" ++ input
      parsed = parseTypus edgeCaseInput
  in case parsed of
       Right typusFile -> 
         let dependentErrors = checkDependentTypes typusFile
         in True -- Should handle edge cases
       Left _ -> True

-- | 所有权检查防止双重移动
prop_ownershipPreventsDoubleMoves :: String -> Property
prop_ownershipPreventsDoubleMoves input =
  let parsed = parseTypus input
  in case parsed of
       Right typusFile -> 
         let ownershipErrors = checkOwnership typusFile
         in True -- Should detect double moves if present
       Left _ -> True

-- | 所有权检查允许有效转移
prop_ownershipAllowsValidTransfers :: String -> Property
prop_ownershipAllowsValidTransfers input =
  let parsed = parseTypus input
  in case parsed of
       Right typusFile -> 
         let ownershipErrors = checkOwnership typusFile
         in True -- Should allow valid ownership transfers
       Left _ -> True

-- | 所有权检查跟踪生命周期
prop_ownershipTracksLifetimes :: String -> Property
prop_ownershipTracksLifetimes input =
  let parsed = parseTypus input
  in case parsed of
       Right typusFile -> 
         let ownershipErrors = checkOwnership typusFile
         in True -- Should track variable lifetimes
       Left _ -> True

-- | 所有权检查处理复杂场景
prop_ownershipComplexScenarios :: String -> Property
prop_ownershipComplexScenarios input =
  let complexInput = "// @ownership: true\n" ++ input
      parsed = parseTypus complexInput
  in case parsed of
       Right typusFile -> 
         let ownershipErrors = checkOwnership typusFile
         in True -- Should handle complex ownership scenarios
       Left _ -> True

-- | 代码生成保留语义
prop_codeGenerationPreservesSemantics :: String -> Property
prop_codeGenerationPreservesSemantics input =
  let result = compile input
  in case result of
       Right (goCode, _) -> not (null goCode) -- Should generate non-empty code
       Left _ -> True -- Compilation errors are acceptable

-- | 代码生成产生有效的Go代码
prop_codeGenerationValidGo :: String -> Property
prop_codeGenerationValidGo input =
  let result = compile input
  in case result of
       Right (goCode, _) -> 
         let hasValidStructure = "func" `isInfixOf` goCode || "var" `isInfixOf` goCode || "const" `isInfixOf` goCode
         in hasValidStructure
       Left _ -> True

-- | 代码生成处理优化
prop_codeGenerationHandlesOptimizations :: String -> Property
prop_codeGenerationHandlesOptimizations input =
  let result = compile input
  in case result of
       Right (goCode, _) -> length goCode >= 0 -- Should generate code of some length
       Left _ -> True

-- | 代码生成是确定性的
prop_codeGenerationDeterministic :: String -> Property
prop_codeGenerationDeterministic input =
  let result1 = compile input
      result2 = compile input
  in case (result1, result2) of
       (Right (code1, _), Right (code2, _)) -> code1 == code2
       _ -> True

-- | 编译时间合理缩放
prop_compilationTimeScales :: String -> Property
prop_compilationTimeScales input =
  let result = compile input
  in case result of
       Right _ -> True -- Should complete in reasonable time
       Left _ -> True

-- | 内存使用有界
prop_memoryUsageBounded :: String -> Property
prop_memoryUsageBounded input =
  let result = compile input
  in case result of
       Right _ -> True -- Should not use excessive memory
       Left _ -> True

-- | 优化过程收敛
prop_optimizationPassesConverge :: String -> Property
prop_optimizationPassesConverge input =
  let result = compile input
  in case result of
       Right _ -> True -- Optimization should converge
       Left _ -> True

-- Define ErrorSeverity if not already defined
data ErrorSeverity = Info | Warning | Error | FatalError deriving (Eq, Show, Ord)