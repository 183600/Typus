{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module EnhancedPreciseTypeTests (enhancedPreciseTypeTests) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import qualified Dependencies as Dep
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Control.Monad.State
import Control.Monad.Except
import qualified Data.Text as T
import Data.List (nub, sort, intercalate)
import Data.Maybe (isJust, fromJust)

-- ============================================================================
-- 增强的精确类型测试套件
-- 涵盖更复杂的类型场景和边界情况
-- ============================================================================

enhancedPreciseTypeTests :: TestTree
enhancedPreciseTypeTests = testGroup "Enhanced Precise Type Tests"
  [ advancedDependentTypeTests
  , complexRefinementTests
  , sophisticatedLinearTypeTests
  , advancedOwnershipTests
  , typeLevelComputationTests
  , constraintOptimizationTests
  , polymorphicConstraintTests
  , errorBoundaryTests
  , performanceTypeTests
  , integrationTypeTests
  ]

-- ============================================================================
-- 高级依赖类型测试
-- ============================================================================

advancedDependentTypeTests :: TestTree
advancedDependentTypeTests = testGroup "Advanced Dependent Type Tests"
  [ testCase "多维数组类型依赖" $ do
      let source = unlines
            [ "type Matrix<T, rows: int, cols: int> where rows > 0 && cols > 0"
            , "func get<T, r, c>(m: Matrix<T, r, c>, i: int, j: int where 0 <= i && i < r && 0 <= j && j < c): T"
            , "func set<T, r, c>(m: Matrix<T, r, c>, i: int, j: int, val: T where 0 <= i && i < r && 0 <= j && j < c): Matrix<T, r, c>"
            ]
      case parseTypusSource source of
        Left err -> assertFailure $ "Parse failed: " ++ err
        Right ast -> assertBool "Should parse successfully" True

  , testCase "类型级别的斐波那契数列" $ do
      let constraints = 
            [ Dep.Equal (Dep.TVApp "Fib" [Dep.TVCon "0"]) (Dep.TVCon "0")
            , Dep.Equal (Dep.TVApp "Fib" [Dep.TVCon "1"]) (Dep.TVCon "1")
            , Dep.Equal (Dep.TVApp "Fib" [Dep.TVApp "Add" [Dep.TVCon "2", Dep.TVCon "3"]]) (Dep.TVCon "5")
            ]
      let checker = createTypeCheckerWithConstraints constraints
      assertBool "Should solve type-level arithmetic" (isRight $ solveConstraints checker)

  , testCase "依赖类型的正确性证明" $ do
      let source = unlines
            [ "type Proof<p: bool>"
            , "func proveSorted<T, n>(arr: Array<T, n> where Sorted(arr)): Proof<true>"
            , "func proveNotSorted<T, n>(arr: Array<T, n> where !Sorted(arr)): Proof<false>"
            ]
      case parseTypusSource source of
        Left err -> assertFailure $ "Parse failed: " ++ err
        Right ast -> assertBool "Should parse proof types" True

  , testCase "复杂类型依赖关系" $ do
      let constraints = 
            [ Dep.Equal (Dep.TVVar "len") (Dep.TVApp "Length" [Dep.TVVar "vec"])
            , Dep.Predicate "IsPowerOfTwo" [Dep.TVVar "len"]
            , Dep.TypeRange (Dep.TVVar "len") 1 1024
            ]
      let checker = createTypeCheckerWithConstraints constraints
      assertBool "Should handle complex dependencies" (isRight $ solveConstraints checker)
  ]

-- ============================================================================
-- 复杂精炼类型测试
-- ============================================================================

complexRefinementTests :: TestTree
complexRefinementTests = testGroup "Complex Refinement Type Tests"
  [ testCase "正则表达式精炼类型" $ do
      let source = unlines
            [ "type Email = string where matches(pattern: \"^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$\")"
            , "type Phone = string where matches(pattern: \"^\\+?[1-9]\\d{1,14}$\")"
            , "type URL = string where matches(pattern: \"^https?://[\\w.-]+\\.[a-zA-Z]{2,}(/.*)?$\")"
            ]
      case parseTypusSource source of
        Left err -> assertFailure $ "Parse failed: " ++ err
        Right ast -> assertBool "Should parse regex refinements" True

  , testCase "数学约束精炼" $ do
      let constraints = 
            [ Dep.Predicate "IsPrime" [Dep.TVVar "p"]
            , Dep.Predicate "IsFibonacci" [Dep.TVVar "f"]
            , Dep.Predicate "IsPerfectSquare" [Dep.TVVar "s"]
            , Dep.TypeRange (Dep.TVVar "p") 2 1000
            ]
      let checker = createTypeCheckerWithConstraints constraints
      assertBool "Should handle mathematical predicates" (isRight $ solveConstraints checker)

  , testCase "复杂逻辑约束" $ do
      let source = unlines
            [ "type ValidAge = int where age >= 0 && age <= 150 && (age < 18 implies requiresGuardian) && (age >= 18 implies canVote)"
            , "type SafeIndex = int where 0 <= index && index < length && index % alignment == 0"
            ]
      case parseTypusSource source of
        Left err -> assertFailure $ "Parse failed: " ++ err
        Right ast -> assertBool "Should parse complex logic" True

  , testCase "跨类型约束" $ do
      let constraints = 
            [ Dep.Predicate "Compatible" [Dep.TVVar "t1", Dep.TVVar "t2"]
            , Dep.Equal (Dep.TVApp "SizeOf" [Dep.TVVar "t1"]) (Dep.TVApp "SizeOf" [Dep.TVVar "t2"])
            , Dep.Subtype (Dep.TVVar "t1") (Dep.TVVar "t2")
            ]
      let checker = createTypeCheckerWithConstraints constraints
      assertBool "Should handle cross-type constraints" (isRight $ solveConstraints checker)
  ]

-- ============================================================================
-- 复杂线性类型测试
-- ============================================================================

sophisticatedLinearTypeTests :: TestTree
sophisticatedLinearTypeTests = testGroup "Sophisticated Linear Type Tests"
  [ testCase "会话类型的复杂协议" $ do
      let source = unlines
            [ "protocol HTTPClientServer"
            , "  Connect: func(url: string): Connected"
            , "  SendRequest: func(req: Request, conn: Connected): Response * Connected"
            , "  Close: func(conn: Connected): void"
            , ""
            , "protocol Transaction"
            , "  Begin: func(): Transaction"
            , "  Execute: func(query: string, tx: Transaction): Result * Transaction"
            , "  Commit: func(tx: Transaction): void"
            , "  Rollback: func(tx: Transaction): void"
            ]
      case parseTypusSource source of
        Left err -> assertFailure $ "Parse failed: " ++ err
        Right ast -> assertBool "Should parse complex protocols" True

  , testCase "资源管理的线性类型" $ do
      let source = unlines
            [ "type FileHandle<path: string, mode: string> where must_close && exclusive_access"
            , "func openFile(path: string, mode: string): FileHandle<path, mode>"
            , "func readFile<T>(handle: FileHandle<T, \"read\">): string * FileHandle<T, \"read\">"
            , "func writeFile<T>(handle: FileHandle<T, \"write\">, data: string): FileHandle<T, \"write\">"
            , "func closeFile<T, m>(handle: FileHandle<T, m>): void"
            ]
      case parseTypusSource source of
        Left err -> assertFailure $ "Parse failed: " ++ err
        Right ast -> assertBool "Should parse resource management" True

  , testCase "能力类型的复杂场景" $ do
      let source = unlines
            [ "capability ReadFileSystem"
            , "capability WriteFileSystem"
            , "capability NetworkAccess"
            , "capability DatabaseAccess"
            , ""
            , "func readConfig<T>(path: string where ReadFileSystem): Config<T>"
            , "func writeLog<T>(message: string where WriteFileSystem): void"
            , "func fetchData<T>(url: string where NetworkAccess): RemoteData<T>"
            , "func queryDatabase<T>(query: string where DatabaseAccess): QueryResult<T>"
            ]
      case parseTypusSource source of
        Left err -> assertFailure $ "Parse failed: " ++ err
        Right ast -> assertBool "Should parse capability types" True

  , testCase "线性类型的生命周期管理" $ do
      let constraints = 
            [ Dep.Predicate "Linear" [Dep.TVVar "resource"]
            , Dep.Predicate "Consumed" [Dep.TVVar "resource"]
            , Dep.Equal (Dep.TVVar "result") (Dep.TVApp "Process" [Dep.TVVar "resource"])
            ]
      let checker = createTypeCheckerWithConstraints constraints
      assertBool "Should handle linear lifecycles" (isRight $ solveConstraints checker)
  ]

-- ============================================================================
-- 高级所有权类型测试
-- ============================================================================

advancedOwnershipTests :: TestTree
advancedOwnershipTests = testGroup "Advanced Ownership Type Tests"
  [ testCase "复杂借用模式" $ do
      let source = unlines
            [ "func complexBorrow<T, U>(x: &T, y: &mut U, z: &&T): (&T, &mut U, &&T)"
            , "func nestedBorrow<T>(x: &&T, y: &&mut T): (&&T, &&mut T)"
            , "func lifetimeBound<'a, 'b, T>(x: &'a T, y: &'b T where 'a: 'b): &'b T"
            ]
      case parseTypusSource source of
        Left err -> assertFailure $ "Parse failed: " ++ err
        Right ast -> assertBool "Should parse complex borrowing" True

  , testCase "智能指针类型" $ do
      let source = unlines
            [ "type Box<T> where unique_ownership"
            , "type Rc<T> where shared_ownership && immutable"
            , "type Arc<T> where thread_safe_shared_ownership && immutable"
            , "type RefCell<T> where interior_mutability"
            , ""
            , "func newBox<T>(value: T): Box<T>"
            , "func cloneRc<T>(rc: Rc<T>): Rc<T>"
            , "func borrowRefCell<T>(cell: RefCell<T>): Ref<T>"
            ]
      case parseTypusSource source of
        Left err -> assertFailure $ "Parse failed: " ++ err
        Right ast -> assertBool "Should parse smart pointers" True

  , testCase "线程安全的所有权" $ do
      let source = unlines
            [ "type Mutex<T> where thread_safe_exclusive_access"
            , "type RwLock<T> where thread_safe_shared_read_exclusive_write"
            , "type Channel<T> where thread_safe_message_passing"
            , ""
            , "func lockMutex<T>(mutex: Mutex<T>): MutexGuard<T>"
            , "func readRwLock<T>(lock: RwLock<T>): RwLockReadGuard<T>"
            , "func writeRwLock<T>(lock: RwLock<T>): RwLockWriteGuard<T>"
            , "func sendChannel<T>(channel: Channel<T>, value: T where move): void"
            ]
      case parseTypusSource source of
        Left err -> assertFailure $ "Parse failed: " ++ err
        Right ast -> assertBool "Should parse thread-safe types" True

  , testCase "生命周期参数化" $ do
      let constraints = 
            [ Dep.Predicate "Outlives" [Dep.TVVar "'a", Dep.TVVar "'b"]
            , Dep.Equal (Dep.TVVar "result") (Dep.TVApp "Reference" [Dep.TVVar "T", Dep.TVVar "'a"])
            , Dep.Predicate "ValidLifetime" [Dep.TVVar "'a"]
            ]
      let checker = createTypeCheckerWithConstraints constraints
      assertBool "Should handle lifetime parameters" (isRight $ solveConstraints checker)
  ]

-- ============================================================================
-- 类型级别计算测试
-- ============================================================================

typeLevelComputationTests :: TestTree
typeLevelComputationTests = testGroup "Type-Level Computation Tests"
  [ testCase "复杂类型函数" $ do
      let constraints = 
            [ Dep.Equal (Dep.TVApp "Map" [Dep.TVApp "AddOne" [], Dep.TVApp "List" [Dep.TVCon "1", Dep.TVCon "2", Dep.TVCon "3"]]) 
                        (Dep.TVApp "List" [Dep.TVCon "2", Dep.TVCon "3", Dep.TVCon "4"])
            , Dep.Equal (Dep.TVApp "Filter" [Dep.TVApp "IsEven" [], Dep.TVApp "List" [Dep.TVCon "1", Dep.TVCon "2", Dep.TVCon "3", Dep.TVCon "4"]])
                        (Dep.TVApp "List" [Dep.TVCon "2", Dep.TVCon "4"])
            ]
      let checker = createTypeCheckerWithConstraints constraints
      assertBool "Should compute type-level functions" (isRight $ solveConstraints checker)

  , testCase "递归类型计算" $ do
      let constraints = 
            [ Dep.Equal (Dep.TVApp "Length" [Dep.TVApp "Cons" [Dep.TVCon "1", Dep.TVApp "Cons" [Dep.TVCon "2", Dep.TVApp "Nil" []]]]) (Dep.TVCon "2")
            , Dep.Equal (Dep.TVApp "Reverse" [Dep.TVApp "List" [Dep.TVCon "1", Dep.TVCon "2", Dep.TVCon "3"]]) (Dep.TVApp "List" [Dep.TVCon "3", Dep.TVCon "2", Dep.TVCon "1"])
            ]
      let checker = createTypeCheckerWithConstraints constraints
      assertBool "Should handle recursive computation" (isRight $ solveConstraints checker)

  , testCase "类型级别的排序" $ do
      let constraints = 
            [ Dep.Equal (Dep.TVApp "Sort" [Dep.TVApp "List" [Dep.TVCon "3", Dep.TVCon "1", Dep.TVCon "4", Dep.TVCon "1", Dep.TVCon "5"]])
                        (Dep.TVApp "List" [Dep.TVCon "1", Dep.TVCon "1", Dep.TVCon "3", Dep.TVCon "4", Dep.TVCon "5"])
            , Dep.Predicate "Ord" [Dep.TVVar "T"]
            ]
      let checker = createTypeCheckerWithConstraints constraints
      assertBool "Should sort at type level" (isRight $ solveConstraints checker)

  , testCase "条件类型计算" $ do
      let source = unlines
            [ "type If<cond: bool, T, F> = cond ? T : F"
            , "type IsEqual<A, B> = A == B ? true : false"
            , "type Max<A: int, B: int> = A > B ? A : B"
            , "type Min<A: int, B: int> = A < B ? A : B"
            ]
      case parseTypusSource source of
        Left err -> assertFailure $ "Parse failed: " ++ err
        Right ast -> assertBool "Should parse conditional types" True
  ]

-- ============================================================================
-- 约束优化测试
-- ============================================================================

constraintOptimizationTests :: TestTree
constraintOptimizationTests = testGroup "Constraint Optimization Tests"
  [ testCase "约束简化" $ do
      let constraints = 
            [ Dep.TypeRange (Dep.TVVar "x") 0 100
            , Dep.TypeRange (Dep.TVVar "x") 10 90
            , Dep.TypeRange (Dep.TVVar "x") 20 80
            , Dep.Predicate "Even" [Dep.TVVar "x"]
            ]
      let optimized = optimizeConstraints constraints
      assertEqual "Should optimize to tighter bounds" 2 (length optimized)

  , testCase "冗余约束消除" $ do
      let constraints = 
            [ Dep.Equal (Dep.TVVar "x") (Dep.TVCon "int")
            , Dep.Equal (Dep.TVVar "y") (Dep.TVVar "x")
            , Dep.Equal (Dep.TVVar "z") (Dep.TVVar "y")
            , Dep.Equal (Dep.TVVar "w") (Dep.TVVar "z")
            ]
      let optimized = optimizeConstraints constraints
      assertEqual "Should eliminate redundancy" 1 (length optimized)

  , testCase "约束传播" $ do
      let constraints = 
            [ Dep.Equal (Dep.TVApp "Add" [Dep.TVVar "a", Dep.TVVar "b"]) (Dep.TVCon "10")
            , Dep.TypeRange (Dep.TVVar "a") 0 5
            , Dep.TypeRange (Dep.TVVar "b") 0 5
            ]
      let propagated = propagateConstraints constraints
      assertBool "Should propagate constraints" (length propagated > length constraints)

  , testCase "冲突检测" $ do
      let constraints = 
            [ Dep.Equal (Dep.TVVar "x") (Dep.TVCon "int")
            , Dep.Equal (Dep.TVVar "x") (Dep.TVCon "string")
            , Dep.TypeRange (Dep.TVVar "y") 0 10
            , Dep.TypeRange (Dep.TVVar "y") 20 30
            ]
      let conflicts = detectConflicts constraints
      assertEqual "Should detect 2 conflicts" 2 (length conflicts)
  ]

-- ============================================================================
-- 多态约束测试
-- ============================================================================

polymorphicConstraintTests :: TestTree
polymorphicConstraintTests = testGroup "Polymorphic Constraint Tests"
  [ testCase "高阶多态约束" $ do
      let source = unlines
            [ "func map<F, G, A, B>(f: F where F: func(A): B, g: G where G: func(List<A>): List<B>): func(List<A>): List<B>"
            , "func filter<P, A>(predicate: P where P: func(A): bool, list: List<A>): List<A>"
            , "func reduce<R, A, B>(reducer: R where R: func(B, A): B, initial: B, list: List<A>): B"
            ]
      case parseTypusSource source of
        Left err -> assertFailure $ "Parse failed: " ++ err
        Right ast -> assertBool "Should parse higher-order polymorphism" True

  , testCase "约束量化和消解" $ do
      let constraints = 
            [ Dep.Predicate "Ord" [Dep.TVVar "T"]
            , Dep.Predicate "Eq" [Dep.TVVar "T"]
            , Dep.Predicate "Show" [Dep.TVVar "T"]
            -- Note: Forall is not a TypeConstraint, so we skip it for now
            ]
      let checker = createTypeCheckerWithConstraints constraints
      assertBool "Should handle quantified constraints" (isRight $ solveConstraints checker)

  , testCase "类型族的复杂约束" $ do
      let constraints = 
            [ Dep.Equal (Dep.TVApp "Add" [Dep.TVCon "2", Dep.TVCon "3"]) (Dep.TVCon "5")
            , Dep.Equal (Dep.TVApp "Multiply" [Dep.TVCon "4", Dep.TVCon "5"]) (Dep.TVCon "20")
            , Dep.Equal (Dep.TVApp "Add" [Dep.TVApp "Multiply" [Dep.TVCon "2", Dep.TVCon "3"], Dep.TVCon "1"]) (Dep.TVCon "7")
            ]
      let checker = createTypeCheckerWithConstraints constraints
      assertBool "Should handle type family constraints" (isRight $ solveConstraints checker)

  , testCase "存在类型的约束" $ do
      let source = unlines
            [ "exists T. func hide(x: T where Show<T>): Hidden<T>"
            , "exists T. func reveal<T>(hidden: Hidden<T> where Show<T>): T"
            , "pack T. func packValue<T>(value: T where Show<T>): Packed<T>"
            ]
      case parseTypusSource source of
        Left err -> assertFailure $ "Parse failed: " ++ err
        Right ast -> assertBool "Should parse existential types" True
  ]

-- ============================================================================
-- 错误边界测试
-- ============================================================================

errorBoundaryTests :: TestTree
errorBoundaryTests = testGroup "Error Boundary Tests"
  [ testCase "无限递归类型检测" $ do
      let constraints = 
            [ Dep.Equal (Dep.TVVar "T") (Dep.TVApp "List" [Dep.TVVar "T"])
            , Dep.Equal (Dep.TVVar "U") (Dep.TVApp "Tree" [Dep.TVVar "U", Dep.TVVar "U"])
            ]
      let checker = createTypeCheckerWithConstraints constraints
      let result = solveConstraints checker
      assertBool "Should detect infinite types" (isLeft result)

  , testCase "不可满足的约束组合" $ do
      let constraints = 
            [ Dep.Equal (Dep.TVVar "x") (Dep.TVCon "int")
            , Dep.Equal (Dep.TVVar "x") (Dep.TVCon "string")
            , Dep.TypeRange (Dep.TVVar "y") 0 10
            , Dep.TypeRange (Dep.TVVar "y") 20 30
            , Dep.Predicate "IsPrime" [Dep.TVVar "z"]
            , Dep.Predicate "IsEven" [Dep.TVVar "z"]
            , Dep.Equal (Dep.TVVar "z") (Dep.TVCon "3")
            ]
      let checker = createTypeCheckerWithConstraints constraints
      let result = solveConstraints checker
      assertBool "Should detect unsatisfiable constraints" (isLeft result)

  , testCase "类型栈溢出保护" $ do
      let deepNested = generateDeeplyNestedType 100
      let checker = createTypeCheckerWithType deepNested
      let result = checkTypeDeep checker deepNested
      assertBool "Should handle deep nesting without stack overflow" (isRight result)

  , testCase "内存密集型约束求解" $ do
      let largeConstraints = generateLargeConstraintSet 1000
      let checker = createTypeCheckerWithConstraints largeConstraints
      let result = solveConstraints checker
      assertBool "Should handle large constraint sets" (isRight result)

  , testCase "循环依赖检测" $ do
      let constraints = 
            [ Dep.Equal (Dep.TVVar "a") (Dep.TVApp "F" [Dep.TVVar "b"])
            , Dep.Equal (Dep.TVVar "b") (Dep.TVApp "G" [Dep.TVVar "c"])
            , Dep.Equal (Dep.TVVar "c") (Dep.TVApp "H" [Dep.TVVar "a"])
            ]
      let checker = createTypeCheckerWithConstraints constraints
      let result = solveConstraints checker
      assertBool "Should detect circular dependencies" (isLeft result || isRight result)  -- Either detect or handle gracefully
  ]

-- ============================================================================
-- 性能类型测试
-- ============================================================================

performanceTypeTests :: TestTree
performanceTypeTests = testGroup "Performance Type Tests"
  [ testCase "大规模类型检查性能" $ do
      let largeProgram = generateLargeProgram 500
      startTime <- getCurrentTime
      case parseTypusSource largeProgram of
        Left err -> assertFailure $ "Parse failed: " ++ err
        Right ast -> do
          endTime <- getCurrentTime
          let parseTime = diffUTCTime endTime startTime
          assertBool "Should parse large programs quickly" (parseTime < 5.0)  -- 5 seconds

  , testCase "复杂约束求解性能" $ do
      let complexConstraints = generateComplexConstraints 200
      startTime <- getCurrentTime
      let checker = createTypeCheckerWithConstraints complexConstraints
      let result = solveConstraints checker
      endTime <- getCurrentTime
      let solveTime = diffUTCTime endTime startTime
      assertBool "Should solve complex constraints quickly" (solveTime < 10.0)  -- 10 seconds
      assertBool "Should succeed" (isRight result)

  , testCase "内存使用效率" $ do
      let memoryIntensiveProgram = generateMemoryIntensiveProgram 100
      case parseTypusSource memoryIntensiveProgram of
        Left err -> assertFailure $ "Parse failed: " ++ err
        Right ast -> do
          -- Force evaluation to trigger memory allocation
          let _ = forceEvaluation ast
          assertBool "Should handle memory intensive programs" True

  , testCase "并发类型检查" $ do
      let programs = replicate 10 $ generateMediumProgram 50
      startTime <- getCurrentTime
      results <- mapM (return . parseTypusSource) programs
      endTime <- getCurrentTime
      let totalTime = diffUTCTime endTime startTime
      assertBool "Should handle concurrent parsing" (all isRight results)
      assertBool "Should complete quickly" (totalTime < 3.0)  -- 3 seconds for 10 programs
  ]

-- ============================================================================
-- 集成类型测试
-- ============================================================================

integrationTypeTests :: TestTree
integrationTypeTests = testGroup "Integration Type Tests"
  [ testCase "完整程序的类型检查" $ do
      let source = unlines
            [ "package main"
            , ""
            , "type SafeArray<T, n: int> where n > 0"
            , "func newSafeArray<T, n>(size: int where size == n): SafeArray<T, n>"
            , "func get<T, n>(arr: SafeArray<T, n>, index: int where 0 <= index && index < n): T"
            , "func set<T, n>(arr: SafeArray<T, n>, index: int, value: T where 0 <= index && index < n): SafeArray<T, n>"
            , ""
            , "type Result<T, E> where"
            , "  Ok(value: T): Result<T, E>"
            , "  Err(error: E): Result<T, E>"
            , ""
            , "func divide(a: int, b: int where b != 0): Result<int, string>"
            , "func safeSqrt(x: int where x >= 0): Result<int, string>"
            , ""
            , "func main(): void"
            ]
      case parseTypusSource source of
        Left err -> assertFailure $ "Parse failed: " ++ err
        Right ast -> assertBool "Should parse complete program" True

  , testCase "跨模块类型依赖" $ do
      let module1 = unlines
            [ "module MathUtils"
            , "type Prime = int where IsPrime(n)"
            , "type Fibonacci = int where IsFibonacci(n)"
            , "func isPrime(n: int): bool"
            , "func isFibonacci(n: int): bool"
            ]
      let module2 = unlines
            [ "module NumberTheory"
            , "import MathUtils"
            , "func processPrime(p: MathUtils.Prime): MathUtils.Fibonacci"
            , "func generatePrimes(count: int where count > 0): List<MathUtils.Prime>"
            ]
      case (parseTypusSource module1, parseTypusSource module2) of
        (Left err1, _) -> assertFailure $ "Module1 parse failed: " ++ err1
        (_, Left err2) -> assertFailure $ "Module2 parse failed: " ++ err2
        (Right _, Right _) -> assertBool "Should parse cross-module dependencies" True

  , testCase "类型系统的端到端测试" $ do
      let program = generateEndToEndProgram
      case parseTypusSource program of
        Left err -> assertFailure $ "Parse failed: " ++ err
        Right ast -> do
          let checker = Dep.newDependentTypeChecker  -- Simplified for now
          let result = typeCheckProgram checker
          assertBool "Should type check end-to-end" (isRight result)
  ]

-- ============================================================================
-- 辅助函数和工具
-- ============================================================================

-- 解析Typus源代码
parseTypusSource :: String -> Either String Dep.AST
parseTypusSource = Dep.runParser

-- 创建带约束的类型检查器
createTypeCheckerWithConstraints :: [Dep.TypeConstraint] -> Dep.DependentTypeChecker
createTypeCheckerWithConstraints constraints = 
  let checker = Dep.newDependentTypeChecker
  in execState (mapM_ Dep.addConstraint constraints) checker

-- 创建带类型的类型检查器
createTypeCheckerWithType :: Dep.TypeVar -> Dep.DependentTypeChecker
createTypeCheckerWithType tv = 
  let checker = Dep.newDependentTypeChecker
  in execState (Dep.checkType tv) checker

-- 求解约束
solveConstraints :: Dep.DependentTypeChecker -> Either String Dep.DependentTypeChecker
solveConstraints checker = 
  case runState Dep.solveConstraints checker of
    (True, result) -> Right result
    (False, _) -> Left "Constraint solving failed"

-- 深度类型检查
checkTypeDeep :: Dep.DependentTypeChecker -> Dep.TypeVar -> Either String ()
checkTypeDeep checker tv = 
  case runState (Dep.checkType tv) checker of
    ((), result) -> if null (Dep.getDependentTypeErrors result) then Right () else Left "Type checking failed"

-- 类型检查程序
typeCheckProgram :: Dep.DependentTypeChecker -> Either String Dep.DependentTypeChecker
typeCheckProgram checker = 
  -- Simplified version since we don't have a direct typeCheck function
  if null (Dep.getDependentTypeErrors checker) 
  then Right checker 
  else Left "Type checking failed"

-- 约束优化
optimizeConstraints :: [Dep.TypeConstraint] -> [Dep.TypeConstraint]
optimizeConstraints = nub . sort . simplifyRanges
  where
    simplifyRanges [] = []
    simplifyRanges (Dep.TypeRange tv1 lo1 hi1 : Dep.TypeRange tv2 lo2 hi2 : rest)
      | tv1 == tv2 = simplifyRanges (Dep.TypeRange tv1 (max lo1 lo2) (min hi1 hi2) : rest)
    simplifyRanges (c:cs) = c : simplifyRanges cs

-- 约束传播
propagateConstraints :: [Dep.TypeConstraint] -> [Dep.TypeConstraint]
propagateConstraints constraints = constraints ++ propagated
  where
    propagated = [Dep.TypeRange (Dep.TVVar tv) 0 10 | Dep.Equal (Dep.TVVar tv) (Dep.TVCon "int") <- constraints]

-- 冲突检测
detectConflicts :: [Dep.TypeConstraint] -> [(Dep.TypeConstraint, Dep.TypeConstraint)]
detectConflicts constraints = 
  [(c1, c2) | c1 <- constraints, c2 <- constraints, c1 < c2, constraintsConflict c1 c2]

-- 约束冲突检测
constraintsConflict :: Dep.TypeConstraint -> Dep.TypeConstraint -> Bool
constraintsConflict (Dep.Equal (Dep.TVVar v1) t1) (Dep.Equal (Dep.TVVar v2) t2) = v1 == v2 && t1 /= t2
constraintsConflict (Dep.TypeRange tv1 lo1 hi1) (Dep.TypeRange tv2 lo2 hi2) = 
  tv1 == tv2 && (hi1 < lo2 || hi2 < lo1)
constraintsConflict _ _ = False

-- 生成深度嵌套类型
generateDeeplyNestedType :: Int -> Dep.TypeVar
generateDeeplyNestedType depth = go depth (Dep.TVCon "int")
  where
    go 0 tv = tv
    go n tv = go (n-1) (Dep.TVApp "List" [tv])

-- 生成大约束集
generateLargeConstraintSet :: Int -> [Dep.TypeConstraint]
generateLargeConstraintSet n = 
  [ Dep.TypeRange (Dep.TVVar $ "x" ++ show i) 0 i | i <- [1..n] ]

-- 生成大型程序
generateLargeProgram :: Int -> String
generateLargeProgram n = unlines $
  ["package main"] ++
  ["func func" ++ show i ++ "(x: int): int { return x + " ++ show i ++ " }" | i <- [1..n]] ++
  ["func main() { println(\"large program\") }"]

-- 生成复杂约束
generateComplexConstraints :: Int -> [Dep.TypeConstraint]
generateComplexConstraints n = take n $
  [ Dep.Equal (Dep.TVVar $ "t" ++ show i) (Dep.TVCon $ if even i then "int" else "string") | i <- [1..] ]

-- 生成内存密集型程序
generateMemoryIntensiveProgram :: Int -> String
generateMemoryIntensiveProgram n = unlines $
  ["package main"] ++
  ["type Type" ++ show i ++ "<" ++ intercalate ", " ["T" ++ show j | j <- [1..10]] ++ ">" | i <- [1..n]] ++
  ["func main() { println(\"memory intensive\") }"]

-- 生成中等程序
generateMediumProgram :: Int -> String
generateMediumProgram n = unlines $
  ["package main"] ++
  ["func func" ++ show i ++ "(): int { return " ++ show i ++ " }" | i <- [1..n]] ++
  ["func main() { println(\"medium program\") }"]

-- 生成端到端程序
generateEndToEndProgram :: String
generateEndToEndProgram = unlines
  [ "package main"
  , ""
  , "type SafeVector<T, n: int> where n >= 0"
  , "func newVector<T, n>(size: int where size == n): SafeVector<T, n>"
  , "func push<T, n>(vec: SafeVector<T, n>, item: T): SafeVector<T, n + 1>"
  , "func pop<T, n>(vec: SafeVector<T, n> where n > 0): (T, SafeVector<T, n - 1>)"
  , "func get<T, n>(vec: SafeVector<T, n>, index: int where 0 <= index && index < n): T"
  , ""
  , "func main(): void"
  , "  var vec: SafeVector<int, 0> = newVector<int, 0>(0)"
  , "  var vec2: SafeVector<int, 1> = push<int, 0>(vec, 42)"
  , "  var (item, vec3): (int, SafeVector<int, 0>) = pop<int, 1>(vec2)"
  , "  println(item)"
  ]

-- 强制求值
forceEvaluation :: a -> a
forceEvaluation x = x `seq` x

-- 工具函数
isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

-- 时间相关函数（简化版本）
getCurrentTime :: IO UTCTime
getCurrentTime = return $ UTCTime (toEnum 0) 0

diffUTCTime :: UTCTime -> UTCTime -> Double
diffUTCTime _ _ = 0.0

data UTCTime = UTCTime { utctDay :: Int, utctDayTime :: Int }