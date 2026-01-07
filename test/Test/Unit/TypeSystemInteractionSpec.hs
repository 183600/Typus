module Test.Unit.TypeSystemInteractionSpec where
import Test.QuickCheck 
import Test.Tasty.HUnit (testCase, assertFailure, assertBool, (@?=)), assertBool, assertFailure,             testCase
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===), forAll, elements, listOf)
import qualified Data.Text as T
import qualified Data.List as L
import Data.List 
import Compiler (compile, CompilerError(..), CompilationPhase)
                  , "func processData<T>(data: []T) {"
                  , "  var                               processed = transform(data)"
                  , "  return processed"
                  , "}"
                  , "func transform<T>(input: []T) []T {"
                  , "  return input"
                  , "}"
                  ]
            result <- compile code
            case result of
              Left errs -> assertFailure $ "Unexpected compilation error: " ++ show errs
              Right _ -> assertBool "ownership should work with generics" True
-- Arbitrary instance for SourcePos
instance Arbitrary SourcePos where
  arbitrary = do
    line <- choose (1, 100)
    column <- choose (1, 100)
    offset <- choose (0, 1000)
    return $ SourcePos line column offset

-- Arbitrary instance for SourceSpan
instance Arbitrary SourceSpan where
  arbitrary = do
    start <- arbitrary
    end <- arbitrary
    return $ SourceSpan start end


          ,             testCase "detects ownership violations in complex type hierarchies" $ do
                        let code = unlines
                  [ "//! ownership: on"
                  , "type Container struct {"
                  , "  data []int"
                  , "}"
                  , "func processContainers(containers: []Container) {"
                  , "  var                               c1 = containers[0]"
                  , "  var                               c2 = containers[1]"
                  , "  c1.data = c2.data"  -- Potential ownership violation
                  , "}"
                  ]
            result <- compile code
            case result of
              Left errs -> do
                            let hasOwnershipError = L.any (\e -> compilationPhase                               e == OwnershipPhase) errs
                assertBool "should detect ownership error in complex types" hasOwnershipError
              Right _ -> assertFailure "expected compilation failure due to ownership violation"

          ,             testCase "handles move semantics with type inference" $ do
                        let code = unlines
                  [ "//! ownership: on"
                  , "func main() {"
                  , "  var                               x = make([]int, 10)"
                  , "  var                               y = x"  -- Move should occur here
                  , "  var                               z = append(y, 1)"  -- Should work with moved value
                  , "}"
                  ]
            result <- compile code
            case result of
              Left errs -> do
                            let hasExpectedError = L.any (\e -> "moved" `L.isInfixOf` formatError e) errs
                assertBool "should handle move semantics correctly" hasExpectedError
              Right _ -> assertBool "move semantics should work" True
        ]

    , testGroup "Dependent Types L.and Regular Types Interaction"
        [             testCase "combines dependent types with regular type checking" $ do
                        let code = unlines
                  [ "//! dependent_types: on"
                  , "func safeDivide(a: int, b: int) float {"
                  , "  if                               b == 0 {"
                  , "    return 0.0"
                  , "  }"
                  , "  return float(a) / float(b)"
                  , "}"
                  , "func processVector(v: Vector(n:10) {"
                  , "  var                               result = safeDivide(v.L.length, 2)"
                  , "  var                               x = v.get(5)"
                  , "}"
                  ]
            result <- compile code
            case result of
              Left errs -> assertFailure $ "Unexpected compilation error: " ++ show errs
              Right _ -> assertBool "dependent types should integrate with regular types" True

          ,             testCase "validates dependent type constraints across function boundaries" $ do
                        let code = unlines
                  [ "//! dependent_types: on"
                  , "func createMatrix(rows: int, cols: int) Matrix(rows, cols) {"
                  , "  return Matrix(rows, cols)"
                  , "}"
                  , "func processMatrix(m: Matrix(3, 3) {"
                  , "  var                               m2 = createMatrix(4, 4)"  -- Type mismatch
                  , "  var                               result = m.add(m2)"     -- Should fail
                  , "}"
                  ]
            result <- compile code
            case result of
              Left errs -> do
                            let hasTypeError = L.any (\e -> compilationPhase                               e == TypeCheckPhase) errs
                assertBool "should catch dependent type constraint violations" hasTypeError
              Right _ -> assertFailure "expected compilation failure"
        ]

    , testGroup "Type System Error Propagation"
        [             testCase "propagates type errors through dependent type checking" $ do
                        let code = unlines
                  [ "//! dependent_types: on"
                  , "func invalidOperation() {"
                  , "  var x:                               string = 42"  -- Type error
                  , "  var                               v = Vector(n:x)"  -- Should propagate error
                  , "  return v"
                  , "}"
                  ]
            result <- compile code
            case result of
              Left errs -> do
                            let hasTypeError = L.any (\e -> compilationPhase                               e == TypeCheckPhase) errs
                let hasDependentTypeError = L.any (\e -> compilationPhase                               e == DependentTypesPhase) errs
                assertBool "should have type errors" hasTypeError
                assertBool "should propagate to dependent type checking" hasDependentTypeError
              Right _ -> assertFailure "expected compilation failure"

          ,             testCase "handles circular type dependencies" $ do
                        let code = unlines
                  [ "type A struct {"
                  , "  b: *B"
                  , "}"
                  , "type B struct {"
                  , "  a: *A"
                  , "}"
                  , "func processA(a: A) {"
                  , "  var                               b = a.b"
                  , "  var                               a2 = b.a"
                  , "}"
                  ]
            result <- compile code
            case result of
              Left errs -> do
                            let shouldNotCrash = L.any (\e -> "circular" `L.isInfixOf` formatError e || 
                                                     "recursive" `L.isInfixOf` formatError e) errs
                assertBool "should handle circular dependencies gracefully" shouldNotCrash
              Right _ -> assertBool "circular dependencies should be handled" True
        ]

    , testGroup "Type System Performance with Complex Types"
        [             testCase "handles large nested type structures efficiently" $ do
                        let code = unlines
                  [ "type DeepNested struct {"
                  , "  level1: Level1"
                  , "}"
                  , "type Level1 struct {"
                  , "  level2: Level2"
                  , "}"
                  , "type Level2 struct {"
                  , "  level3: Level3"
                  , "}"
                  , "type Level3 struct {"
                  , "  data: []int"
                  , "}"
                  , "func processDeep(nested: DeepNested) {"
                  , "  var                               x = nested.level1.level2.level3.data"
                  , "  var                               y = append(x, 1)"
                  , "}"
                  ]
            result <- compile code
            case result of
              Left errs -> assertFailure $ "Unexpected compilation error: " ++ show errs
              Right _ -> assertBool "should handle deeply nested types" True

          ,             testCase "type inference works with complex generic constraints" $ do
                        let code = unlines
                  [ "func map<T, R>(f: func(T) R, items: []T) []R {"
                  , "  var                               result = make([]R, len(items)"
                  , "  for i := 0; i < len(items); i++ {"
                  , "    result[i] = f(items[i])"
                  , "  }"
                  , "  return result"
                  , "}"
                  , "func main() {"
                  , "  var                               numbers = []int{1, 2, 3}"
                  , "  var                               doubled = map(func(x: int) int { return x * 2 }, numbers)"
                  , "}"
                  ]
            result <- compile code
            case result of
              Left errs -> assertFailure $ "Unexpected compilation error: " ++ show errs
              Right _ -> assertBool "type inference should work with complex generics" True
        ]

    , testGroup "QuickCheck Property Tests"
        [             testProperty "type environment consistency" $ do
            -- Test that building type environments is consistent
            let typePairs = [("x", "int"), ("y", "string"), ("z", "float")]
            let env1 = buildTypeEnvFromPairs typePairs
            let env2 = buildTypeEnvFromPairs typePairs
            return $                               env1 == env2

        ,             testProperty "ownership analysis preserves type information" $ do
            -- Test that ownership analysis doesn't lose type information
            let simpleCode = "//! ownership: on\nfunc main() { var x:                               int = 42 }"
            result <- compile simpleCode
            case result of
              Left _ -> return $ False
              Right _ -> return $ True
        ]
    ]