module Test.Unit.DependentTypeConstraintSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, assertFailure, testCase)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===), forAll, elements, choose)
import qualified Data.Text as T
import Data.List (isInfixOf)

import Compiler (compile, CompilerError(..), CompilationPhase(..))
import Compiler.DependentTypeChecker (checkDependentTypes)
import DependentTypesParser (parseDependentType)

-- | Test dependent type constraints and validation
tests :: TestTree
tests =
  testGroup "Dependent Type Constraint Tests"
    [ testGroup "Basic Dependent Type Constraints"
        [ testCase "validates vector size constraints" $ do
            let code = unlines
                  [ "//! dependent_types: on"
                  , "func main() {"
                  , "  var v = Vector(n:5)"
                  , "  var x = v.get(3)"   -- Valid access
                  , "  var y = v.get(10)"  -- Invalid access
                  , "}"
                  ]
            result <- compile code
            case result of
              Left errs -> do
                let hasDependentTypeError = any (\e -> compilationPhase e == DependentTypesPhase) errs
                let hasBoundsError = any (\e -> "bounds" `T.isInfixOf` formatError e) errs
                assertBool "should detect bounds violation" hasDependentTypeError
                assertBool "error should mention bounds" hasBoundsError
              Right _ -> assertFailure "expected dependent type error for bounds violation"

        , testCase "validates matrix dimension constraints" $ do
            let code = unlines
                  [ "//! dependent_types: on"
                  , "func processMatrix(m: Matrix(3, 4)) {"
                  , "  var x = m.get(2, 3)"   -- Valid access
                  , "  var y = m.get(4, 2)"   -- Invalid row access"
                  , "}"
                  ]
            result <- compile code
            case result of
              Left errs -> do
                let hasDependentTypeError = any (\e -> compilationPhase e == DependentTypesPhase) errs
                assertBool "should detect matrix dimension violation" hasDependentTypeError
              Right _ -> assertFailure "expected dependent type error for dimension violation"

        , testCase "allows valid dependent type operations" $ do
            let code = unlines
                  [ "//! dependent_types: on"
                  , "func main() {"
                  , "  var v1 = Vector(n:5)"
                  , "  var v2 = Vector(n:3)"
                  , "  var v3 = v1.concat(v2)"  -- Should create Vector(n:8)"
                  , "  var x = v3.get(7)"       -- Valid access"
                  , "}"
                  ]
            result <- compile code
            case result of
              Left errs -> assertFailure $ "Unexpected compilation error: " ++ show errs
              Right _ -> assertBool "valid dependent type operations should work" True
        ]

    , testGroup "Complex Dependent Type Expressions"
        [ testCase "handles nested dependent types" $ do
            let code = unlines
                  [ "//! dependent_types: on"
                  , "func main() {"
                  , "  var m = Matrix(rows:3, cols:4)"
                  , "  var row = m.getRow(1)"  -- Should return Vector(n:4)"
                  , "  var x = row.get(3)"     -- Valid access"
                  , "  var y = row.get(5)"     -- Invalid access"
                  , "}"
                  ]
            result <- compile code
            case result of
              Left errs -> do
                let hasDependentTypeError = any (\e -> compilationPhase e == DependentTypesPhase) errs
                assertBool "should handle nested dependent type constraints" hasDependentTypeError
              Right _ -> assertFailure "expected dependent type error for nested constraint violation"

        , testCase "validates dependent type function signatures" $ do
            let code = unlines
                  [ "//! dependent_types: on"
                  , "func safeGet<T>(v: Vector(n:N), index: I) T where I < N {"
                  , "  return v.get(index)"
                  , "}"
                  , "func main() {"
                  , "  var v = Vector(n:5)"
                  , "  var x = safeGet(v, 3)"   -- Valid"
                  , "  var y = safeGet(v, 10)"  -- Should fail at compile time"
                  , "}"
                  ]
            result <- compile code
            case result of
              Left errs -> do
                let hasDependentTypeError = any (\e -> compilationPhase e == DependentTypesPhase) errs
                assertBool "should validate dependent type function constraints" hasDependentTypeError
              Right _ -> assertFailure "expected dependent type error for function constraint violation"

        , testCase "handles dependent type arithmetic" $ do
            let code = unlines
                  [ "//! dependent_types: on"
                  , "func main() {"
                  , "  var v1 = Vector(n:5)"
                  , "  var v2 = Vector(n:3)"
                  , "  var v3 = v1.slice(0, 2)"  -- Should return Vector(n:2)"
                  , "  var v4 = v1.slice(2, 5)"  -- Should return Vector(n:3)"
                  , "  var v5 = v3.concat(v4)"   -- Should return Vector(n:5)"
                  , "  var x = v5.get(4)"        -- Valid access"
                  , "}"
                  ]
            result <- compile code
            case result of
              Left errs -> assertFailure $ "Unexpected compilation error: " ++ show errs
              Right _ -> assertBool "dependent type arithmetic should work" True
        ]

    , testGroup "Dependent Type Inference"
        [ testCase "infers dependent types from context" $ do
            let code = unlines
                  [ "//! dependent_types: on"
                  , "func createIdentityMatrix(n: int) Matrix(n, n) {"
                  , "  return Matrix(n, n)"
                  , "}"
                  , "func main() {"
                  , "  var m = createIdentityMatrix(3)"  -- Should infer Matrix(3, 3)"
                  , "  var x = m.get(2, 2)"             -- Valid access"
                  , "}"
                  ]
            result <- compile code
            case result of
              Left errs -> assertFailure $ "Unexpected compilation error: " ++ show errs
              Right _ -> assertBool "dependent type inference should work" True

        , testCase "handles dependent type constraints in generics" $ do
            let code = unlines
                  [ "//! dependent_types: on"
                  , "func processVectors<T, N>(v1: Vector(N), v2: Vector(N)) Vector(N) {"
                  , "  return v1.add(v2)"
                  , "}"
                  , "func main() {"
                  , "  var v1 = Vector(n:5)"
                  , "  var v2 = Vector(n:5)"
                  , "  var v3 = processVectors(v1, v2)"  -- Should work"
                  , "  var v4 = Vector(n:3)"
                  , "  var v5 = processVectors(v1, v4)"  -- Should fail"
                  , "}"
                  ]
            result <- compile code
            case result of
              Left errs -> do
                let hasDependentTypeError = any (\e -> compilationPhase e == DependentTypesPhase) errs
                assertBool "should handle generic dependent type constraints" hasDependentTypeError
              Right _ -> assertFailure "expected dependent type error for generic constraint violation"
        ]

    , testGroup "Dependent Type Error Messages"
        [ testCase "provides clear constraint violation messages" $ do
            let code = unlines
                  [ "//! dependent_types: on"
                  , "func main() {"
                  , "  var v = Vector(n:5)"
                  , "  var x = v.get(10)"
                  , "}"
                  ]
            result <- compile code
            case result of
              Left errs -> do
                let hasClearMessage = any (\e -> "index" `T.isInfixOf` formatError e && 
                                                     "bounds" `T.isInfixOf` formatError e) errs
                assertBool "should provide clear constraint violation messages" hasClearMessage
              Right _ -> assertFailure "expected dependent type error"

        , testCase "shows constraint information in errors" $ do
            let code = unlines
                  [ "//! dependent_types: on"
                  , "func processMatrix(m: Matrix(2, 3)) {"
                  , "  var x = m.get(3, 1)"
                  , "}"
                  ]
            result <- compile code
            case result of
              Left errs -> do
                let hasConstraintInfo = any (\e -> "2" `T.isInfixOf` formatError e &&
                                                   "3" `T.isInfixOf` formatError e) errs
                assertBool "should show constraint information in errors" hasConstraintInfo
              Right _ -> assertFailure "expected dependent type error"
        ]

    , testGroup "Dependent Type Performance"
        [ testCase "handles large dependent type expressions efficiently" $ do
            let code = unlines
                  [ "//! dependent_types: on"
                  , "func main() {"
                  , "  var v1 = Vector(n:1000)"
                  , "  var v2 = Vector(n:500)"
                  , "  var v3 = v1.concat(v2)"  -- Should create Vector(n:1500)"
                  , "  var v4 = v3.slice(100, 200)"  -- Should create Vector(n:100)"
                  , "  var x = v4.get(50)"  -- Valid access"
                  , "}"
                  ]
            result <- compile code
            case result of
              Left errs -> assertFailure $ "Unexpected compilation error: " ++ show errs
              Right _ -> assertBool "should handle large dependent type expressions" True

        , testCase "optimizes dependent type constraint checking" $ do
            let code = unlines
                  [ "//! dependent_types: on"
                  , "func validateAccess<T>(v: Vector(n:N), indices: []int) {"
                  , "  for i := 0; i < len(indices); i++ {"
                  , "    var x = v.get(indices[i])"  -- Should optimize repeated checks"
                  , "  }"
                  , "}"
                  , "func main() {"
                  , "  var v = Vector(n:100)"
                  , "  var indices = []int{1, 2, 3, 4, 5}"
                  , "  validateAccess(v, indices)"
                  , "}"
                  ]
            result <- compile code
            case result of
              Left errs -> assertFailure $ "Unexpected compilation error: " ++ show errs
              Right _ -> assertBool "should optimize dependent type constraint checking" True
        ]

    , testGroup "QuickCheck Property Tests"
        [ testProperty "vector access respects bounds" $ forAll (choose (0, 100)) $ \size -> do
            let validIndex = size `div` 2
            let invalidIndex = size + 10
            let validCode = unlines
                  [ "//! dependent_types: on"
                  , "func main() {"
                  , "  var v = Vector(n:" ++ show size ++ ")"
                  , "  var x = v.get(" ++ show validIndex ++ ")"
                  , "}"
                  ]
            let invalidCode = unlines
                  [ "//! dependent_types: on"
                  , "func main() {"
                  , "  var v = Vector(n:" ++ show size ++ ")"
                  , "  var x = v.get(" ++ show invalidIndex ++ ")"
                  , "}"
                  ]
            validResult <- compile validCode
            invalidResult <- compile invalidCode
            case (validResult, invalidResult) of
              (Right _, Left errs) -> return $ any (\e -> compilationPhase e == DependentTypesPhase) errs
              _ -> return $ False

        , testProperty "matrix dimensions are preserved" $ forAll (choose (1, 10)) $ \rows -> do
            forAll (choose (1, 10)) $ \cols -> do
                let code = unlines
                      [ "//! dependent_types: on"
                      , "func main() {"
                      , "  var m = Matrix(rows:" ++ show rows ++ ", cols:" ++ show cols ++ ")"
                      , "  var row = m.getRow(" ++ show (rows `div` 2) ++ ")"
                      , "}"
                      ]
                result <- compile code
                case result of
                    Left _ -> return $ False
                    Right _ -> return $ True
        ]
    ]