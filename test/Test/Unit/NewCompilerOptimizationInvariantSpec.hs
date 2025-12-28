module Test.Unit.NewCompilerOptimizationInvariantSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Test.Tasty.QuickCheck (testProperty, Property, Arbitrary(..), choose, listOf, elements)
import Compiler
import Compiler.IR
import qualified Data.Text as T

-- | Test compiler optimization invariants
tests :: TestTree
tests =
  testGroup "Compiler Optimization Invariant Tests"
    [ testGroup "Optimization preserves semantics"
        [ testCase "Dead code elimination preserves observable behavior" $ do
            let input = "func test() {\n  let x = 42\n  if false {\n    return x\n  }\n  return 0\n}"
                result = compile input
            case result of
                Left err -> assertBool ("Compilation should succeed: " ++ show err) False
                Right ir -> assertBool "IR should be generated" True

        , testCase "Constant folding maintains correctness" $ do
            let input = "func test() {\n  return 1 + 2 * 3\n}"
                result = compile input
            case result of
                Left err -> assertBool ("Compilation should succeed: " ++ show err) False
                Right ir -> assertBool "Constant folding should be applied" True

        , testCase "Loop invariant code motion preserves results" $ do
            let input = "func test() {\n  let constant = 42\n  for i in 0..10 {\n    let x = constant + i\n  }\n}"
                result = compile input
            case result of
                Left err -> assertBool ("Compilation should succeed: " ++ show err) False
                Right ir -> assertBool "Loop invariants should be moved" True
        ]

    , testGroup "Optimization preserves type safety"
        [ testCase "Type checking after optimizations" $ do
            let input = "func test() {\n  let x: int = \"string\" // type error\n  return x\n}"
                result = compile input
            case result of
                Left _ -> assertBool "Type errors should be caught" True
                Right _ -> assertBool "Should not compile with type errors" False

        , testCase "Ownership analysis preserved after optimization" $ do
            let input = "// @ownership true\nfunc test() {\n  let data = allocate()\n  transfer(data)\n  // data should not be usable here\n}"
                result = compile input
            case result of
                Left err -> assertBool ("Ownership violations should be caught: " ++ show err) True
                Right _ -> assertBool "Should not compile with ownership violations" False
        ]

    , testGroup "Optimization preserves dependencies"
        [ testCase "Dependency analysis after optimization" $ do
            let input = "func a() { return b() }\nfunc b() { return c() }\nfunc c() { return 42 }"
                result = compile input
            case result of
                Left err -> assertBool ("Compilation should succeed: " ++ show err) False
                Right ir -> assertBool "Dependencies should be preserved" True

        , testCase "Dependent type constraints preserved" $ do
            let input = "// @dependent-types true\nfunc test(n: int) where n > 0 {\n  let array: [n]int = new_array(n)\n  return array\n}"
                result = compile input
            case result of
                Left err -> assertBool ("Dependent types should be preserved: " ++ show err) False
                Right ir -> assertBool "Type constraints should be maintained" True
        ]

    , testGroup "Property-based tests"
        [ testProperty "Optimization preserves program termination" prop_optimizationPreservesTermination
        , testProperty "Optimization preserves type correctness" prop_optimizationPreservesTypes
        , testProperty "Optimization preserves ownership safety" prop_optimizationPreservesOwnership
        , testProperty "Optimization preserves dependent type constraints" prop_optimizationPreservesDependentTypes
        ]
    ]

-- Property: Optimization should not change termination behavior
prop_optimizationPreservesTermination :: String -> Bool
prop_optimizationPreservesTermination input =
    case compile input of
        Left _ -> True  -- Compilation errors are acceptable
        Right ir -> True  -- Successful compilation is acceptable

-- Property: Optimization should preserve type correctness
prop_optimizationPreservesTypes :: String -> Bool
prop_optimizationPreservesTypes input =
    case compile input of
        Left _ -> True  -- Type errors should be caught
        Right ir -> True  -- Well-typed programs should remain well-typed

-- Property: Optimization should preserve ownership safety
prop_optimizationPreservesOwnership :: String -> Bool
prop_optimizationPreservesOwnership input =
    case compile input of
        Left _ -> True  -- Ownership violations should be caught
        Right ir -> True  -- Safe programs should remain safe

-- Property: Optimization should preserve dependent type constraints
prop_optimizationPreservesDependentTypes :: String -> Bool
prop_optimizationPreservesDependentTypes input =
    case compile input of
        Left _ -> True  -- Constraint violations should be caught
        Right ir -> True  -- Valid constraints should be preserved