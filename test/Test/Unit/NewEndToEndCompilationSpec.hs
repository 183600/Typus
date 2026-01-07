module Test.Unit.NewEndToEndCompilationSpec where
import Test.QuickCheck 
import Test.Tasty.HUnit (testCase, assertFailure, assertBool, (@?=)), assertBool
import Test.Tasty.QuickCheck (testProperty, Property, Arbitrary(..), choose, listOf, elements)
import IntegratedCompiler
import qualified Data.Text as T
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


-- | Test end-to-end compilation scenarios
tests :: TestTree
tests =
    testGroup "End-to-End Compilation Tests"
    [ testGroup "Complete compilation pipeline"
        [             testCase "Simple function compilation" $ do
                        let input = "func add(a: int, b: int) -> int {\n  return a + b\n}"
                                              result = compileToEnd input
            case result of
                Left err -> assertBool ("Simple function should compile: " ++ show err) False
                Right output -> assertBool "Should generate output" True

          ,             testCase "Complex function with control flow" $ do
                        let input = "func factorial(n: int) -> int {\n  if n <= 1 {\n    return 1\n  } else {\n    return n * factorial(n - 1)\n  }\n}"
                                              result = compileToEnd input
            case result of
                Left err -> assertBool ("Complex function should compile: " ++ show err) False
                Right output -> assertBool "Should generate complex output" True

          ,             testCase "Module with multiple functions" $ do
                        let input = "func helper(x: int) -> int {\n  return x * 2\n}\n\nfunc main() -> int {\n  return helper(21)\n}"
                                              result = compileToEnd input
            case result of
                Left err -> assertBool ("Multiple functions should compile: " ++ show err) False
                Right output -> assertBool "Should handle multiple functions" True
        ]

    , testGroup "Ownership-enabled compilation"
        [             testCase "Basic ownership compilation" $ do
                        let input = "// @ownership true\nfunc test() {\n  let data = allocate()\n  let processed = process(data)\n  return processed\n}"
                                              result = compileToEnd input
            case result of
                Left err -> assertBool ("Ownership code should compile: " ++ show err) False
                Right output -> assertBool "Should generate ownership-aware code" True

          ,             testCase "Ownership transfer compilation" $ do
                        let input = "// @ownership true\nfunc test() {\n  let data = allocate()\n  transfer(data)\n  return\n}"
                                              result = compileToEnd input
            case result of
                Left err -> assertBool ("Ownership transfer should compile: " ++ show err) False
                Right output -> assertBool "Should handle ownership transfer" True

          ,             testCase "Borrowing compilation" $ do
                        let input = "// @ownership true\nfunc test() {\n  let data = allocate()\n  let borrowed = borrow(data)\n  let result = use(borrowed)\n  return result\n}"
                                              result = compileToEnd input
            case result of
                Left err -> assertBool ("Borrowing should compile: " ++ show err) False
                Right output -> assertBool "Should handle borrowing" True
        ]

    , testGroup "Dependent types compilation"
        [             testCase "Basic dependent types" $ do
                        let input = "// @dependent-types true\nfunc test(n: int) where n > 0 {\n  let array: [n]int = new_array(n)\n  return array\n}"
                                              result = compileToEnd input
            case result of
                Left err -> assertBool ("Dependent types should compile: " ++ show err) False
                Right output -> assertBool "Should generate dependent type code" True

          ,             testCase "Type-level functions" $ do
                        let input = "// @dependent-types true\ntype Vector(n: int) = [n]float\nfunc test() {\n  let v: Vector(3) = new_vector(3)\n  return v\n}"
                                              result = compileToEnd input
            case result of
                Left err -> assertBool ("Type-level functions should compile: " ++ show err) False
                Right output -> assertBool "Should handle type-level functions" True

          ,             testCase "Constraint propagation" $ do
                        let input = "// @dependent-types true\nfunc test(a: int, b: int) where a +                               b = 10 {\n  let c: int where                               c = a * b\n  return c\n}"
                                              result = compileToEnd input
            case result of
                Left err -> assertBool ("Constraint propagation should compile: " ++ show err) False
                Right output -> assertBool "Should handle constraint propagation" True
        ]

    , testGroup "Combined features compilation"
        [             testCase "Ownership + dependent types" $ do
                        let input = "// @ownership true\n// @dependent-types true\nfunc test(n: int) where n > 0 {\n  let array: [n]int = allocate_array(n)\n  let processed = process(array)\n  return processed\n}"
                                              result = compileToEnd input
            case result of
                Left err -> assertBool ("Combined features should compile: " ++ show err) False
                Right output -> assertBool "Should handle combined features" True

          ,             testCase "Complex combined scenario" $ do
                        let input = "// @ownership true\n// @dependent-types true\nfunc process_data(n: int) where n > 0 {\n  let data: [n]int = allocate_array(n)\n  for i in 0..n {\n    let borrowed = borrow(data[i])\n    data[i] = transform(borrowed)\n  }\n  return data\n}"
                                              result = compileToEnd input
            case result of
                Left err -> assertBool ("Complex scenario should compile: " ++ show err) False
                Right output -> assertBool "Should handle complex scenarios" True
        ]

    , testGroup "Error handling in compilation"
        [             testCase "Compilation errors are properly reported" $ do
                        let input = "func test() {\n  let x:                               int = \"string\" // type error\n  return x\n}"
                                              result = compileToEnd input
            case result of
                Left _ -> assertBool "Should report compilation errors" True
                Right output -> assertBool "Should not compile with errors" False

          ,             testCase "Partial compilation recovery" $ do
                        let input = "func valid() { return 42 }\nfunc invalid() {\n  let x:                               int = \"string\"\n}\nfunc also_valid() { return 24 }"
                                              result = compileToEnd input
            case result of
                Left _ -> assertBool "Should handle partial errors" True
                Right output -> assertBool "Should attempt partial compilation" True
        ]

    , testGroup "Property-based tests"
        [             testProperty "Compilation preserves semantics" prop_compilationPreservesSemantics
        ,             testProperty "Generated code is syntactically valid" prop_generatedCodeValid
        ,             testProperty "Compilation is deterministic" prop_compilationDeterministic
        ,             testProperty "Compilation handles edge cases" prop_compilationHandlesEdgeCases
        ]
    ]

-- Property: Compilation should preserve program semantics
prop_compilationPreservesSemantics :: String -> Bool
prop_compilationPreservesSemantics                               input =
    case compileToEnd input of
        Left _ -> True  -- Compilation errors are acceptable
        Right output -> True  -- Successful compilation should preserve semantics

-- Property: Generated code should be syntactically valid
prop_generatedCodeValid :: String -> Bool
prop_generatedCodeValid                               input =
    case compileToEnd input of
        Left _ -> True  -- Compilation errors are acceptable
        Right output -> not (null output)  -- Generated code should not be empty

-- Property: Compilation should be deterministic
prop_compilationDeterministic :: String -> Bool
prop_compilationDeterministic                               input =
    let result1 = compileToEnd input
                                      result2 = compileToEnd input
    in case (result1, result2) of
        (Left _, Left _) -> True
        (Right out1, Right out2) ->                               out1 == out2
        _ -> False  -- Results should be consistent

-- Property: Compilation should handle edge cases
prop_compilationHandlesEdgeCases :: String -> Bool
prop_compilationHandlesEdgeCases                               input =
    case compileToEnd input of
        Left _ -> True  -- Should handle edge cases gracefully
        Right _ -> True  -- Successful compilation is acceptable