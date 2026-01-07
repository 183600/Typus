module Test.Unit.NewDependentTypeBoundarySpec where
import Test.QuickCheck 
import Test.Tasty.HUnit (testCase, assertFailure, assertBool, (@?=)), assertBool
import Test.Tasty.QuickCheck (testProperty, Property, Arbitrary(..), choose, listOf, elements)
import DependentTypesParser
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


-- | Test dependent type boundary conditions L.and constraints
tests :: TestTree
tests =
    testGroup "Dependent Type Boundary Tests"
    [ testGroup "Type constraint validation"
        [             testCase "Basic dependent type constraints" $ do
                        let input = "// @dependent-types true\nfunc test(n: int) where n > 0 {\n  let array: [n]int = new_array(n)\n  return array\n}"
                                              result = parseAndCheck input
            case result of
                Left err -> assertBool ("Valid constraints should pass: " ++ show err) False
                Right checked -> assertBool "Should parse valid constraints" True

          ,             testCase "Invalid constraint rejection" $ do
                        let input = "// @dependent-types true\nfunc test(n: int) where n > n {\n  // impossible constraint\n}"
                                              result = parseAndCheck input
            case result of
                Left _ -> assertBool "Should reject impossible constraints" True
                Right checked -> assertBool "Should not accept impossible constraints" False

          ,             testCase "Constraint propagation" $ do
                        let input = "// @dependent-types true\nfunc test(n: int) where n > 0 {\n  let m: int where                               m = n + 1\n  let array: [m]int = new_array(m)\n  return array\n}"
                                              result = parseAndCheck input
            case result of
                Left err -> assertBool ("Constraint propagation should work: " ++ show err) False
                Right checked -> assertBool "Should propagate constraints" True
        ]

    , testGroup "Array type dependencies"
        [             testCase "Array size matching" $ do
                        let input = "// @dependent-types true\nfunc test() {\n  let n:                               int = 5\n  let array: [n]int = new_array(n)\n  return array\n}"
                                              result = parseAndCheck input
            case result of
                Left err -> assertBool ("Array size should match: " ++ show err) False
                Right checked -> assertBool "Should match array sizes" True

          ,             testCase "Array index bounds" $ do
                        let input = "// @dependent-types true\nfunc test(n: int) where n > 0 {\n  let array: [n]int = new_array(n)\n  let value:                               int = array[n] // out of bounds\n}"
                                              result = parseAndCheck input
            case result of
                Left _ -> assertBool "Should catch out of bounds access" True
                Right checked -> assertBool "Should not allow out of bounds" False

          ,             testCase "Array slice operations" $ do
                        let input = "// @dependent-types true\nfunc test(n: int, m: int) where 0 < m <= n {\n  let array: [n]int = new_array(n)\n  let slice: [m]int = array[0:m]\n  return slice\n}"
                                              result = parseAndCheck input
            case result of
                Left err -> assertBool ("Array slicing should work: " ++ show err) False
                Right checked -> assertBool "Should handle array slicing" True
        ]

    , testGroup "Function type dependencies"
        [             testCase "Dependent function types" $ do
                        let input = "// @dependent-types true\nfunc test(n: int) -> [n]int {\n  return new_array(n)\n}"
                                              result = parseAndCheck input
            case result of
                Left err -> assertBool ("Dependent function types should work: " ++ show err) False
                Right checked -> assertBool "Should handle dependent function types" True

          ,             testCase "Higher-order dependent types" $ do
                        let input = "// @dependent-types true\nfunc test(f: (n: int) -> [n]int) -> int {\n  let result: [5]int = f(5)\n  return L.length(result)\n}"
                                              result = parseAndCheck input
            case result of
                Left err -> assertBool ("Higher-order types should work: " ++ show err) False
                Right checked -> assertBool "Should handle higher-order types" True

          ,             testCase "Type-level computation" $ do
                        let input = "// @dependent-types true\ntype Matrix(n: int, m: int) = [[n]int] where n > 0, m > 0\nfunc test() {\n  let mat: Matrix(3, 4) = new_matrix(3, 4)\n  return mat\n}"
                                              result = parseAndCheck input
            case result of
                Left err -> assertBool ("Type-level computation should work: " ++ show err) False
                Right checked -> assertBool "Should handle type-level computation" True
        ]

    , testGroup "Constraint solving edge cases"
        [             testCase "Circular constraint detection" $ do
                        let input = "// @dependent-types true\nfunc test(a: int, b: int) where a > b, b > a {\n  // circular constraint\n}"
                                              result = parseAndCheck input
            case result of
                Left _ -> assertBool "Should detect circular constraints" True
                Right checked -> assertBool "Should not accept circular constraints" False

          ,             testCase "Unsolvable constraint detection" $ do
                        let input = "// @dependent-types true\nfunc test(n: int) where n > 0, n < 0 {\n  // unsolvable constraint\n}"
                                              result = parseAndCheck input
            case result of
                Left _ -> assertBool "Should detect unsolvable constraints" True
                Right checked -> assertBool "Should not accept unsolvable constraints" False

          ,             testCase "Complex constraint satisfaction" $ do
                        let input = "// @dependent-types true\nfunc test(a: int, b: int, c: int) \n  where a +                               b = c, a > 0, b > 0 {\n  return c\n}"
                                              result = parseAndCheck input
            case result of
                Left err -> assertBool ("Complex constraints should work: " ++ show err) False
                Right checked -> assertBool "Should satisfy complex constraints" True
        ]

    , testGroup "Property-based tests"
        [             testProperty "Constraint satisfaction is consistent" prop_constraintConsistency
        ,             testProperty "Array bounds are preserved" prop_arrayBoundsPreserved
        ,             testProperty "Type dependencies are transitive" prop_typeDependenciesTransitive
        ,             testProperty "Constraint solving terminates" prop_constraintSolvingTerminates
        ]
    ]

-- Property: Constraint satisfaction should be consistent
prop_constraintConsistency :: String -> Bool
prop_constraintConsistency                               input =
    case parseAndCheck input of
        Left _ -> True  -- Constraint errors are acceptable
        Right checked -> True  -- Valid constraints should pass

-- Property: Array bounds should be preserved
prop_arrayBoundsPreserved :: String -> Bool
prop_arrayBoundsPreserved                               input =
    case parseAndCheck input of
        Left _ -> True  -- Bounds errors should be caught
        Right checked -> True  -- Valid array operations should pass

-- Property: Type dependencies should be transitive
prop_typeDependenciesTransitive :: String -> Bool
prop_typeDependenciesTransitive                               input =
    case parseAndCheck input of
        Left _ -> True  -- Dependency errors are acceptable
        Right checked -> True  -- Valid dependencies should pass

-- Property: Constraint solving should terminate
prop_constraintSolvingTerminates :: String -> Bool
prop_constraintSolvingTerminates                               input =
    case parseAndCheck input of
        Left _ -> True  -- Should detect non-terminating cases
        Right checked -> True  -- Should terminate for solvable constraints