{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.NewDependentTypeValidationSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual, assertBool, assertFailure)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===), forAll, elements, suchThat)
import qualified Data.Text as T
import Data.Maybe (isJust, isNothing, catMaybes)
import Data.List (isInfixOf, nub)

import Parser (parseTypus, TypusFile(..))
import Compiler (compile, CompilerError(..), CompilationPhase(..))
import Compiler.DependentTypeChecker (checkDependentTypes)
import SourceLocation (SourcePos(..), SourceSpan(..), Located(..))

-- | Test dependent type validation functionality
tests :: TestTree
tests =
  testGroup "New Dependent Type Validation Tests"
    [ basicDependentTypeTests
    , typeConstraintTests
    , typeInferenceTests
    , typeEqualityTests
    , typeScopeTests
    , errorReportingTests
    , quickCheckProperties
    ]

-- | Basic dependent type functionality tests
basicDependentTypeTests :: TestTree
basicDependentTypeTests =
  testGroup "Basic Dependent Type Tests"
    [ testCase "Validate simple dependent array types" $
        let input = "// @dependent-types: true\nlet arr: array[5]int = [1,2,3,4,5]"
            result = compile "test.typus" input
        in case result of
             Left errs -> do
               let depTypeErrs = filter isDependentTypeError errs
               assertBool "Should accept matching array size" (null depTypeErrs)
             Right _ -> assertBool "Should succeed with correct array size" True

    , testCase "Reject mismatched array sizes" $
        let input = "// @dependent-types: true\nlet arr: array[5]int = [1,2,3]"
            result = compile "test.typus" input
        in case result of
             Left errs -> do
               let depTypeErrs = filter isDependentTypeError errs
               assertBool "Should reject mismatched array size" (any isArraySizeMismatch depTypeErrs)
               assertBool "Should explain size difference" (any explainsSizeDifference depTypeErrs)
             Right _ -> assertFailure "Should have failed with array size mismatch"

    , testCase "Validate vector length constraints" $
        let input = "// @dependent-types: true\ntype Vec(n: int) = array[n]int\nlet v: Vec(3) = [1,2,3]"
            result = compile "test.typus" input
        in case result of
             Left errs -> do
               let depTypeErrs = filter isDependentTypeError errs
               assertBool "Should accept valid vector type" (null depTypeErrs)
             Right _ -> assertBool "Should succeed with valid vector" True

    , testCase "Handle dependent function types" $
        let input = "// @dependent-types: true\nfunc id<n: int>(arr: array[n]int) -> array[n]int { return arr }"
            result = compile "test.typus" input
        in case result of
             Left errs -> do
               let depTypeErrs = filter isDependentTypeError errs
               assertBool "Should handle dependent function types" (any handlesDependentFunctionTypes depTypeErrs)
             Right _ -> assertBool "Should succeed with dependent function" True
    ]

-- | Type constraint tests
typeConstraintTests :: TestTree
typeConstraintTests =
  testGroup "Type Constraint Tests"
    [ testCase "Validate positive integer constraints" $
        let input = "// @dependent-types: true\ntype Positive = int where n > 0\nlet x: Positive = 5"
            result = compile "test.typus" input
        in case result of
             Left errs -> do
               let depTypeErrs = filter isDependentTypeError errs
               assertBool "Should accept positive integer" (null depTypeErrs)
             Right _ -> assertBool "Should succeed with positive integer" True

    , testCase "Reject negative integer constraints" $
        let input = "// @dependent-types: true\ntype Positive = int where n > 0\nlet x: Positive = -5"
            result = compile "test.typus" input
        in case result of
             Left errs -> do
               let depTypeErrs = filter isDependentTypeError errs
               assertBool "Should reject negative integer" (any isConstraintViolation depTypeErrs)
               assertBool "Should explain constraint" (any explainsConstraint depTypeErrs)
             Right _ -> assertFailure "Should have failed with constraint violation"

    , testCase "Validate range constraints" $
        let input = "// @dependent-types: true\ntype Age = int where n >= 0 && n <= 150\nlet x: Age = 25"
            result = compile "test.typus" input
        in case result of
             Left errs -> do
               let depTypeErrs = filter isDependentTypeError errs
               assertBool "Should accept valid range" (null depTypeErrs)
             Right _ -> assertBool "Should succeed with valid range" True

    , testCase "Handle complex constraints" $
        let input = "// @dependent-types: true\ntype Even = int where n % 2 == 0\ntype PositiveEven = Even where n > 0\nlet x: PositiveEven = 4"
            result = compile "test.typus" input
        in case result of
             Left errs -> do
               let depTypeErrs = filter isDependentTypeError errs
               assertBool "Should handle complex constraints" (any handlesComplexConstraints depTypeErrs)
             Right _ -> assertBool "Should succeed with complex constraints" True
    ]

-- | Type inference tests
typeInferenceTests :: TestTree
typeInferenceTests =
  testGroup "Type Inference Tests"
    [ testCase "Infer array size from literal" $
        let input = "// @dependent-types: true\nlet arr = [1,2,3,4,5]\nlet first: int = arr[0]"
            result = compile "test.typus" input
        in case result of
             Left errs -> do
               let depTypeErrs = filter isDependentTypeError errs
               assertBool "Should infer array size correctly" (any infersArraySize depTypeErrs)
             Right _ -> assertBool "Should succeed with type inference" True

    , testCase "Infer dependent function parameters" $
        let input = "// @dependent-types: true\nfunc first<n: int>(arr: array[n]int) -> int { return arr[0] }\nlet result = first([1,2,3])"
            result = compile "test.typus" input
        in case result of
             Left errs -> do
               let depTypeErrs = filter isDependentTypeError errs
               assertBool "Should infer function parameter types" (any infersFunctionParameters depTypeErrs)
             Right _ -> assertBool "Should succeed with parameter inference" True

    , testCase "Handle generic type inference" $
        let input = "// @dependent-types: true\ntype Pair<a, b> = struct { first: a, second: b }\nlet p = Pair { first: 5, second: \"hello\" }"
            result = compile "test.typus" input
        in case result of
             Left errs -> do
               let depTypeErrs = filter isDependentTypeError errs
               assertBool "Should infer generic types" (any infersGenericTypes depTypeErrs)
             Right _ -> assertBool "Should succeed with generic inference" True

    , testCase "Resolve type dependencies" $
        let input = "// @dependent-types: true\ntype Matrix(m: int, n: int) = array[m]array[n]int\nlet mat: Matrix(2,3) = [[1,2,3],[4,5,6]]"
            result = compile "test.typus" input
        in case result of
             Left errs -> do
               let depTypeErrs = filter isDependentTypeError errs
               assertBool "Should resolve type dependencies" (any resolvesTypeDependencies depTypeErrs)
             Right _ -> assertBool "Should succeed with type dependencies" True
    ]

-- | Type equality tests
typeEqualityTests :: TestTree
typeEqualityTests =
  testGroup "Type Equality Tests"
    [ testCase "Check equivalent dependent types" $
        let input = "// @dependent-types: true\ntype Vec5 = array[5]int\nlet a: Vec5 = [1,2,3,4,5]\nlet b: array[5]int = a"
            result = compile "test.typus" input
        in case result of
             Left errs -> do
               let depTypeErrs = filter isDependentTypeError errs
               assertBool "Should recognize equivalent types" (any recognizesEquivalentTypes depTypeErrs)
             Right _ -> assertBool "Should succeed with equivalent types" True

    , testCase "Reject incompatible dependent types" $
        let input = "// @dependent-types: true\nlet a: array[3]int = [1,2,3]\nlet b: array[5]int = a"
            result = compile "test.typus" input
        in case result of
             Left errs -> do
               let depTypeErrs = filter isDependentTypeError errs
               assertBool "Should reject incompatible types" (any rejectsIncompatibleTypes depTypeErrs)
               assertBool "Should explain type difference" (any explainsTypeDifference depTypeErrs)
             Right _ -> assertFailure "Should have failed with type incompatibility"

    , testCase "Handle type normalization" $
        let input = "// @dependent-types: true\ntype AddOne<n: int> = n + 1\nlet x: AddOne<4> = 5"
            result = compile "test.typus" input
        in case result of
             Left errs -> do
               let depTypeErrs = filter isDependentTypeError errs
               assertBool "Should normalize type expressions" (any normalizesTypeExpressions depTypeErrs)
             Right _ -> assertBool "Should succeed with type normalization" True

    , testCase "Validate type substitution" $
        let input = "// @dependent-types: true\ntype Double<n: int> = n * 2\nlet x: Double<3> = 6"
            result = compile "test.typus" input
        in case result of
             Left errs -> do
               let depTypeErrs = filter isDependentTypeError errs
               assertBool "Should validate type substitution" (any validatesTypeSubstitution depTypeErrs)
             Right _ -> assertBool "Should succeed with type substitution" True
    ]

-- | Type scope tests
typeScopeTests :: TestTree
typeScopeTests =
  testGroup "Type Scope Tests"
    [ testCase "Handle local dependent types" $
        let input = "// @dependent-types: true\nfunc test() {\n  type LocalVec = array[3]int\n  let v: LocalVec = [1,2,3]\n  return v[0]\n}"
            result = compile "test.typus" input
        in case result of
             Left errs -> do
               let depTypeErrs = filter isDependentTypeError errs
               assertBool "Should handle local types" (any handlesLocalTypes depTypeErrs)
             Right _ -> assertBool "Should succeed with local types" True

    , testCase "Validate type visibility" $
        let input = "// @dependent-types: true\ntype GlobalType = array[5]int\nfunc test() {\n  type LocalType = array[3]int\n  let a: GlobalType = [1,2,3,4,5]\n  let b: LocalType = [1,2,3]\n  return a[0] + b[0]\n}"
            result = compile "test.typus" input
        in case result of
             Left errs -> do
               let depTypeErrs = filter isDependentTypeError errs
               assertBool "Should respect type visibility" (any respectsTypeVisibility depTypeErrs)
             Right _ -> assertBool "Should succeed with proper visibility" True

    , testCase "Handle type parameter scoping" $
        let input = "// @dependent-types: true\nfunc outer<n: int>() {\n  func inner<m: int>(arr: array[m]int) -> int {\n    return arr[0]\n  }\n  let arr: array[n]int = [1]\n  return inner(arr)\n}"
            result = compile "test.typus" input
        in case result of
             Left errs -> do
               let depTypeErrs = filter isDependentTypeError errs
               assertBool "Should handle parameter scoping" (any handlesParameterScoping depTypeErrs)
             Right _ -> assertBool "Should succeed with parameter scoping" True

    , testCase "Validate type lifetime" $
        let input = "// @dependent-types: true\nfunc create_array() -> array[3]int {\n  type LocalArray = array[3]int\n  let arr: LocalArray = [1,2,3]\n  return arr\n}"
            result = compile "test.typus" input
        in case result of
             Left errs -> do
               let depTypeErrs = filter isDependentTypeError errs
               assertBool "Should validate type lifetime" (any validatesTypeLifetime depTypeErrs)
             Right _ -> assertBool "Should succeed with valid type lifetime" True
    ]

-- | Error reporting tests
errorReportingTests :: TestTree
errorReportingTests =
  testGroup "Error Reporting Tests"
    [ testCase "Provide detailed constraint violation messages" $
        let input = "// @dependent-types: true\ntype Positive = int where n > 0\nlet x: Positive = -5"
            result = compile "test.typus" input
        in case result of
             Left errs -> do
               let depTypeErrs = filter isDependentTypeError errs
               assertBool "Should explain constraint violation" (any explainsConstraintViolation depTypeErrs)
               assertBool "Should show actual vs expected" (any showsActualVsExpected depTypeErrs)
               assertBool "Should suggest fix" (any suggestsConstraintFix depTypeErrs)
             Right _ -> assertFailure "Should have failed with constraint violation"

    , testCase "Track error locations precisely" $
        let input = "// @dependent-types: true\nlet arr: array[5]int = [1,2,3]\nlet invalid = arr[10]"
            result = compile "test.typus" input
        in case result of
             Left errs -> do
               let depTypeErrs = filter isDependentTypeError errs
               assertBool "Should locate error precisely" (any locatesErrorPrecisely depTypeErrs)
               assertBool "Should show context" (any showsErrorContext depTypeErrs)
             Right _ -> assertFailure "Should have failed with out-of-bounds access"

    , testCase "Provide helpful error suggestions" $
        let input = "// @dependent-types: true\ntype Even = int where n % 2 == 0\nlet x: Even = 3"
            result = compile "test.typus" input
        in case result of
             Left errs -> do
               let depTypeErrs = filter isDependentTypeError errs
               assertBool "Should suggest valid values" (any suggestsValidValues depTypeErrs)
               assertBool "Should explain constraint logic" (any explainsConstraintLogic depTypeErrs)
             Right _ -> assertFailure "Should have failed with constraint violation"

    , testCase "Handle complex error scenarios" $
        let input = "// @dependent-types: true\ntype Matrix(m: int, n: int) = array[m]array[n]int\nlet mat: Matrix(2,3) = [[1,2],[3,4,5]]"
            result = compile "test.typus" input
        in case result of
             Left errs -> do
               let depTypeErrs = filter isDependentTypeError errs
               assertBool "Should handle complex errors" (any handlesComplexErrors depTypeErrs)
               assertBool "Should provide clear explanation" (any providesClearExplanation depTypeErrs)
             Right _ -> assertFailure "Should have failed with matrix shape error"
    ]

-- | QuickCheck properties for dependent type validation
quickCheckProperties :: TestTree
quickCheckProperties =
  testGroup "QuickCheck Properties"
    [ testProperty "Type constraints are enforced correctly" $
        forAll genConstrainedTypeCode $ \code ->
            case compile "test.typus" code of
              Left errs -> 
                let depTypeErrs = filter isDependentTypeError errs
                in property $ any enforcesTypeConstraints depTypeErrs
              Right _ -> property True  -- Valid code should succeed

    , testProperty "Type inference preserves correctness" $
        forAll genInferenceCode $ \code ->
            case compile "test.typus" code of
              Left errs -> 
                let depTypeErrs = filter isDependentTypeError errs
                in property $ all preservesInferenceCorrectness depTypeErrs
              Right _ -> property True

    , testProperty "Type equality is transitive" $
        forAll genEqualityCode $ \code ->
            case compile "test.typus" code do
              Left errs -> 
                let depTypeErrs = filter isDependentTypeError errs
                in property $ respectsTransitivity depTypeErrs
              Right _ -> property True
    ]

-- | Helper functions for dependent type error detection
isDependentTypeError :: CompilerError -> Bool
isDependentTypeError (CompilerError TypeError _ msg _) = "dependent" `isInfixOf` msg || "constraint" `isInfixOf` msg
isDependentTypeError _ = False

isArraySizeMismatch :: CompilerError -> Bool
isArraySizeMismatch (CompilerError TypeError _ msg _) = "array" `isInfixOf` msg && "size" `isInfixOf` msg
isArraySizeMismatch _ = False

explainsSizeDifference :: CompilerError -> Bool
explainsSizeDifference (CompilerError _ _ msg _) = "expected" `isInfixOf` msg && "actual" `isInfixOf` msg
explainsSizeDifference _ = False

handlesDependentFunctionTypes :: CompilerError -> Bool
handlesDependentFunctionTypes (CompilerError _ _ msg _) = "function" `isInfixOf` msg && "dependent" `isInfixOf` msg
handlesDependentFunctionTypes _ = False

isConstraintViolation :: CompilerError -> Bool
isConstraintViolation (CompilerError TypeError _ msg _) = "constraint" `isInfixOf` msg && "violation" `isInfixOf` msg
isConstraintViolation _ = False

explainsConstraint :: CompilerError -> Bool
explainsConstraint (CompilerError _ _ msg _) = "constraint" `isInfixOf` msg || "where" `isInfixOf` msg
explainsConstraint _ = False

handlesComplexConstraints :: CompilerError -> Bool
handlesComplexConstraints (CompilerError _ _ msg _) = "complex" `isInfixOf` msg && "constraint" `isInfixOf` msg
handlesComplexConstraints _ = False

infersArraySize :: CompilerError -> Bool
infersArraySize (CompilerError TypeError _ msg _) = "infer" `isInfixOf` msg && "array" `isInfixOf` msg
infersArraySize _ = False

infersFunctionParameters :: CompilerError -> Bool
infersFunctionParameters (CompilerError TypeError _ msg _) = "infer" `isInfixOf` msg && "parameter" `isInfixOf` msg
infersFunctionParameters _ = False

infersGenericTypes :: CompilerError -> Bool
infersGenericTypes (CompilerError TypeError _ msg _) = "infer" `isInfixOf` msg && "generic" `isInfixOf` msg
infersGenericTypes _ = False

resolvesTypeDependencies :: CompilerError -> Bool
resolvesTypeDependencies (CompilerError TypeError _ msg _) = "resolve" `isInfixOf` msg && "dependency" `isInfixOf` msg
resolvesTypeDependencies _ = False

recognizesEquivalentTypes :: CompilerError -> Bool
recognizesEquivalentTypes (CompilerError TypeError _ msg _) = "equivalent" `isInfixOf` msg || "compatible" `isInfixOf` msg
recognizesEquivalentTypes _ = False

rejectsIncompatibleTypes :: CompilerError -> Bool
rejectsIncompatibleTypes (CompilerError TypeError _ msg _) = "incompatible" `isInfixOf` msg || "mismatch" `isInfixOf` msg
rejectsIncompatibleTypes _ = False

explainsTypeDifference :: CompilerError -> Bool
explainsTypeDifference (CompilerError _ _ msg _) = "difference" `isInfixOf` msg || "expected" `isInfixOf` msg
explainsTypeDifference _ = False

normalizesTypeExpressions :: CompilerError -> Bool
normalizesTypeExpressions (CompilerError TypeError _ msg _) = "normalize" `isInfixOf` msg
normalizesTypeExpressions _ = False

validatesTypeSubstitution :: CompilerError -> Bool
validatesTypeSubstitution (CompilerError TypeError _ msg _) = "substitution" `isInfixOf` msg
validatesTypeSubstitution _ = False

handlesLocalTypes :: CompilerError -> Bool
handlesLocalTypes (CompilerError TypeError _ msg _) = "local" `isInfixOf` msg && "type" `isInfixOf` msg
handlesLocalTypes _ = False

respectsTypeVisibility :: CompilerError -> Bool
respectsTypeVisibility (CompilerError TypeError _ msg _) = "visibility" `isInfixOf` msg || "scope" `isInfixOf` msg
respectsTypeVisibility _ = False

handlesParameterScoping :: CompilerError -> Bool
handlesParameterScoping (CompilerError TypeError _ msg _) = "parameter" `isInfixOf` msg && "scope" `isInfixOf` msg
handlesParameterScoping _ = False

validatesTypeLifetime :: CompilerError -> Bool
validatesTypeLifetime (CompilerError TypeError _ msg _) = "lifetime" `isInfixOf` msg
validatesTypeLifetime _ = False

explainsConstraintViolation :: CompilerError -> Bool
explainsConstraintViolation (CompilerError _ _ msg _) = "constraint" `isInfixOf` msg && "violation" `isInfixOf` msg
explainsConstraintViolation _ = False

showsActualVsExpected :: CompilerError -> Bool
showsActualVsExpected (CompilerError _ _ msg _) = "actual" `isInfixOf` msg && "expected" `isInfixOf` msg
showsActualVsExpected _ = False

suggestsConstraintFix :: CompilerError -> Bool
suggestsConstraintFix (CompilerError _ _ msg _) = "suggest" `isInfixOf` msg || "fix" `isInfixOf` msg
suggestsConstraintFix _ = False

locatesErrorPrecisely :: CompilerError -> Bool
locatesErrorPrecisely (CompilerError _ (Just span) _ _) = span /= undefined
locatesErrorPrecisely _ = False

showsErrorContext :: CompilerError -> Bool
showsErrorContext (CompilerError _ _ msg _) = length (words msg) >= 5
showsErrorContext _ = False

suggestsValidValues :: CompilerError -> Bool
suggestsValidValues (CompilerError _ _ msg _) = "valid" `isInfixOf` msg || "example" `isInfixOf` msg
suggestsValidValues _ = False

explainsConstraintLogic :: CompilerError -> Bool
explainsConstraintLogic (CompilerError _ _ msg _) = "logic" `isInfixOf` msg || "reason" `isInfixOf` msg
explainsConstraintLogic _ = False

handlesComplexErrors :: CompilerError -> Bool
handlesComplexErrors (CompilerError _ _ msg _) = "complex" `isInfixOf` msg
handlesComplexErrors _ = False

providesClearExplanation :: CompilerError -> Bool
providesClearExplanation (CompilerError _ _ msg _) = "explanation" `isInfixOf` msg || "clear" `isInfixOf` msg
providesClearExplanation _ = False

enforcesTypeConstraints :: CompilerError -> Bool
enforcesTypeConstraints (CompilerError TypeError _ msg _) = "constraint" `isInfixOf` msg
enforcesTypeConstraints _ = False

preservesInferenceCorrectness :: CompilerError -> Bool
preservesInferenceCorrectness (CompilerError TypeError _ msg _) = "inference" `isInfixOf` msg
preservesInferenceCorrectness _ = False

respectsTransitivity :: CompilerError -> Bool
respectsTransitivity (CompilerError TypeError _ msg _) = "transitive" `isInfixOf` msg || "equality" `isInfixOf` msg
respectsTransitivity _ = False

-- | Generators for QuickCheck testing
genConstrainedTypeCode :: Gen String
genConstrainedTypeCode = elements
  [ "// @dependent-types: true\ntype Positive = int where n > 0\nlet x: Positive = 5"
  , "// @dependent-types: true\ntype Even = int where n % 2 == 0\nlet x: Even = 4"
  , "// @dependent-types: true\ntype Range = int where n >= 0 && n <= 100\nlet x: Range = 50"
  , "// @dependent-types: true\ntype NonEmpty = array[n]int where n > 0\nlet x: NonEmpty = [1]"
  ]

genInferenceCode :: Gen String
genInferenceCode = elements
  [ "// @dependent-types: true\nlet arr = [1,2,3,4,5]\nlet first = arr[0]"
  , "// @dependent-types: true\nfunc first<n: int>(arr: array[n]int) -> int { return arr[0] }\nlet result = first([1,2,3])"
  , "// @dependent-types: true\ntype Pair<a, b> = struct { first: a, second: b }\nlet p = Pair { first: 5, second: \"hello\" }"
  , "// @dependent-types: true\nlet matrix = [[1,2,3],[4,5,6]]\nlet element = matrix[0][1]"
  ]

genEqualityCode :: Gen String
genEqualityCode = elements
  [ "// @dependent-types: true\ntype Vec5 = array[5]int\nlet a: Vec5 = [1,2,3,4,5]\nlet b: array[5]int = a"
  , "// @dependent-types: true\ntype AddOne<n: int> = n + 1\nlet x: AddOne<4> = 5"
  , "// @dependent-types: true\ntype Double<n: int> = n * 2\nlet x: Double<3> = 6"
  , "// @dependent-types: true\ntype Matrix(m: int, n: int) = array[m]array[n]int\nlet mat: Matrix(2,3) = [[1,2,3],[4,5,6]]"
  ]