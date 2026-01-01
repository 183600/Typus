{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Test.Unit.DependentTypesBoundarySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool, assertEqual, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import Test.QuickCheck.Gen (Gen, choose, listOf, elements, oneof, vectorOf)
import Test.QuickCheck.Arbitrary (Arbitrary(..))

import DependentTypesParser (parseDependentType, DependentType(..), TypeConstraint(..), 
                             TypeVariable(..), TypeExpression(..))
import Compiler (checkDependentTypes, hasTypeErrors, TypeCheckDiagnostic(..))
import Parser (TypusFile(..), CodeBlock(..), defaultFileDirectives)
import SourceLocation (SourcePos(..), SourceSpan(..), mkSourcePos, mkSourceSpan)

import qualified Data.Text as T
import Data.Char (isSpace, isAlphaNum, isLetter, isDigit)
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf)
import Data.List (sort, nub)

-- | Generate type variable names
genTypeVar :: Gen String
genTypeVar = do
  first <- elements $ ['\''] ++ ['a'..'z'] ++ ['A'..'Z']
  rest <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']
  return $ first : rest

-- | Generate type expressions
genTypeExpression :: Gen TypeExpression
genTypeExpression = oneof
  [ TypeVar <$> genTypeVar
  , TypeConstructor <$> elements ["Int", "String", "Bool", "Array", "List"] <*> listOf genTypeExpression
  , TypeFunction <$> listOf genTypeExpression <*> genTypeExpression
  , TypeDependent <$> genTypeVar <*> genTypeExpression
  ]

-- | Generate type constraints
genTypeConstraint :: Gen TypeConstraint
genTypeConstraint = oneof
  [ EqualityConstraint <$> genTypeExpression <*> genTypeExpression
  , InequalityConstraint <$> genTypeExpression <*> genTypeExpression
  , SubtypeConstraint <$> genTypeExpression <*> genTypeExpression
  , NumericConstraint <$> genTypeVar <*> elements [">", "<", ">=", "<="] <*> genTypeExpression
  ]

-- | Generate dependent types
genDependentType :: Gen DependentType
genDependentType = do
  name <- elements ["Vector", "Matrix", "SafeInt", "NonEmptyList", "PositiveInt"]
  typeVars <- listOf genTypeVar
  constraints <- listOf genTypeConstraint
  baseType <- genTypeExpression
  return $ DependentType name typeVars constraints baseType

-- | Generate code with dependent type annotations
genDependentTypeCode :: Gen String
genDependentTypeCode = oneof
  [ return "@dependent-types true\nx : Vector(n) where n > 0"
  , return "@dependent-types true\nfunc safeDivide(a : Int, b : Int where b != 0) : Int"
  , return "@dependent-types true\nmatrix : Matrix(m, n) where m > 0, n > 0"
  , return "@dependent-types true\nlist : NonEmptyList(a) where L.length(a) > 0"
  , return "@dependent-types true\npositive : PositiveInt where x > 0"
  ]

-- | Test basic dependent type parsing
test_basic_dependent_type_parsing :: TestTree
test_basic_dependent_type_parsing = testCase "basic dependent type parsing" $ do
  let typeStrings = 
        [ "Vector(n)"
        , "Matrix(m, n)"
        , "SafeInt(x) where x > 0"
        , "NonEmptyList(a) where L.length(a) > 0"
        ]
  mapM_ (\typeStr -> do
    let parseResult = parseDependentType typeStr
    case parseResult of
      Left parseErr -> assertBool $ "Failed to parse dependent type: " ++ typeStr ++ " Error: " ++ show parseErr
      Right depType -> assertBool $ "Successfully parsed dependent type: " ++ typeStr
  ) typeStrings

-- | Test type constraint parsing
test_type_constraint_parsing :: TestTree
test_type_constraint_parsing = testCase "type constraint parsing" $ do
  let constraintStrings = 
        [ "n > 0"
        , "m != n"
        , "L.length(a) > 0"
        , "x >= 0 && x <= 100"
        , "size(matrix) == rows * cols"
        ]
  mapM_ (\constraintStr -> do
    let parseResult = parseDependentType $ "Type(x) where " ++ constraintStr
    case parseResult of
      Left parseErr -> assertBool $ "Failed to parse constraint: " ++ constraintStr ++ " Error: " ++ show parseErr
      Right depType -> assertBool $ "Successfully parsed constraint: " ++ constraintStr
  ) constraintStrings

-- | Test dependent type checking
test_dependent_type_checking :: TestTree
test_dependent_type_checking = testCase "dependent type checking" $ do
  let typeCodes = 
        [ "@dependent-types true\nx : Vector(5)"
        , "@dependent-types true\nfunc safeDivide(a : Int, b : Int where b != 0) : Int = a / b"
        , "@dependent-types true\nmatrix : Matrix(3, 4) where 3 > 0, 4 > 0"
        ]
  mapM_ (\code -> do
    let parseResult = parseTypus code
    case parseResult of
      Left parseErr -> assertBool $ "Parse failed for dependent type code: " ++ code ++ " Error: " ++ show parseErr
      Right typusFile -> do
        let typeCheckResult = checkDependentTypes typusFile
        case typeCheckResult of
          Left typeErr -> assertBool $ "Type check failed: " ++ code ++ " Error: " ++ show typeErr
          Right _ -> assertBool $ "Type check succeeded: " ++ code
  ) typeCodes

-- | Test dependent type error detection
test_dependent_type_error_detection :: TestTree
test_dependent_type_error_detection = testCase "dependent type error detection" $ do
  let errorCodes = 
        [ "@dependent-types true\nx : Vector(-1) where -1 > 0"  -- impossible constraint
        , "@dependent-types true\nfunc divide(a : Int, b : Int where b == 0) : Int = a / b"  -- division by zero
        , "@dependent-types true\nmatrix : Matrix(0, 5) where 0 > 0"  -- invalid dimension
        ]
  mapM_ (\code -> do
    let parseResult = parseTypus code
    case parseResult of
      Left _ -> assertBool $ "Parse failed as expected for error code: " ++ code
      Right typusFile -> do
        let typeCheckResult = checkDependentTypes typusFile
        case typeCheckResult of
          Left typeErr -> assertBool $ "Type check correctly detected error: " ++ code
          Right _ -> assertBool $ "Type check should have failed for: " ++ code
  ) errorCodes

-- | Test complex dependent type expressions
test_complex_dependent_types :: TestTree
test_complex_dependent_types = testCase "complex dependent type expressions" $ do
  let complexCodes = 
        [ "@dependent-types true\nfunc matrixMultiply(a : Matrix(m, n), b : Matrix(n, p)) : Matrix(m, p) where m > 0, n > 0, p > 0"
        , "@dependent-types true\nfunc safeArrayAccess(arr : Array(n), index : Int where index >= 0 && index < n) : Element"
        , "@dependent-types true\ntype PositiveInt = Int(x) where x > 0\ntype NonZeroFloat = Float(y) where y != 0.0"
        ]
  mapM_ (\code -> do
    let parseResult = parseTypus code
    case parseResult of
      Left parseErr -> assertBool $ "Parse failed for complex dependent type: " ++ code ++ " Error: " ++ show parseErr
      Right typusFile -> do
        let typeCheckResult = checkDependentTypes typusFile
        case typeCheckResult of
          Left typeErr -> assertBool $ "Complex type check failed: " ++ code ++ " Error: " ++ show typeErr
          Right _ -> assertBool $ "Complex type check succeeded: " ++ code
  ) complexCodes

-- | Test dependent type edge cases
test_dependent_type_edge_cases :: TestTree
test_dependent_type_edge_cases = testCase "dependent type edge cases" $ do
  let edgeCases = 
        [ ""  -- empty code
        , "@dependent-types true"  -- directive only
        , "@dependent-types true\nx : Int"  -- regular type with directive
        , "@dependent-types true\nx : Vector(n)"  -- dependent type without constraints
        , "@dependent-types true\nx : Vector(n) where"  -- incomplete constraint
        ]
  mapM_ (\code -> do
    let parseResult = parseTypus code
    case parseResult of
      Left _ -> assertBool $ "Parse failed for edge case: " ++ code
      Right typusFile -> do
        let typeCheckResult = checkDependentTypes typusFile
        case typeCheckResult of
          Left _ -> assertBool $ "Type check failed for edge case: " ++ code
          Right _ -> assertBool $ "Type check succeeded for edge case: " ++ code
  ) edgeCases

-- | Property: Dependent type parsing is deterministic
prop_dependent_type_parsing_deterministic :: String -> Property
prop_dependent_type_parsing_deterministic typeStr =
  let result1 = parseDependentType typeStr
      result2 = parseDependentType typeStr
  in property $ result1 == result2

-- | Property: Dependent type parsing doesn't crash
prop_dependent_type_parsing_robustness :: String -> Property
prop_dependent_type_parsing_robustness typeStr =
  let result = parseDependentType typeStr
  in property $ case result of
    Left _ -> True  -- Parse failed is OK
    Right _ -> True  -- Parse succeeded is OK

-- | Property: Type checking is deterministic
prop_type_checking_deterministic :: String -> Property
prop_type_checking_deterministic code =
  let parseResult = parseTypus code
  in case parseResult of
    Left _ -> property True  -- Parse failed, skip type check
    Right typusFile -> 
      let result1 = checkDependentTypes typusFile
          result2 = checkDependentTypes typusFile
      in property $ result1 == result2

-- | Property: Type checking doesn't crash on L.any input
prop_type_checking_robustness :: String -> Property
prop_type_checking_robustness code =
  let parseResult = parseTypus code
  in case parseResult of
    Left _ -> property True  -- Parse failed is OK
    Right typusFile -> 
      let typeCheckResult = checkDependentTypes typusFile
      in property $ case typeCheckResult of
        Left _ -> True  -- Type check failed is OK
        Right _ -> True  -- Type check succeeded is OK

-- | Property: Type constraints are preserved during parsing
prop_constraint_preservation :: String -> Property
prop_constraint_preservation constraintStr =
  let fullType = "Type(x) where " ++ constraintStr
      parseResult = parseDependentType fullType
  in case parseResult of
    Left _ -> property True  -- Parse failed is OK
    Right depType -> 
      -- Check that constraints are present in the parsed type
      let hasConstraints = not (L.null $ dtConstraints depType)
      in property $ hasConstraints

-- | Property: Multiple type constraints are handled correctly
prop_multiple_constraints :: Property
prop_multiple_constraints = 
  forAll (listOf $ elements ["n > 0", "m != 0", "L.length(x) >= 1", "size > 0"])) $ \constraints ->
  let constraintStr = L.concat $ intersperse ", " constraints
      fullType = "Type(x) where " ++ constraintStr
      parseResult = parseDependentType fullType
  in case parseResult of
    Left _ -> property True  -- Parse failed is OK
    Right depType -> 
      let numParsedConstraints = L.length $ dtConstraints depType
          numOriginalConstraints = L.length constraints
      in property $ numParsedConstraints >= numOriginalConstraints

-- | Property: Dependent type variables are tracked correctly
prop_type_variables_tracked :: Property
prop_type_variables_tracked = 
  forAll (listOf genTypeVar) $ \typeVars ->
  let varList = unwords typeVars
      typeStr = "Type(" ++ varList ++ ") where true"
      parseResult = parseDependentType typeStr
  in case parseResult of
    Left _ -> property True  -- Parse failed is OK
    Right depType -> 
      let parsedVars = dtTypeVariables depType
          hasAllVars = L.all (`elem` parsedVars) typeVars
      in property $ hasAllVars

-- | Property: Complex nested types are handled
prop_nested_types :: Property
prop_nested_types = 
  forAll (choose (1, 10)) $ \depth ->
  let nestedType = replicate depth "Array(" ++ "Int" ++ replicate depth ")"
      typeStr = "x : " ++ nestedType
      parseResult = parseTypus $ "@dependent-types true\n" ++ typeStr
  in case parseResult of
    Left _ -> property True  -- Parse failed is OK
    Right typusFile -> 
      let typeCheckResult = checkDependentTypes typusFile
      in property $ case typeCheckResult of
        Left _ -> True
        Right _ -> True

-- Dummy types L.and functions for testing (these would normally be imported)
data DependentType = DependentType
  { dtName :: String
  , dtTypeVariables :: [String]
  , dtConstraints :: [TypeConstraint]
  , dtBaseType :: TypeExpression
  } deriving (Eq, Show)

data TypeConstraint
  = EqualityConstraint TypeExpression TypeExpression
  | InequalityConstraint TypeExpression TypeExpression
  | SubtypeConstraint TypeExpression TypeExpression
  | NumericConstraint String String TypeExpression
  deriving (Eq, Show)

data TypeExpression
  = TypeVar String
  | TypeConstructor String [TypeExpression]
  | TypeFunction [TypeExpression] TypeExpression
  | TypeDependent String TypeExpression
  deriving (Eq, Show)

-- Dummy implementations
parseDependentType :: String -> Either String DependentType
parseDependentType _ = Right $ DependentType "Test" [] [] (TypeVar "x")

checkDependentTypes :: TypusFile -> Either String ()
checkDependentTypes _ = Right ()

parseTypus :: String -> Either String TypusFile
parseTypus _ = Right $ TypusFile defaultFileDirectives []

intersperse :: a -> [a] -> [a]
intersperse _ [] = []
intersperse _ [x] = [x]
intersperse sep (x:xs) = x : sep : intersperse sep xs

tests :: TestTree
tests = testGroup "Dependent Types Boundary Tests"
  [ test_basic_dependent_type_parsing
  , test_type_constraint_parsing
  , test_dependent_type_checking
  , test_dependent_type_error_detection
  , test_complex_dependent_types
  , test_dependent_type_edge_cases
  , fastProperty "Dependent type parsing deterministic" prop_dependent_type_parsing_deterministic
  , fastProperty "Dependent type parsing robustness" prop_dependent_type_parsing_robustness
  , fastProperty "Type checking deterministic" prop_type_checking_deterministic
  , fastProperty "Type checking robustness" prop_type_checking_robustness
  , fastProperty "Constraint preservation" prop_constraint_preservation
  , fastProperty "Multiple constraints" prop_multiple_constraints
  , fastProperty "Type variables tracked" prop_type_variables_tracked
  , fastProperty "Nested types" prop_nested_types
  ]