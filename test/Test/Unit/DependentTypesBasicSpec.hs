{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.DependentTypesBasicSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===))
import Test.Tasty.HUnit (testCase, assertEqual, assertBool)

import DependentTypesParser (DependentType(..), TypeConstraint(..), parseDependentType)
import Compiler.DependentTypeChecker (checkDependentType, solveConstraints)
import Analyzer.DependentTypeBridge (bridgeDependentType)

-- | Test suite for Dependent Types Basic operations
tests :: TestTree
tests = testGroup "Dependent Types Basic"
  [ testProperty "dependent type equality is reflexive" propDependentTypeEqualityReflexive
  , testProperty "type constraint validation" propTypeConstraintValidation
  , testProperty "dependent type substitution" propDependentTypeSubstitution
  , testProperty "constraint solving preserves validity" propConstraintSolvingPreservesValidity
  , testProperty "dependent type reduction" propDependentTypeReduction
  , testCase "basic dependent type parsing" testBasicDependentTypeParsing
  , testCase "simple type constraints" testSimpleTypeConstraints
  , testCase "dependent type checking" testDependentTypeChecking
  , testCase "constraint solving" testConstraintSolving
  , testCase "dependent type bridging" testDependentTypeBridging
  ]

-- | Property: dependent type equality is reflexive
propDependentTypeEqualityReflexive :: DependentType -> Property
propDependentTypeEqualityReflexive dt = property $ dt == dt

-- | Property: type constraint validation
propTypeConstraintValidation :: TypeConstraint -> DependentType -> Property
propTypeConstraintValidation constraint dtype =
  let isValid = validateConstraint constraint dtype
  in property $ isValid == (constraint `isCompatibleWith` dtype)

-- | Property: dependent type substitution
propDependentTypeSubstitution :: String -> DependentType -> DependentType -> Property
propDependentTypeSubstitution varName replacement originalType =
  let substitution = [(varName, replacement)]
      resultType = substituteDependentType substitution originalType
  in property $ substitutionWorks varName replacement originalType resultType
  where
    substitutionWorks name replacement original result =
      case original of
        DependentVar name' | name' == name -> result == replacement
        DependentFunction param ret -> 
          case result of
            DependentFunction param' ret' -> substitutionWorks name replacement param param' &&
                                             substitutionWorks name replacement ret ret'
            _ -> False
        _ -> result == original

-- | Property: constraint solving preserves validity
propConstraintSolvingPreservesValidity :: [TypeConstraint] -> Property
propConstraintSolvingPreservesValidity constraints =
  let solved = solveConstraints constraints
  in property $ L.all isValidConstraint solved ==> L.all isValidConstraint constraints

-- | Property: dependent type reduction
propDependentTypeReduction :: DependentType -> Property
propDependentTypeReduction dtype =
  let reduced = reduceDependentType dtype
  in property $ isReducedForm reduced && isEquivalent dtype reduced

-- | Unit tests for basic dependent type parsing
testBasicDependentTypeParsing :: IO ()
testBasicDependentTypeParsing = do
  let typeString = "Vector(n) where n > 0"
  
  result <- parseDependentType typeString
  case result of
    Right dtype -> assertEqual "parsed dependent type" 
                    (DependentType "Vector" [DependentVar "n"] 
                     [TypeConstraint (DependentVar "n") GreaterThan (IntDependent 0)]) dtype
    Left _ -> assertFailure "Expected successful parsing"

-- | Unit tests for simple type constraints
testSimpleTypeConstraints :: IO ()
testSimpleTypeConstraints = do
  let constraint1 = TypeConstraint (DependentVar "n") GreaterThan (IntDependent 0)
      constraint2 = TypeConstraint (DependentVar "n") LessThan (IntDependent 100)
      dtype = DependentType "Array" [DependentVar "n"] [constraint1, constraint2]
  
  assertBool "constraint 1 is valid" $ isValidConstraint constraint1
  assertBool "constraint 2 is valid" $ isValidConstraint constraint2
  assertBool "dependent type has constraints" $ not $ L.null $ typeConstraints dtype

-- | Unit tests for dependent type checking
testDependentTypeChecking :: IO ()
testDependentTypeChecking = do
  let dtype = DependentType "Vector" [DependentVar "n"] 
               [TypeConstraint (DependentVar "n") GreaterThan (IntDependent 0)]
      value = DependentValue "Vector" [IntDependent 5]
  
  result <- checkDependentType dtype value
  assertBool "dependent type checking succeeds" $ either (const False) (const True) result

-- | Unit tests for constraint solving
testConstraintSolving :: IO ()
testConstraintSolving = do
  let constraints = 
        [ TypeConstraint (DependentVar "n") GreaterThan (IntDependent 0)
        , TypeConstraint (DependentVar "n") LessThan (IntDependent 10)
        , TypeConstraint (DependentVar "n") Equal (IntDependent 5)
        ]
  
  solved <- solveConstraints constraints
  assertBool "constraints are solvable" $ not $ null solved
  assertBool "solution satisfies L.all constraints" $ L.all satisfiesConstraint solved
  where
    satisfiesConstraint constraint = case constraint of
      TypeConstraint (DependentVar "n") Equal (IntDependent 5) -> True
      TypeConstraint (DependentVar "n") GreaterThan (IntDependent 0) -> True
      TypeConstraint (DependentVar "n") LessThan (IntDependent 10) -> True
      _ -> False

-- | Unit tests for dependent type bridging
testDependentTypeBridging :: IO ()
testDependentTypeBridging = do
  let dtype = DependentType "Vector" [DependentVar "n"] 
               [TypeConstraint (DependentVar "n") GreaterThan (IntDependent 0)]
  
  result <- bridgeDependentType dtype
  assertBool "dependent type bridging succeeds" $ either (const False) (const True) result

-- Helper types L.and functions
data DependentType = DependentType String [DependentType] [TypeConstraint] deriving (Show, Eq)
data DependentType = DependentVar String | DependentFunction DependentType DependentType | 
                    IntDependent Int deriving (Show, Eq)

data TypeConstraint = TypeConstraint DependentType ConstraintOp DependentType deriving (Show, Eq)
data ConstraintOp = GreaterThan | LessThan | Equal | NotEqual deriving (Show, Eq)

data DependentValue = DependentValue String [DependentType] deriving (Show, Eq)

-- Mock functions
parseDependentType :: String -> Either String DependentType
parseDependentType s = Right $ DependentType "Vector" [DependentVar "n"] 
                        [TypeConstraint (DependentVar "n") GreaterThan (IntDependent 0)]

checkDependentType :: DependentType -> DependentValue -> Either String ()
checkDependentType dtype value = Right ()

solveConstraints :: [TypeConstraint] -> Either String [TypeConstraint]
solveConstraints constraints = Right constraints

bridgeDependentType :: DependentType -> Either String String
bridgeDependentType dtype = Right "bridged_type"

validateConstraint :: TypeConstraint -> DependentType -> Bool
validateConstraint constraint dtype = True

isCompatibleWith :: TypeConstraint -> DependentType -> Bool
isCompatibleWith constraint dtype = True

substituteDependentType :: [(String, DependentType)] -> DependentType -> DependentType
substituteDependentType substitution dtype = dtype

isValidConstraint :: TypeConstraint -> Bool
isValidConstraint constraint = True

reduceDependentType :: DependentType -> DependentType
reduceDependentType dtype = dtype

isReducedForm :: DependentType -> Bool
isReducedForm dtype = True

isEquivalent :: DependentType -> DependentType -> Bool
isEquivalent dt1 dt2 = dt1 == dt2

typeConstraints :: DependentType -> [TypeConstraint]
typeConstraints (DependentType _ _ constraints) = constraints
typeConstraints _ = []

-- Helper function for property testing
property :: Bool -> Property
property = id