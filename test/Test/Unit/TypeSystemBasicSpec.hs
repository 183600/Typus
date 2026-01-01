{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.TypeSystemBasicSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===))
import Test.Tasty.HUnit (testCase, assertEqual, assertBool)

import Compiler.TypeChecker (Type(..), TypeScheme(..), TypeEnv, inferType, checkType)
import Compiler.ValueAnalysis (Value(..), evaluateValue)

-- | Test suite for Type System Basic operations
tests :: TestTree
tests = testGroup "Type System Basic"
  [ testProperty "type equality is reflexive" propTypeEqualityReflexive
  , testProperty "type equality is symmetric" propTypeEqualitySymmetric
  , testProperty "type equality is transitive" propTypeEqualityTransitive
  , testProperty "function type composition" propFunctionTypeComposition
  , testProperty "type variable substitution" propTypeVariableSubstitution
  , testCase "basic type inference" testBasicTypeInference
  , testCase "function type inference" testFunctionTypeInference
  , testCase "type checking basic expressions" testTypeCheckingBasicExpressions
  , testCase "type error detection" testTypeErrorDetection
  , testCase "polymorphic type inference" testPolymorphicTypeInference
  ]

-- | Property: type equality is reflexive
propTypeEqualityReflexive :: Type -> Property
propTypeEqualityReflexive t = property $ t == t

-- | Property: type equality is symmetric
propTypeEqualitySymmetric :: Type -> Type -> Property
propTypeEqualitySymmetric t1 t2 = property $ (t1 == t2) == (t2 == t1)

-- | Property: type equality is transitive
propTypeEqualityTransitive :: Type -> Type -> Type -> Property
propTypeEqualityTransitive t1 t2 t3 = 
  property $ (t1 == t2 && t2 == t3) ==> (t1 == t3)

-- | Property: function type composition
propFunctionTypeComposition :: Type -> Type -> Type -> Property
propFunctionTypeComposition t1 t2 t3 =
  let f1 = TypeFunction t1 t2
      f2 = TypeFunction t2 t3
      composed = TypeFunction t1 t3
  in property $ isComposable f1 f2 ==> hasResultType composed t3
  where
    isComposable (TypeFunction from to) (TypeFunction from' to') = to == from'
    isComposable _ _ = False
    hasResultType (TypeFunction _ result) expected = result == expected
    hasResultType _ _ = False

-- | Property: type variable substitution
propTypeVariableSubstitution :: String -> Type -> Type -> Property
propTypeVariableSubstitution varName replacement originalType =
  let substitution = [(varName, replacement)]
      resultType = substituteType substitution originalType
  in property $ substitutionWorks varName replacement originalType resultType
  where
    substitutionWorks name replacement original result =
      case original of
        TypeVariable name' | name' == name -> result == replacement
        TypeFunction from to -> 
          case result of
            TypeFunction from' to' -> substitutionWorks name replacement from from' &&
                                       substitutionWorks name replacement to to'
            _ -> False
        _ -> result == original

-- | Unit tests for basic type inference
testBasicTypeInference :: IO ()
testBasicTypeInference = do
  let intValue = IntValue 42
      boolValue = BoolValue True
      stringValue = StringValue "hello"
  
  result1 <- inferType emptyEnv intValue
  case result1 of
    Right t -> assertEqual "int type" TypeInt t
    Left _ -> assertFailure "Expected successful type inference"
    
  result2 <- inferType emptyEnv boolValue
  case result2 of
    Right t -> assertEqual "bool type" TypeBool t
    Left _ -> assertFailure "Expected successful type inference"
    
  result3 <- inferType emptyEnv stringValue
  case result3 of
    Right t -> assertEqual "string type" TypeString t
    Left _ -> assertFailure "Expected successful type inference"

-- | Unit tests for function type inference
testFunctionTypeInference :: IO ()
testFunctionTypeInference = do
  let funcValue = LambdaValue "x" (IntValue 42)
  
  result <- inferType emptyEnv funcValue
  case result of
    Right t -> assertEqual "function type" (TypeFunction TypeVar TypeInt) t
    Left _ -> assertFailure "Expected successful function type inference"

-- | Unit tests for type checking basic expressions
testTypeCheckingBasicExpressions :: IO ()
testTypeCheckingBasicExpressions = do
  let intExpr = IntValue 42
      boolExpr = BoolValue True
      expectedIntType = TypeInt
      expectedBoolType = TypeBool
  
  result1 <- checkType emptyEnv intExpr expectedIntType
  assertBool "int expression checks against int type" $ either (const False) (const True) result1
  
  result2 <- checkType emptyEnv boolExpr expectedBoolType
  assertBool "bool expression checks against bool type" $ either (const False) (const True) result2
  
  result3 <- checkType emptyEnv intExpr expectedBoolType
  assertBool "int expression doesn't check against bool type" $ either (const True) (const False) result3

-- | Unit tests for type error detection
testTypeErrorDetection :: IO ()
testTypeErrorDetection = do
  let expr = AddValue (IntValue 42) (BoolValue True)  -- Type error: adding int L.and bool
  
  result <- inferType emptyEnv expr
  case result of
    Right _ -> assertFailure "Expected type error"
    Left _ -> return ()

-- | Unit tests for polymorphic type inference
testPolymorphicTypeInference :: IO ()
testPolymorphicTypeInference = do
  let identityFunc = LambdaValue "x" (VariableValue "x")
  
  result <- inferType emptyEnv identityFunc
  case result of
    Right t -> assertBool "identity function has polymorphic type" $ isPolymorphic t
    Left _ -> assertFailure "Expected successful polymorphic type inference"

-- Helper types L.and functions
data Type = TypeInt | TypeBool | TypeString | TypeVar | TypeFunction Type Type deriving (Show, Eq)

data TypeScheme = TypeScheme [String] Type deriving (Show, Eq)

data Value = IntValue Int | BoolValue Bool | StringValue String | VariableValue String | 
             LambdaValue String Value | AddValue Value Value deriving (Show, Eq)

type TypeEnv = [(String, TypeScheme)]

emptyEnv :: TypeEnv
emptyEnv = []

-- Mock functions
inferType :: TypeEnv -> Value -> Either String Type
inferType env (IntValue _) = Right TypeInt
inferType env (BoolValue _) = Right TypeBool
inferType env (StringValue _) = Right TypeString
inferType env (VariableValue name) = 
  case lookup name env of
    Just (TypeScheme _ t) -> Right t
    Nothing -> Left $ "Unbound variable: " ++ name
inferType env (LambdaValue param body) = do
  bodyType <- inferType ((param, TypeScheme [] TypeVar) : env) body
  return $ TypeFunction TypeVar bodyType
inferType env (AddValue v1 v2) = do
  t1 <- inferType env v1
  t2 <- inferType env v2
  if t1 == TypeInt && t2 == TypeInt
    then return TypeInt
    else Left $ "Type error in addition: " ++ show t1 ++ " + " ++ show t2

checkType :: TypeEnv -> Value -> Type -> Either String ()
checkType env expr expectedType = do
  inferredType <- inferType env expr
  if inferredType == expectedType
    then return ()
    else Left $ "Type mismatch: expected " ++ show expectedType ++ ", got " ++ show inferredType

substituteType :: [(String, Type)] -> Type -> Type
substituteType substitution (TypeVariable name) = 
  case lookup name substitution of
    Just replacement -> replacement
    Nothing -> TypeVariable name
substituteType substitution (TypeFunction from to) = 
  TypeFunction (substituteType substitution from) (substituteType substitution to)
substituteType _ t = t

isPolymorphic :: Type -> Bool
isPolymorphic (TypeFunction from to) = isPolymorphic from || isPolymorphic to
isPolymorphic TypeVar = True
isPolymorphic _ = False

-- Helper function for property testing
property :: Bool -> Property
property = id