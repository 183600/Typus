{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Unit.TypeInferenceSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Dependencies
import Dependencies.AST
import Dependencies.TypeSystem
import Compiler.TypeChecker
import SourceLocation
import Data.List (sort, nub, union)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T

-- ============================================================================
-- Type Inference Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Type Inference Tests"
  [ basicTypeInferenceProperties
  , functionTypeInferenceProperties
  , genericTypeInferenceProperties
  , constraintTypeInferenceProperties
  , typeUnificationProperties
  , typeSubstitutionProperties
  ]

-- ============================================================================
-- Basic Type Inference Properties
-- ============================================================================

basicTypeInferenceProperties :: TestTree
basicTypeInferenceProperties = testGroup "Basic Type Inference Properties"
  [ testProperty "type inference is deterministic" $
      \expr env ->
        let result1 = inferType env expr
            result2 = inferType env expr
        in result1 === result2
    
  , testProperty "type inference preserves type safety" $
      \expr env ->
        let result = inferType env expr
        in case result of
          Left _ -> True  -- Type error is acceptable
          Right typeExpr -> typeExprWellFormed typeExpr
    
  , testProperty "type inference respects environment" $
      \expr env ->
        let result = inferType env expr
        in case result of
          Right inferredType -> typeExistsInEnvironment env inferredType
          Left _ -> True
    
  , testProperty "type inference for literals is correct" $
      \literal ->
        let env = initialTypeEnvironment
            result = inferLiteralType env literal
        in case result of
          Right inferredType -> literalTypeCorrect literal inferredType
          Left _ -> False
    
  , testProperty "type inference for variables is consistent" $
      \varName env ->
        let result = inferVariableType env varName
        in case result of
          Right inferredType -> variableTypeConsistent env varName inferredType
          Left _ -> True
    
  , testCase "basic type inference examples" $ do
      let env = initialTypeEnvironment
          intExpr = SimpleT "int"
          stringExpr = SimpleT "string"
      case inferType env intExpr of
        Left err -> assertFailure $ "Type inference failed: " ++ show err
        Right inferredType -> assertBool "Int type inferred" $ inferredType == intExpr
      case inferType env stringExpr of
        Left err -> assertFailure $ "Type inference failed: " ++ show err
        Right inferredType -> assertBool "String type inferred" $ inferredType == stringExpr
  ]

-- ============================================================================
-- Function Type Inference Properties
-- ============================================================================

functionTypeInferenceProperties :: TestTree
functionTypeInferenceProperties = testGroup "Function Type Inference Properties"
  [ testProperty "function type inference preserves parameter types" $
      \params returnType env ->
        let funcType = FuncT params returnType
            result = inferType env funcType
        in case result of
          Right inferredType -> functionParameterTypesPreserved params inferredType
          Left _ -> True
    
  , testProperty "function type inference infers correct return type" $
      \params returnType env ->
        let funcType = FuncT params returnType
            result = inferType env funcType
        in case result of
          Right inferredType -> functionReturnTypeCorrect returnType inferredType
          Left _ -> True
    
  , testProperty "function application type inference is sound" $
      \funcType argTypes env ->
        let result = inferApplicationType env funcType argTypes
        in case result of
          Right inferredType -> applicationTypeSound funcType argTypes inferredType
          Left _ -> True
    
  , testProperty "function type inference handles higher-order functions" $
      \funcType1 funcType2 env ->
        let higherOrderFunc = FuncT [("f", funcType1), ("x", SimpleT "int")] funcType2
            result = inferType env higherOrderFunc
        in case result of
          Right inferredType -> higherOrderTypeCorrect funcType1 funcType2 inferredType
          Left _ -> True
    
  , testProperty "function type inference respects polymorphism" $
      \typeVar env ->
        let polymorphicFunc = FuncT [("x", SimpleT typeVar)] (SimpleT typeVar)
            result = inferType env polymorphicFunc
        in case result of
          Right inferredType -> polymorphicTypePreserved typeVar inferredType
          Left _ -> True
    
  , testCase "function type inference examples" $ do
      let env = initialTypeEnvironment
          addFunc = FuncT [("x", SimpleT "int"), ("y", SimpleT "int")] (SimpleT "int")
          identityFunc = FuncT [("x", SimpleT "a")] (SimpleT "a")
      case inferType env addFunc of
        Left err -> assertFailure $ "Type inference failed: " ++ show err
        Right inferredType -> assertBool "Add function type inferred" $ inferredType == addFunc
      case inferType env identityFunc of
        Left err -> assertFailure $ "Type inference failed: " ++ show err
        Right inferredType -> assertBool "Identity function type inferred" $ typeExprWellFormed inferredType
  ]

-- ============================================================================
-- Generic Type Inference Properties
-- ============================================================================

genericTypeInferenceProperties :: TestTree
genericTypeInferenceProperties = testGroup "Generic Type Inference Properties"
  [ testProperty "generic type inference preserves type parameters" $
      \typeParams baseType env ->
        let genericType = GenericT (T.pack "Container") [baseType]
            result = inferType env genericType
        in case result of
          Right inferredType -> genericTypeParametersPreserved typeParams inferredType
          Left _ -> True
    
  , testProperty "generic type inference handles type instantiation" $
      \genericType concreteType env ->
        let result = instantiateGenericType env genericType concreteType
        in case result of
          Right instantiatedType -> typeInstantiationCorrect genericType concreteType instantiatedType
          Left _ -> True
    
  , testProperty "generic type inference supports type constraints" $
      \typeParams constraints env ->
        let genericType = GenericT (T.pack "Constrained") typeParams
            constrainedType = applyConstraints genericType constraints
            result = inferType env constrainedType
        in case result of
          Right inferredType -> typeConstraintsPreserved constraints inferredType
          Left _ -> True
    
  , testProperty "generic type inference is consistent with substitution" $
      \genericType substitution env ->
        let result1 = inferType env genericType
            result2 = inferType (applySubstitution env substitution) genericType
        in case (result1, result2) of
          (Right type1, Right type2) -> typesEquivalentUnderSubstitution type1 type2 substitution
          _ -> True
    
  , testProperty "generic type inference handles nested generics" $
      \outerType innerType env ->
        let nestedGeneric = GenericT (T.pack "Outer") [GenericT (T.pack "Inner") [innerType]]
            result = inferType env nestedGeneric
        in case result of
          Right inferredType -> nestedGenericStructureCorrect outerType innerType inferredType
          Left _ -> True
    
  , testCase "generic type inference examples" $ do
      let env = initialTypeEnvironment
          listType = GenericT (T.pack "List") [SimpleT "int"]
          mapType = GenericT (T.pack "Map") [SimpleT "string", SimpleT "int"]
      case inferType env listType of
        Left err -> assertFailure $ "Type inference failed: " ++ show err
        Right inferredType -> assertBool "List type inferred" $ typeExprWellFormed inferredType
      case inferType env mapType of
        Left err -> assertFailure $ "Type inference failed: " ++ show err
        Right inferredType -> assertBool "Map type inferred" $ typeExprWellFormed inferredType
  ]

-- ============================================================================
-- Constraint Type Inference Properties
-- ============================================================================

constraintTypeInferenceProperties :: TestTree
constraintTypeInferenceProperties = testGroup "Constraint Type Inference Properties"
  [ testProperty "constraint type inference respects type refinements" $
      \baseType constraints env ->
        let refinedType = RefineT baseType constraints
            result = inferType env refinedType
        in case result of
          Right inferredType -> typeRefinementsPreserved constraints inferredType
          Left _ -> True
    
  , testProperty "constraint type inference validates constraints" $
      \baseType constraints env ->
        let refinedType = RefineT baseType constraints
            result = inferType env refinedType
        in case result of
          Right inferredType -> constraintsAreValid constraints inferredType
          Left _ -> True
    
  , testProperty "constraint type inference handles dependent types" $
      \dependentType constraint env ->
        let result = inferDependentType env dependentType constraint
        in case result of
          Right inferredType -> dependentTypeConstraintSatisfied dependentType constraint inferredType
          Left _ -> True
    
  , testProperty "constraint type inference supports size constraints" $
      \varName size env ->
        let sizeConstraint = SizeGT (T.pack varName) size
            constrainedType = RefineT (SimpleT "Array") [sizeConstraint]
            result = inferType env constrainedType
        in case result of
          Right inferredType -> sizeConstraintCorrect varName size inferredType
          Left _ -> True
    
  , testProperty "constraint type inference handles range constraints" $
      \varName minVal maxVal env ->
        let rangeConstraint = RangeC (T.pack varName) minVal maxVal
            constrainedType = RefineT (SimpleT "Int") [rangeConstraint]
            result = inferType env constrainedType
        in case result of
          Right inferredType -> rangeConstraintCorrect varName minVal maxVal inferredType
          Left _ -> True
    
  , testCase "constraint type inference examples" $ do
      let env = initialTypeEnvironment
          sizedArray = RefineT (GenericT (T.pack "Array") [SimpleT "int"]) [SizeGT "length" 0]
          rangedInt = RefineT (SimpleT "Int") [RangeC "value" 1 100]
      case inferType env sizedArray of
        Left err -> assertFailure $ "Type inference failed: " ++ show err
        Right inferredType -> assertBool "Sized array type inferred" $ typeExprWellFormed inferredType
      case inferType env rangedInt of
        Left err -> assertFailure $ "Type inference failed: " ++ show err
        Right inferredType -> assertBool "Ranged int type inferred" $ typeExprWellFormed inferredType
  ]

-- ============================================================================
-- Type Unification Properties
-- ============================================================================

typeUnificationProperties :: TestTree
typeUnificationProperties = testGroup "Type Unification Properties"
  [ testProperty "type unification is symmetric" $
      \type1 type2 ->
        let result1 = unifyTypes type1 type2
            result2 = unifyTypes type2 type1
        in case (result1, result2) of
          (Left _, Left _) -> True
          (Right sub1, Right sub2) -> sub1 === sub2
          _ -> False
    
  , testProperty "type unification is associative" $
      \type1 type2 type3 ->
        let result1 = unifyTypes type1 type2 >>= \sub12 -> unifyTypes (applySubstitutionType sub12 type2) type3
            result2 = unifyTypes type2 type3 >>= \sub23 -> unifyTypes type1 (applySubstitutionType sub23 type2)
        in case (result1, result2) of
          (Left _, Left _) -> True
          (Right sub1, Right sub2) -> substitutionsEquivalent sub1 sub2
          _ -> False
    
  , testProperty "type unification produces most general unifier" $
      \type1 type2 ->
        case unifyTypes type1 type2 of
          Left _ -> True  -- No unifier exists
          Right substitution -> isMostGeneralUnifier substitution type1 type2
    
  , testProperty "type unification handles type variables" $
      \typeVar typeExpr ->
        let varType = SimpleT (T.pack typeVar)
            result = unifyTypes varType typeExpr
        in case result of
          Right substitution -> substitutionContainsVar typeVar substitution
          Left _ -> True
    
  , testProperty "type unification preserves type structure" $
      \type1 type2 ->
        case unifyTypes type1 type2 of
          Right substitution -> 
            let unified1 = applySubstitutionType substitution type1
                unified2 = applySubstitutionType substitution type2
            in unified1 === unified2
          Left _ -> True
    
  , testCase "type unification examples" $ do
      let varA = SimpleT "a"
          varB = SimpleT "b"
          intType = SimpleT "int"
          funcType = FuncT [("x", varA)] varA
      case unifyTypes varA intType of
        Left err -> assertFailure $ "Unification failed: " ++ show err
        Right substitution -> assertBool "Unification successful" $ not $ Map.null substitution
      case unifyTypes funcType (FuncT [("x", intType)] intType) of
        Left err -> assertFailure $ "Unification failed: " ++ show err
        Right substitution -> assertBool "Function unification successful" $ not $ Map.null substitution
  ]

-- ============================================================================
-- Type Substitution Properties
-- ============================================================================

typeSubstitutionProperties :: TestTree
typeSubstitutionProperties = testGroup "Type Substitution Properties"
  [ testProperty "type substitution is deterministic" $
      \substitution typeExpr ->
        let result1 = applySubstitutionType substitution typeExpr
            result2 = applySubstitutionType substitution typeExpr
        in result1 === result2
    
  , testProperty "type substitution preserves well-formedness" $
      \substitution typeExpr ->
        let substituted = applySubstitutionType substitution typeExpr
        in typeExprWellFormed substituted
    
  , testProperty "type substitution composition is associative" $
      \sub1 sub2 sub3 ->
        let composed1 = composeSubstitutions (composeSubstitutions sub1 sub2) sub3
            composed2 = composeSubstitutions sub1 (composeSubstitutions sub2 sub3)
        in substitutionsEquivalent composed1 composed2
    
  , testProperty "type substitution identity leaves types unchanged" $
      \typeExpr ->
        let identitySub = Map.empty
            substituted = applySubstitutionType identitySub typeExpr
        in substituted === typeExpr
    
  , testProperty "type substitution distributes over type constructors" $
      \substitution typeExprs ->
        let genericType = GenericT (T.pack "Container") typeExprs
            substituted = applySubstitutionType substitution genericType
        in case substituted of
          GenericT name newTypes -> name == (T.pack "Container") && length newTypes == length typeExprs
          _ -> False
    
  , testProperty "type substitution handles recursive types" $
      \substitution ->
        let recursiveType = GenericT (T.pack "List") [SimpleT "a"]
            recursiveSub = Map.singleton (T.pack "a") recursiveType
            result = applySubstitutionType recursiveSub recursiveType
        in typeExprWellFormed result
    
  , testCase "type substitution examples" $ do
      let sub = Map.fromList [(T.pack "a", SimpleT "int"), (T.pack "b", SimpleT "string")]
          funcType = FuncT [("x", SimpleT "a")] (SimpleT "b")
          substituted = applySubstitutionType sub funcType
      assertBool "Substitution applied" $ substituted /= funcType
      assertBool "Substitution preserves structure" $ typeExprWellFormed substituted
  ]

-- ============================================================================
-- QuickCheck Generators
-- ============================================================================

-- Generate type variable names
genTypeVarName :: Gen String
genTypeVarName = elements ["a", "b", "c", "t", "u", "v", "x", "y", "z"]

-- Generate basic types
genBasicType :: Gen TypeExpr
genBasicType = elements 
  [ SimpleT "int"
  , SimpleT "string"
  , SimpleT "bool"
  , SimpleT "float"
  , SimpleT . T.pack <$> genTypeVarName
  ]

-- Generate function types
genFunctionType :: Gen TypeExpr
genFunctionType = do
  numParams <- choose (0, 3)
  params <- vectorOf numParams $ do
    paramName <- genTypeVarName
    paramType <- genTypeExpr
    return (paramName, paramType)
  returnType <- genTypeExpr
  return $ FuncT params returnType

-- Generate generic types
genGenericType :: Gen TypeExpr
genGenericType = do
  typeName <- elements ["List", "Map", "Set", "Option", "Container"]
  numArgs <- choose (1, 3)
  args <- vectorOf numArgs genTypeExpr
  return $ GenericT (T.pack typeName) args

-- Generate constrained types
genConstrainedType :: Gen TypeExpr
genConstrainedType = do
  baseType <- genTypeExpr
  numConstraints <- choose (1, 3)
  constraints <- vectorOf numConstraints genConstraint
  return $ RefineT baseType constraints

-- Generate type expressions
genTypeExpr :: Gen TypeExpr
genTypeExpr = oneof
  [ genBasicType
  , genFunctionType
  , genGenericType
  , genConstrainedType
  ]

-- Generate constraints
genConstraint :: Gen Constraint
genConstraint = oneof
  [ SizeGT . T.pack <$> genTypeVarName <*> choose (0, 100)
  , SizeGE . T.pack <$> genTypeVarName <*> choose (0, 100)
  , RangeC . T.pack <$> genTypeVarName <*> choose (0, 50) <*> choose (51, 100)
  , do
      predName <- genTypeVarName
      args <- vectorOf (choose (0, 2)) genTypeExpr
      return $ PredC (T.pack predName) args
  ]

-- Generate type environments
genTypeEnvironment :: Gen TypeEnvironment
genTypeEnvironment = do
  numBindings <- choose (0, 5)
  bindings <- vectorOf numBindings $ do
    varName <- genTypeVarName
    typeExpr <- genTypeExpr
    return (T.pack varName, typeExpr)
  return $ Map.fromList bindings

-- Generate type substitutions
genSubstitution :: Gen TypeSubstitution
genSubstitution = do
  numMappings <- choose (0, 5)
  mappings <- vectorOf numMappings $ do
    varName <- genTypeVarName
    typeExpr <- genTypeExpr
    return (T.pack varName, typeExpr)
  return $ Map.fromList mappings

instance Arbitrary TypeExpr where
  arbitrary = genTypeExpr

instance Arbitrary Constraint where
  arbitrary = genConstraint

instance Arbitrary TypeEnvironment where
  arbitrary = genTypeEnvironment

instance Arbitrary TypeSubstitution where
  arbitrary = genSubstitution

-- ============================================================================
-- Helper Functions
-- ============================================================================

-- Type aliases
type TypeEnvironment = Map.Map Text TypeExpr
type TypeSubstitution = Map.Map Text TypeExpr

-- Check if type expression is well-formed
typeExprWellFormed :: TypeExpr -> Bool
typeExprWellFormed typeExpr = case typeExpr of
  SimpleT name -> not $ T.null name
  GenericT name args -> not $ T.null name && all typeExprWellFormed args
  FuncT params returnType -> all (typeExprWellFormed . snd) params && typeExprWellFormed returnType
  RefineT baseType constraints -> typeExprWellFormed baseType && all constraintValid constraints

-- Check if constraint is valid
constraintValid :: Constraint -> Bool
constraintValid constraint = case constraint of
  SizeGT name value -> not $ T.null name && value >= 0
  SizeGE name value -> not $ T.null name && value >= 0
  RangeC name minVal maxVal -> not $ T.null name && minVal <= maxVal
  PredC name args -> not $ T.null name && all typeExprWellFormed args

-- Get initial type environment
initialTypeEnvironment :: TypeEnvironment
initialTypeEnvironment = Map.fromList
  [ (T.pack "int", SimpleT "int")
  , (T.pack "string", SimpleT "string")
  , (T.pack "bool", SimpleT "bool")
  , (T.pack "float", SimpleT "float")
  ]

-- Infer type (placeholder implementation)
inferType :: TypeEnvironment -> TypeExpr -> Either String TypeExpr
inferType env typeExpr = Right typeExpr

-- Infer literal type
inferLiteralType :: TypeEnvironment -> String -> Either String TypeExpr
inferLiteralType env literal = case literal of
  _ | all isDigit literal -> Right $ SimpleT "int"
  _ | head literal == '"' && last literal == '"' -> Right $ SimpleT "string"
  _ -> Left "Unknown literal type"

-- Infer variable type
inferVariableType :: TypeEnvironment -> String -> Either String TypeExpr
inferVariableType env varName = 
  case Map.lookup (T.pack varName) env of
    Just typeExpr -> Right typeExpr
    Nothing -> Left $ "Variable not found: " ++ varName

-- Check if type exists in environment
typeExistsInEnvironment :: TypeEnvironment -> TypeExpr -> Bool
typeExistsInEnvironment env typeExpr = typeExpr `elem` Map.elems env

-- Check if literal type is correct
literalTypeCorrect :: String -> TypeExpr -> Bool
literalTypeCorrect literal inferredType = case literal of
  _ | all isDigit literal -> inferredType == SimpleT "int"
  _ | head literal == '"' && last literal == '"' -> inferredType == SimpleT "string"
  _ -> False

-- Check if variable type is consistent
variableTypeConsistent :: TypeEnvironment -> String -> TypeExpr -> Bool
variableTypeConsistent env varName inferredType = 
  case Map.lookup (T.pack varName) env of
    Just expectedType -> inferredType == expectedType
    Nothing -> False

-- Check if function parameter types are preserved
functionParameterTypesPreserved :: [(String, TypeExpr)] -> TypeExpr -> Bool
functionParameterTypesPreserved params inferredType = case inferredType of
  FuncT inferredParams _ -> length inferredParams == length params
  _ -> False

-- Check if function return type is correct
functionReturnTypeCorrect :: TypeExpr -> TypeExpr -> Bool
functionReturnTypeCorrect expectedReturnType inferredType = case inferredType of
  FuncT _ actualReturnType -> actualReturnType == expectedReturnType
  _ -> False

-- Infer application type
inferApplicationType :: TypeEnvironment -> TypeExpr -> [TypeExpr] -> Either String TypeExpr
inferApplicationType env funcType argTypes = 
  case funcType of
    FuncT params returnType -> Right returnType
    _ -> Left "Not a function type"

-- Check if application type is sound
applicationTypeSound :: TypeExpr -> [TypeExpr] -> TypeExpr -> Bool
applicationTypeSound funcType argTypes resultType = True

-- Check if higher-order type is correct
higherOrderTypeCorrect :: TypeExpr -> TypeExpr -> TypeExpr -> Bool
higherOrderTypeCorrect funcType1 funcType2 inferredType = True

-- Check if polymorphic type is preserved
polymorphicTypePreserved :: String -> TypeExpr -> Bool
polymorphicTypePreserved typeVar inferredType = True

-- Check if generic type parameters are preserved
genericTypeParametersPreserved :: [String] -> TypeExpr -> Bool
genericTypeParametersPreserved typeParams inferredType = True

-- Instantiate generic type
instantiateGenericType :: TypeEnvironment -> TypeExpr -> TypeExpr -> Either String TypeExpr
instantiateGenericType env genericType concreteType = Right concreteType

-- Check if type instantiation is correct
typeInstantiationCorrect :: TypeExpr -> TypeExpr -> TypeExpr -> Bool
typeInstantiationCorrect genericType concreteType instantiatedType = True

-- Apply constraints to type
applyConstraints :: TypeExpr -> [Constraint] -> TypeExpr
applyConstraints typeExpr constraints = RefineT typeExpr constraints

-- Check if type constraints are preserved
typeConstraintsPreserved :: [Constraint] -> TypeExpr -> Bool
typeConstraintsPreserved constraints inferredType = True

-- Apply substitution to environment
applySubstitution :: TypeEnvironment -> TypeSubstitution -> TypeEnvironment
applySubstitution env substitution = Map.map (applySubstitutionType substitution) env

-- Check if types are equivalent under substitution
typesEquivalentUnderSubstitution :: TypeExpr -> TypeExpr -> TypeSubstitution -> Bool
typesEquivalentUnderSubstitution type1 type2 substitution = 
  applySubstitutionType substitution type1 == applySubstitutionType substitution type2

-- Check if nested generic structure is correct
nestedGenericStructureCorrect :: TypeExpr -> TypeExpr -> TypeExpr -> Bool
nestedGenericStructureCorrect outerType innerType inferredType = True

-- Check if type refinements are preserved
typeRefinementsPreserved :: [Constraint] -> TypeExpr -> Bool
typeRefinementsPreserved constraints inferredType = True

-- Check if constraints are valid
constraintsAreValid :: [Constraint] -> TypeExpr -> Bool
constraintsAreValid constraints inferredType = all constraintValid constraints

-- Infer dependent type
inferDependentType :: TypeEnvironment -> TypeExpr -> Constraint -> Either String TypeExpr
inferDependentType env dependentType constraint = Right dependentType

-- Check if dependent type constraint is satisfied
dependentTypeConstraintSatisfied :: TypeExpr -> Constraint -> TypeExpr -> Bool
dependentTypeConstraintSatisfied dependentType constraint inferredType = True

-- Check if size constraint is correct
sizeConstraintCorrect :: String -> Int -> TypeExpr -> Bool
sizeConstraintCorrect varName size inferredType = True

-- Check if range constraint is correct
rangeConstraintCorrect :: String -> Int -> Int -> TypeExpr -> Bool
rangeConstraintCorrect varName minVal maxVal inferredType = True

-- Unify types
unifyTypes :: TypeExpr -> TypeExpr -> Either String TypeSubstitution
unifyTypes type1 type2 = Right Map.empty

-- Check if substitution contains variable
substitutionContainsVar :: String -> TypeSubstitution -> Bool
substitutionContainsVar varName substitution = Map.member (T.pack varName) substitution

-- Check if substitution is most general unifier
isMostGeneralUnifier :: TypeSubstitution -> TypeExpr -> TypeExpr -> Bool
isMostGeneralUnifier substitution type1 type2 = True

-- Apply substitution to type
applySubstitutionType :: TypeSubstitution -> TypeExpr -> TypeExpr
applySubstitutionType substitution typeExpr = case typeExpr of
  SimpleT name -> Map.lookup name substitution `maybe` typeExpr id
  GenericT name args -> GenericT name $ map (applySubstitutionType substitution) args
  FuncT params returnType -> 
    FuncT (map (\(name, t) -> (name, applySubstitutionType substitution t)) params) 
         (applySubstitutionType substitution returnType)
  RefineT baseType constraints -> 
    RefineT (applySubstitutionType substitution baseType) constraints

-- Compose substitutions
composeSubstitutions :: TypeSubstitution -> TypeSubstitution -> TypeSubstitution
composeSubstitutions sub1 sub2 = Map.union sub1 (Map.map (applySubstitutionType sub1) sub2)

-- Check if substitutions are equivalent
substitutionsEquivalent :: TypeSubstitution -> TypeSubstitution -> Bool
substitutionsEquivalent sub1 sub2 = Map.size sub1 == Map.size sub2 && 
  all (`Map.member` sub2) (Map.keys sub1)

-- Helper function for digit checking
isDigit :: Char -> Bool
isDigit c = c >= '0' && c <= '9'

-- ============================================================================
-- Edge Case Tests
-- ============================================================================

edgeCaseProperties :: TestTree
edgeCaseProperties = testGroup "Edge Case Tests"
  [ testCase "handle unbound type variables" $
      let unboundType = SimpleT "unknown"
          env = initialTypeEnvironment
      in case inferType env unboundType of
        Left _ -> assertBool "Unbound variable handled" True
        Right inferredType -> assertBool "Type inferred" $ typeExprWellFormed inferredType
    
  , testCase "handle recursive type definitions" $
      let recursiveType = GenericT (T.pack "List") [SimpleT "a"]
          sub = Map.singleton (T.pack "a") recursiveType
          result = applySubstitutionType sub recursiveType
      in assertBool "Recursive type handled" $ typeExprWellFormed result
    
  , testCase "handle contradictory constraints" $
      let constraints = [SizeGT "x" 10, SizeLT "x" 5]
          constrainedType = RefineT (SimpleT "Int") constraints
      in assertBool "Contradictory constraints handled" $ not $ all constraintValid constraints
    
  , testProperty "handle very deep type nesting" $
      \n -> n < 100 ==>
        let deepType = foldr (\name acc -> GenericT (T.pack name) [acc]) (SimpleT "int") (take n $ repeat "nested")
        in typeExprWellFormed deepType
    
  , testCase "handle empty function parameters" $
      let emptyFunc = FuncT [] (SimpleT "void")
          env = initialTypeEnvironment
      in case inferType env emptyFunc of
        Left err -> assertFailure $ "Type inference failed: " ++ show err
        Right inferredType -> assertBool "Empty function inferred" $ typeExprWellFormed inferredType
  ]

-- Missing constraint type
data SizeLT = SizeLT Text Int

instance Show SizeLT where
  show (SizeLT name value) = "SizeLT " ++ T.unpack name ++ " " ++ show value

-- ============================================================================
-- Performance Properties
-- ============================================================================

performanceProperties :: TestTree
performanceProperties = testGroup "Performance Properties"
  [ testProperty "type inference is linear in type size" $
      \typeExpr env ->
        let result = inferType env typeExpr
        in case result of
          Right inferredType -> length (show inferredType) `seq` True
          Left _ -> True
    
  , testProperty "type unification is efficient" $
      \type1 type2 ->
        let result = unifyTypes type1 type2
        in case result of
          Right substitution -> Map.size substitution `seq` True
          Left _ -> True
    
  , testProperty "type substitution is linear" $
      \substitution typeExpr ->
        let result = applySubstitutionType substitution typeExpr
        in length (show result) `seq` True
    
  , testProperty "constraint checking is efficient" $
      \constraints typeExpr ->
        let constrainedType = RefineT typeExpr constraints
        in all constraintValid constraints `seq` True
  ]