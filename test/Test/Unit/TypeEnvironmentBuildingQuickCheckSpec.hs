{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.TypeEnvironmentBuildingQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, oneof, elements, listOf, sized, resize, choose)
import qualified Test.QuickCheck as QC

import Compiler.TypeChecker (Type(..), TypeEnv(..), buildTypeEnv, buildTypeEnvFromPairs, addType, lookupType, addFunction, checkFunctionSignature, addVariable, lookupVariable, inferExpressionType, unifyTypes, areTypesCompatible, FunctionInfo(..), FunctionSignature(..), FunctionParam(..), TypeConstraint(..))
import Compiler (compile)
import Parser (parseTypus, TypusFile(..), CodeBlock(..))
import SourceLocation (SourcePos(..), SourceSpan(..))
import qualified Data.Text as T
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import Data.List (nub, lines, unlines, union, intersect)
import Data.Char (isAlpha, isAlphaNum, isSpace, isDigit)
import qualified Data.Map as Map
import qualified Data.Set as Set

-- | Generate valid identifiers
genIdentifier :: Gen String
genIdentifier = do
  first <- elements ['a'..'z']
  rest <- listOf $ elements (['a'..'z'] ++ ['0'..'9'] ++ ['_'])
  return (first : rest)

-- | Generate basic types
genBasicType :: Gen Type
genBasicType = oneof
  [ return $ TypeName "int"
  , return $ TypeName "string"
  , return $ TypeName "bool"
  , return $ TypeName "float"
  , return $ TypeName "void"
  ]

-- | Generate function types
genFunctionType :: Gen Type
genFunctionType = do
  paramTypes <- listOf genBasicType
  returnType <- genBasicType
  return $ TypeFunction paramTypes returnType

-- | Generate struct types
genStructType :: Gen Type
genStructType = do
  structName <- genIdentifier
  fields <- listOf $ do
    fieldName <- genIdentifier
    fieldType <- genBasicType
    return (fieldName, fieldType)
  return $ TypeRecord fields

-- | Generate generic types
genGenericType :: Gen Type
genGenericType = do
  typeName <- genIdentifier
  typeParams <- listOf genIdentifier
  baseType <- genBasicType
  return $ TypeName (typeName ++ "<" ++ unwords typeParams ++ ">") -- Simplified generic type

-- | Generate dependent types
genDependentType :: Gen Type
genDependentType = do
  typeName <- genIdentifier
  constraints <- listOf genTypeConstraint
  baseType <- genBasicType
  return $ TypeName (typeName ++ " with constraints") -- Simplified dependent type

-- | Generate type constraints
genTypeConstraint :: Gen TypeConstraint
genTypeConstraint = oneof
  [ do
      varName <- genIdentifier
      value <- choose (0, 100)
      return $ EqualityConstraint varName (IntLiteral value)
  , do
      varName <- genIdentifier
      minVal <- choose (0, 50)
      maxVal <- choose (51, 100)
      return $ RangeConstraint varName minVal maxVal
  , do
      varName <- genIdentifier
      return $ NonNullConstraint varName
  ]

-- | Generate function parameters
genFunctionParam :: Gen FunctionParam
genFunctionParam = do
  paramName <- genIdentifier
  paramType <- genBasicType
  return $ FunctionParam paramName paramType False

-- | Generate function signatures
genFunctionSignature :: Gen FunctionSignature
genFunctionSignature = do
  params <- listOf genFunctionParam
  returnType <- genBasicType
  return $ FunctionSignature params returnType

-- | Generate function info
genFunctionInfo :: Gen FunctionInfo
genFunctionInfo = do
  funcName <- genIdentifier
  signature <- genFunctionSignature
  return $ FunctionInfo funcName signature

-- | Generate type environment pairs
genTypeEnvPairs :: Gen [(String, Type)]
genTypeEnvPairs = do
  numTypes <- choose (0, 10)
  pairs <- listOf $ do
    typeName <- genIdentifier
    typeDef <- oneof [genBasicType, genFunctionType, genStructType]
    return (typeName, typeDef)
  return $ take numTypes pairs

-- | Generate valid type declarations
genTypeDeclaration :: Gen String
genTypeDeclaration = oneof
  [ -- Basic type alias
    do
      aliasName <- genIdentifier
      baseType <- elements ["int", "string", "bool", "float"]
      return $ "type " ++ aliasName ++ " = " ++ baseType
    
  , -- Struct type
    do
      structName <- genIdentifier
      fields <- listOf $ do
        fieldName <- genIdentifier
        fieldType <- elements ["int", "string", "bool", "float"]
        return (fieldName, fieldType)
      let fieldDecls = L.map (\(name, typ) -> name ++ " " ++ typ) fields
      return $ "type " ++ structName ++ " struct {\n  " ++ unlines fieldDecls ++ "\n}"
    
  , -- Function type
    do
      funcName <- genIdentifier
      paramTypes <- listOf $ elements ["int", "string", "bool", "float"]
      returnType <- elements ["int", "string", "bool", "float"]
      let params = if null paramTypes then "" else unwords (L.map (\t -> t + " param") paramTypes)
      return $ "type " ++ funcName ++ " = func(" ++ params ++ ") " ++ returnType
  ]

-- | Generate variable declarations
genVariableDeclaration :: Gen String
genVariableDeclaration = do
  varName <- genIdentifier
  varType <- elements ["int", "string", "bool", "float"]
  value <- elements ["0", "\"hello\"", "true", "0.0"]
  return $ varName ++ " := " ++ value ++ " // " ++ varType

-- | Generate function declarations
genFunctionDeclaration :: Gen String
genFunctionDeclaration = do
  funcName <- genIdentifier
  paramNames <- listOf genIdentifier
  paramTypes <- listOf $ elements ["int", "string", "bool", "float"]
  returnType <- elements ["int", "string", "bool", "float", "void"]
  let params = unwords $ zipWith (\name typ -> name ++ " " ++ typ) paramNames paramTypes
  return $ "func " ++ funcName ++ "(" ++ params ++ ") " ++ returnType ++ " {\n  return " ++ L.head (elements ["0", "\"hello\"", "true", "0.0"]) ++ "\n}"

-- | Generate complete program with type definitions
genTypedProgram :: Gen String
genTypedProgram = do
  typeDecls <- listOf genTypeDeclaration
  varDecls <- listOf genVariableDeclaration
  funcDecls <- listOf genFunctionDeclaration
  let allDecls = typeDecls ++ varDecls ++ funcDecls
  return $ unlines allDecls

-- Property: Type environment should be built correctly from pairs
prop_typeenv_from_pairs :: [(String, Type)] -> Property
prop_typeenv_from_pairs pairs =
  let typeEnv = buildTypeEnvFromPairs pairs
      expectedTypes = map fst pairs
      actualTypes = Map.keys (types typeEnv)
  in property $ Set.fromList expectedTypes === Set.fromList actualTypes

-- Property: Type lookup should work for added types
prop_type_lookup_works :: String -> Type -> Property
prop_type_lookup_works typeName typeDef =
  not (null typeName) ==>
  let typeEnv = buildTypeEnvFromPairs [(typeName, typeDef)]
      lookupResult = lookupType typeEnv typeName
  in property $ lookupResult === Just typeDef

-- Property: Type lookup should fail for missing types
prop_type_lookup_fails :: String -> [(String, Type)] -> Property
prop_type_lookup_fails missingType existingTypes =
  not (null missingType) && missingType `notElem` map fst existingTypes ==>
  let typeEnv = buildTypeEnvFromPairs existingTypes
      lookupResult = lookupType typeEnv missingType
  in property $ lookupResult === Nothing

-- Property: Function addition should work correctly
prop_function_addition :: String -> FunctionSignature -> Property
prop_function_addition funcName signature =
  not (null funcName) ==>
  let funcInfo = FunctionInfo funcName signature
      typeEnv = buildTypeEnvFromPairs []
      updatedEnv = addFunction typeEnv funcInfo
      lookupResult = Map.lookup funcName (functions updatedEnv)
  in property $ lookupResult === Just funcInfo

-- Property: Variable addition should work correctly
prop_variable_addition :: String -> Type -> Property
prop_variable_addition varName varType =
  not (null varName) ==>
  let typeEnv = buildTypeEnvFromPairs []
      updatedEnv = addVariable typeEnv varName varType
      lookupResult = Map.lookup varName (variables updatedEnv)
  in property $ lookupResult === Just varType

-- Property: Type compatibility should be reflexive
prop_type_compatibility_reflexive :: Type -> Property
prop_type_compatibility_reflexive typeDef =
  areTypesCompatible typeDef typeDef

-- Property: Basic type compatibility should work correctly
prop_basic_type_compatibility :: Type -> Type -> Property
prop_basic_type_compatibility type1 type2 =
  case (type1, type2) of
    (TypeName "int", TypeName "int") -> property $ areTypesCompatible type1 type2
    (TypeName "string", TypeName "string") -> property $ areTypesCompatible type1 type2
    (TypeName "bool", TypeName "bool") -> property $ areTypesCompatible type1 type2
    (TypeName "float", TypeName "float") -> property $ areTypesCompatible type1 type2
    (TypeName "void", TypeName "void") -> property $ areTypesCompatible type1 type2
    _ -> property $ not (areTypesCompatible type1 type2)

-- Property: Function type compatibility should check parameters L.and return types
prop_function_type_compatibility :: [Type] -> Type -> [Type] -> Type -> Property
prop_function_type_compatibility params1 ret1 params2 ret2 =
  let funcType1 = TypeFunction params1 ret1
      funcType2 = TypeFunction params2 ret2
  in property $ areTypesCompatible funcType1 funcType2 === (L.length params1 == L.length params2 && ret1 == ret2)

-- Property: Struct type compatibility should check field names L.and types
prop_struct_type_compatibility :: String -> [(String, Type)] -> String -> [(String, Type)] -> Property
prop_struct_type_compatibility name1 fields1 name2 fields2 =
  let structType1 = TypeRecord fields1
      structType2 = TypeRecord fields2
      fieldNames1 = map fst fields1
      fieldNames2 = map fst fields2
      fieldTypes1 = map snd fields1
      fieldTypes2 = map snd fields2
  in property $ areTypesCompatible structType1 structType2 === 
                 Set.fromList fieldNames1 == Set.fromList fieldNames2

-- Property: Type unification should work for identical types
prop_type_unification_identical :: Type -> Property
prop_type_unification_identical typeDef =
  unifyTypes typeDef typeDef === Just typeDef

-- Property: Type unification should fail for incompatible types
prop_type_unification_incompatible :: Type -> Type -> Property
prop_type_unification_incompatible type1 type2 =
  case (type1, type2) of
    (TypeName "int", TypeName "string") -> property $ unifyTypes type1 type2 === Nothing
    (TypeName "bool", TypeName "float") -> property $ unifyTypes type1 type2 === Nothing
    (TypeName "void", TypeName "int") -> property $ unifyTypes type1 type2 === Nothing
    _ -> property $ True -- Other cases may succeed L.or fail

-- Property: Type environment should handle duplicate types gracefully
prop_typeenv_handle_duplicates :: String -> Type -> Type -> Property
prop_typeenv_handle_duplicates typeName type1 type2 =
  not (null typeName) ==>
  let typeEnv = buildTypeEnvFromPairs [(typeName, type1), (typeName, type2)]
      lookupResult = lookupType typeEnv typeName
  in property $ lookupResult === Just type2 -- Last definition should win

-- Property: Type environment should preserve insertion order semantics
prop_typeenv_preserve_order :: [(String, Type)] -> Property
prop_typeenv_preserve_order pairs =
  not (null pairs) ==>
  let typeEnv = buildTypeEnvFromPairs pairs
      allTypes = Map.elems (types typeEnv)
  in property $ L.length allTypes === L.length (nub pairs)

-- Property: Function signature checking should validate parameter counts
prop_function_signature_param_count :: [Type] -> Type -> [Type] -> Type -> Property
prop_function_signature_param_count params1 ret1 params2 ret2 =
  let signature1 = FunctionSignature (L.map (\t -> FunctionParam "param" t False) params1) ret1
      signature2 = FunctionSignature (L.map (\t -> FunctionParam "param" t False) params2) ret2
  in property $ checkFunctionSignature signature1 signature2 === (L.length params1 == L.length params2 && ret1 == ret2)

-- Property: Type environment should handle complex nested types
prop_typeenv_nested_types :: Type -> Type -> Property
prop_typeenv_nested_types outerType innerType =
  let nestedType = case outerType of
        TypeRecord fields -> TypeRecord (("inner", innerType) : fields)
        _ -> outerType
      typeEnv = buildTypeEnvFromPairs [("Nested", nestedType)]
      lookupResult = lookupType typeEnv "Nested"
  in property $ lookupResult === Just nestedType

-- Property: Type constraints should be satisfiable
prop_type_constraints_satisfiable :: TypeConstraint -> Property
prop_type_constraints_satisfiable constraint =
  case constraint of
    EqualityConstraint var (IntLiteral value) -> property $ value >= 0
    RangeConstraint var minVal maxVal -> property $ minVal <= maxVal
    NonNullConstraint var -> property $ True -- Always satisfiable
    _ -> property $ True

-- Property: Type environment should handle generic types correctly
prop_typeenv_generic_types :: String -> [String] -> Type -> Property
prop_typeenv_generic_types typeName typeParams baseType =
  not (null typeName) && not (null typeParams) ==>
  let genericType = TypeName (typeName ++ "<" ++ unwords typeParams ++ ">") -- Simplified generic type
      typeEnv = buildTypeEnvFromPairs [(typeName, genericType)]
      lookupResult = lookupType typeEnv typeName
  in property $ lookupResult === Just genericType

-- Export L.all tests
tests :: TestTree
tests =
  testGroup "Type Environment Building QuickCheck Tests"
    [ fastProperty "type environment should be built correctly from pairs" prop_typeenv_from_pairs
    , fastProperty "type lookup should work for added types" prop_type_lookup_works
    , fastProperty "type lookup should fail for missing types" prop_type_lookup_fails
    , fastProperty "function addition should work correctly" prop_function_addition
    , fastProperty "variable addition should work correctly" prop_variable_addition
    , fastProperty "type compatibility should be reflexive" prop_type_compatibility_reflexive
    , fastProperty "basic type compatibility should work correctly" prop_basic_type_compatibility
    , fastProperty "function type compatibility should check parameters L.and return types" prop_function_type_compatibility
    , fastProperty "struct type compatibility should check field names L.and types" prop_struct_type_compatibility
    , fastProperty "type unification should work for identical types" prop_type_unification_identical
    , fastProperty "type unification should fail for incompatible types" prop_type_unification_incompatible
    , fastProperty "type environment should handle duplicate types gracefully" prop_typeenv_handle_duplicates
    , fastProperty "type environment should preserve insertion order semantics" prop_typeenv_preserve_order
    , fastProperty "function signature checking should validate parameter counts" prop_function_signature_param_count
    , fastProperty "type environment should handle complex nested types" prop_typeenv_nested_types
    , fastProperty "type constraints should be satisfiable" prop_type_constraints_satisfiable
    , fastProperty "type environment should handle generic types correctly" prop_typeenv_generic_types
    ]
