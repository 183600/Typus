{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Test.Unit.NewTypeSystemQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import TestSupport.Arbitrary

import Compiler.TypeChecker
  ( Type(..)
  , TypeEnv(..)
  , FunctionParam(..)
  , FunctionSignature(..)
  , TypeError(..)
  , buildTypeEnvFromPairs
  , addType
  , lookupType
  , addFunction
  , checkFunctionSignature
  , addVariable
  , lookupVariable
  , inferExpressionType
  , unifyTypes
  , substituteType
  , instantiateGeneric
  , areTypesCompatible
  , checkFunctionParameters
  , inferFunctionReturnType
  , validateRecursiveType
  , checkInterfaceImplementation
  , canCoerce
  , isSubtype
  , typesEqual
  , constructHigherKindedType
  , computeTypeLevel
  , validateDependentType
  , TypeConstraint(..)
  , applyConstraints
  , satisfiesConstraints
  , UnknownType
  , TypeName
  , TypeFunction
  , TypeRecord
  , TypeUnion
  )

import Compiler.GoAst (GoModule(..), ImportDecl(..), GoDecl(..), FuncDecl(..), PackageDecl(..))
import Parser (TypusFile(..), CodeBlock(..))
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (intercalate, isInfixOf, isPrefixOf)
import Data.Char (isAlphaNum, isDigit)

-- | 新的类型系统QuickCheck测试套件
tests :: TestTree
tests =
  testGroup "New Type System QuickCheck Tests"
    [ fastProperty "buildTypeEnvFromPairs creates correct environment" prop_buildTypeEnvFromPairs_correct
    , fastProperty "addType and lookupType are consistent" prop_addType_lookupType_consistent
    , fastProperty "addVariable and lookupVariable are consistent" prop_addVariable_lookupVariable_consistent
    , fastProperty "areTypesCompatible reflexive property" prop_areTypesCompatible_reflexive
    , fastProperty "areTypesCompatible symmetric for basic types" prop_areTypesCompatible_symmetric_basic
    , fastProperty "unifyTypes handles identical types" prop_unifyTypes_identical
    , fastProperty "typesEqual is reflexive" prop_typesEqual_reflexive
    , fastProperty "typesEqual is symmetric" prop_typesEqual_symmetric
    , fastProperty "isSubtype reflexive property" prop_isSubtype_reflexive
    , fastProperty "constructHigherKindedType preserves structure" prop_constructHigherKindedType_preserves
    , fastProperty "computeTypeLevel handles nested types" prop_computeTypeLevel_nested
    , fastProperty "applyConstraints preserves valid constraints" prop_applyConstraints_preserves
    , fastProperty "satisfiesConstraints handles empty constraints" prop_satisfiesConstraints_empty
    ]

-- Property: buildTypeEnvFromPairs creates correct environment
prop_buildTypeEnvFromPairs_correct :: [(String, Type)] -> Property
prop_buildTypeEnvFromPairs_correct pairs =
  not (null pairs) && length pairs <= 10 ==>
  let env = buildTypeEnvFromPairs pairs
      expectedVarTypes = Map.fromList pairs
  in property $ varTypes env === expectedVarTypes .&&.
     not (Map.null (functionTypes env))  -- Should have builtin functions

-- Property: addType and lookupType are consistent
prop_addType_lookupType_consistent :: String -> Type -> Property
prop_addType_lookupType_consistent typeName typ =
  not (null typeName) && length typeName <= 20 ==>
  let env = buildTypeEnvFromPairs []
      updatedEnv = addType typeName typ env
  in property $ lookupType updatedEnv typeName === Just typ

-- Property: addVariable and lookupVariable are consistent
prop_addVariable_lookupVariable_consistent :: String -> Type -> Property
prop_addVariable_lookupVariable_consistent varName varType =
  not (null varName) && length varName <= 20 ==>
  let env = buildTypeEnvFromPairs []
      updatedEnv = addVariable varName varType env
  in property $ lookupVariable updatedEnv varName === Just varType

-- Property: areTypesCompatible reflexive property
prop_areTypesCompatible_reflexive :: Type -> Property
prop_areTypesCompatible_reflexive typ =
  property $ areTypesCompatible typ typ

-- Property: areTypesCompatible symmetric for basic types
prop_areTypesCompatible_symmetric_basic :: Type -> Type -> Property
prop_areTypesCompatible_symmetric_basic typ1 typ2 =
  let compat1 = areTypesCompatible typ1 typ2
      compat2 = areTypesCompatible typ2 typ1
  in property $ compat1 === compat2

-- Property: unifyTypes handles identical types
prop_unifyTypes_identical :: Type -> Property
prop_unifyTypes_identical typ =
  let result = unifyTypes typ typ
  in property $ result === Just typ

-- Property: typesEqual is reflexive
prop_typesEqual_reflexive :: Type -> Property
prop_typesEqual_reflexive typ =
  property $ typesEqual typ typ

-- Property: typesEqual is symmetric
prop_typesEqual_symmetric :: Type -> Type -> Property
prop_typesEqual_symmetric typ1 typ2 =
  let equal1 = typesEqual typ1 typ2
      equal2 = typesEqual typ2 typ1
  in property $ equal1 === equal2

-- Property: isSubtype reflexive property
prop_isSubtype_reflexive :: Type -> Property
prop_isSubtype_reflexive typ =
  property $ isSubtype typ typ

-- Property: constructHigherKindedType preserves structure
prop_constructHigherKindedType_preserves :: [Type] -> Property
prop_constructHigherKindedType_preserves typeArgs =
  not (null typeArgs) && length typeArgs <= 5 ==>
  let higherKinded = constructHigherKindedType "Container" typeArgs
      level = computeTypeLevel higherKinded
  in property $ level >= length typeArgs

-- Property: computeTypeLevel handles nested types
prop_computeTypeLevel_nested :: Type -> Type -> Property
prop_computeTypeLevel_nested typ1 typ2 =
  let functionType = TypeFunction [typ1] typ2
      level = computeTypeLevel functionType
      level1 = computeTypeLevel typ1
      level2 = computeTypeLevel typ2
  in property $ level >= max level1 level2 .&&. level >= 1

-- Property: applyConstraints preserves valid constraints
prop_applyConstraints_preserves :: Type -> [TypeConstraint] -> Property
prop_applyConstraints_preserves typ constraints =
  let constrained = applyConstraints constraints typ
  in property $ not (null constrained) ==> constrained === typ

-- Property: satisfiesConstraints handles empty constraints
prop_satisfiesConstraints_empty :: Type -> Property
prop_satisfiesConstraints_empty typ =
  let constraints = []
  in property $ satisfiesConstraints constraints typ

-- Additional properties for type system

-- Property: inferExpressionType handles literals
prop_inferExpressionType_literals :: String -> Property
prop_inferExpressionType_literals literal =
  isStringLiteral literal || isNumericLiteral literal ==>
  let env = buildTypeEnvFromPairs []
      inferredType = inferExpressionType env literal
  in property $ inferredType /= UnknownType

-- Property: checkFunctionParameters validates parameter count
prop_checkFunctionParameters_count :: [Type] -> [Type] -> Property
prop_checkFunctionParameters_count paramTypes argTypes =
  let signature = FunctionSignature
        { fsParams = map (\t -> FunctionParam Nothing t False) paramTypes
        , fsReturns = []
        }
      env = buildTypeEnvFromPairs []
      result = checkFunctionParameters env signature argTypes
  in property $ (length paramTypes == length argTypes) ==> null result

-- Property: substituteType handles type variables
prop_substituteType_variables :: String -> Type -> Type -> Property
prop_substituteType_variables varName replacement originalType =
  not (null varName) ==>
  let substitution = [(varName, replacement)]
      result = substituteType substitution originalType
  in property $ not (null result)

-- Property: instantiateGeneric creates concrete types
prop_instantiateGeneric_concrete :: Type -> Property
prop_instantiateGeneric_concrete genericType =
  let result = instantiateGeneric genericType
  in property $ not (null result)

-- Property: canCoerce handles compatible types
prop_canCoerce_compatible :: Type -> Type -> Property
prop_canCoerce_compatible sourceType targetType =
  areTypesCompatible sourceType targetType ==>
  let result = canCoerce sourceType targetType
  in property $ result

-- Property: validateRecursiveType detects valid recursive types
prop_validateRecursiveType_valid :: String -> Property
prop_validateRecursiveType_valid typeName =
  not (null typeName) && length typeName <= 20 ==>
  let recursiveType = TypeName typeName
      result = validateRecursiveType recursiveType
  in property $ result  -- All simple types should be valid

-- Property: checkInterfaceImplementation handles simple cases
prop_checkInterfaceImplementation_simple :: Type -> Type -> Property
prop_checkInterfaceImplementation_simple interfaceType implementationType =
  let result = checkInterfaceImplementation interfaceType implementationType
  in property $ not (null result) ==> True  -- Should return some result

-- Property: TypeFunction creation preserves parameter and return types
prop_typeFunction_preserves :: [Type] -> Type -> Property
prop_typeFunction_preserves paramTypes returnType =
  not (null paramTypes) && length paramTypes <= 5 ==>
  let functionType = TypeFunction paramTypes returnType
  in property $ case functionType of
        TypeFunction params ret -> params === paramTypes .&&. ret === returnType
        _ -> property False

-- Property: TypeRecord creation preserves field types
prop_typeRecord_preserves :: [(String, Type)] -> Property
prop_typeRecord_preserves fields =
  not (null fields) && length fields <= 5 ==>
  let recordType = TypeRecord fields
  in property $ case recordType of
        TypeRecord recordFields -> recordFields === fields
        _ -> property False

-- Property: TypeUnion creation preserves variant types
prop_typeUnion_preserves :: [Type] -> Property
prop_typeUnion_preserves variantTypes =
  not (null variantTypes) && length variantTypes <= 5 ==>
  let unionType = TypeUnion variantTypes
  in property $ case unionType of
        TypeUnion variants -> variants === variantTypes
        _ -> property False

-- Helper functions for property testing

-- Check if a string represents a string literal
isStringLiteral :: String -> Bool
isStringLiteral s = length s >= 2 && 
                   head s == '"' && 
                   last s == '"' &&
                   not (any (`elem` "\"\n\r\t") (init (tail s)))

-- Check if a string represents a numeric literal
isNumericLiteral :: String -> Bool
isNumericLiteral s = case s of
    [] -> False
    '-':rest -> all isDigit rest
    _ -> all isDigit s

-- Property: inferFunctionReturnType handles simple cases
prop_inferFunctionReturnType_simple :: Type -> Property
prop_inferFunctionReturnType_simple returnType =
  let signature = FunctionSignature
        { fsParams = []
        , fsReturns = [returnType]
        }
      env = buildTypeEnvFromPairs []
      result = inferFunctionReturnType env signature
  in property $ result === Just returnType

-- Property: TypeConstraint application maintains consistency
prop_typeConstraint_consistency :: Type -> TypeConstraint -> Property
prop_typeConstraint_consistency typ constraint =
  let constrained = applyConstraints [constraint] typ
  in property $ satisfiesConstraints [constraint] constrained

-- Property: Complex type construction preserves properties
prop_complex_type_preserves :: [[Type]] -> Property
prop_complex_type_preserves typeGroups =
  not (null typeGroups) && all (not . null) typeGroups && length typeGroups <= 3 ==>
  let complexTypes = map TypeUnion typeGroups
      levels = map computeTypeLevel complexTypes
  in property $ all (> 0) levels

-- Property: Type environment merging preserves all entries
prop_typeEnv_merge :: [(String, Type)] -> [(String, Type)] -> Property
prop_typeEnv_merge pairs1 pairs2 =
  not (null pairs1) && not (null pairs2) &&
  length pairs1 <= 5 && length pairs2 <= 5 ==>
  let env1 = buildTypeEnvFromPairs pairs1
      env2 = buildTypeEnvFromPairs pairs2
      mergedEnv = TypeEnv
        { varTypes = Map.union (varTypes env1) (varTypes env2)
        , functionTypes = Map.union (functionTypes env1) (functionTypes env2)
        }
  in property $ Map.size (varTypes mergedEnv) >= max (Map.size (varTypes env1)) (Map.size (varTypes env2))

-- Property: Type variable substitution handles nested structures
prop_substitution_nested :: String -> Type -> Type -> Property
prop_substitution_nested varName replacement complexType =
  not (null varName) ==>
  let nestedType = TypeFunction [TypeName varName, complexType] (TypeName varName)
      substitution = [(varName, replacement)]
      result = substituteType substitution nestedType
  in property $ not (null result)

-- Property: Constraint satisfaction is monotonic
prop_constraint_monotonic :: Type -> [TypeConstraint] -> [TypeConstraint] -> Property
prop_constraint_monotonic typ constraints1 constraints2 =
  let satisfied1 = satisfiesConstraints constraints1 typ
      satisfied2 = satisfiesConstraints (constraints1 ++ constraints2) typ
  in property $ satisfied2 ==> satisfied1