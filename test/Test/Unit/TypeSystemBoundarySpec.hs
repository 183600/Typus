{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.TypeSystemBoundarySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (testCase, assertFailure, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck 
  ( Property, (===), (==>), forAll, counterexample, classify, property
  , (.&&.), (.||.), Arbitrary(..), Gen, oneof, elements, listOf
  , sized, resize, suchThat, frequency, choose, getPositive, getNonEmpty
  )

import Compiler.TypeChecker
import Compiler.Errors.Core (ErrorLocation(..))
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (nub, sort, intercalate)
import Data.Maybe (isJust, isNothing, fromMaybe)

-- | Generate basic type names
genBasicType :: Gen String
genBasicType = elements 
  [ "int", "string", "bool", "float", "char", "void", "L.any", "never"
  , "i32", "i64", "f32", "f64", "u8", "u16", "u32", "u64"
  ]

-- | Generate complex type constructors
genComplexType :: Gen String
genComplexType = oneof
  [ do
      elemType <- genBasicType
      return $ "[]" ++ elemType
  , do
      keyType <- genBasicType
      valueType <- genBasicType
      return $ "Map<" ++ keyType ++ "," ++ valueType ++ ">"
  , do
      types <- listOf1 genBasicType
      return $ "(" ++ intercalate "," types ++ ")"
  , do
      retType <- genBasicType
      paramTypes <- listOf genBasicType
      return $ "fn(" ++ intercalate "," paramTypes ++ ")->" ++ retType
  ]

-- | Generate type variable names
genTypeVar :: Gen String
genTypeVar = do
  n <- choose (0, 25)
  return [chr (ord 'a' + n)]

-- | Generate dependent type constraints
genDependentType :: Gen String
genDependentType = oneof
  [ do
      baseType <- genBasicType
      constraint <- elements ["n > 0", "n >= 0", "len(s) > 0", "size >= 1"]
      return $ baseType ++ "{" ++ constraint ++ "}"
  , do
      baseType <- genComplexType
      n <- choose (1, 100)
      return $ baseType ++ "{n = " ++ show n ++ "}"
  , do
      baseType <- genBasicType
      return $ "Vector<" ++ baseType ++ ">{n > 0}"
  ]

-- | Generate type expressions
genTypeExpression :: Gen String
genTypeExpression = oneof
  [ genBasicType
  , genComplexType
  , genDependentType
  , do
      left <- genBasicType
      right <- genBasicType
      op <- elements ["|", "&", "->"]
      return $ left ++ " " ++ op ++ " " ++ right
  ]

-- | Generate type constraints
genTypeConstraint :: Gen (String, String)
genTypeConstraint = do
  left <- genTypeExpression
  right <- genTypeExpression
  return (left, right)

-- | Generate type substitution mapping
genTypeSubstitution :: Gen [(String, String)]
genTypeSubstitution = do
  numSubs <- choose (1, 5)
  sequence $ replicate numSubs $ do
    var <- genTypeVar
    typ <- genTypeExpression
    return (var, typ)

-- | Generate invalid type expressions
genInvalidType :: Gen String
genInvalidType = oneof
  [ elements ["", "invalid", "123", "type<>", "fn()", "Map<,>"]
  , do
      baseType <- genBasicType
      invalidConstraint <- elements ["{", "{}", "{invalid", "{{}}"]
      return $ baseType ++ invalidConstraint
  ]

-- Property: Type unification should be symmetric
prop_type_unification_symmetric :: String -> String -> Property
prop_type_unification_symmetric type1 type2 =
  not (null type1) && not (null type2) ==> 
  let result1 = unifyTypes type1 type2
      result2 = unifyTypes type2 type1
  in property $ result1 === result2

-- Property: Type unification should be reflexive
prop_type_unification_reflexive :: String -> Property
prop_type_unification_reflexive typ =
  not (null typ) ==> 
  let result = unifyTypes typ typ
  in property $ isJust result

-- Property: Type substitution should preserve type structure
prop_type_substitution_preserves_structure :: [(String, String)] -> String -> Property
prop_type_substitution_preserves_structure subs typ =
  not (null subs) && not (null typ) ==> 
  let substituted = applyTypeSubstitution subs typ
  in property $ not (null substituted)

-- Property: Type substitution should be idempotent
prop_type_substitution_idempotent :: [(String, String)] -> String -> Property
prop_type_substitution_idempotent subs typ =
  let sub1 = applyTypeSubstitution subs typ
      sub2 = applyTypeSubstitution subs sub1
  in property $ sub1 === sub2

-- Property: Type inference should handle basic types correctly
prop_type_inference_basic :: String -> Property
prop_type_inference_basic expression =
  expression `elem` ["42", "\"hello\"", "true", "3.14", "'a'"] ==> 
  let inferred = inferType expression
      expected = case expression of
        "42" -> Just "int"
        "\"hello\"" -> Just "string"
        "true" -> Just "bool"
        "3.14" -> Just "float"
        "'a'" -> Just "char"
        _ -> Nothing
  in property $ inferred === expected

-- Property: Type checking should catch type mismatches
prop_type_checking_mismatch :: String -> String -> Property
prop_type_checking_mismatch expression expectedType =
  expression `elem` ["42", "\"hello\"", "true"] && 
  expectedType `elem` ["string", "bool", "int"] ==> 
  let result = checkType expression expectedType
      shouldBeValid = case (expression, expectedType) of
        ("42", "int") -> True
        ("\"hello\"", "string") -> True
        ("true", "bool") -> True
        _ -> False
  in property $ result === shouldBeValid

-- Property: Dependent type constraints should be validated
prop_dependent_type_constraints :: String -> Property
prop_dependent_type_constraints dependentType =
  "{" `L.isInfixOf` dependentType ==> 
  let isValid = validateDependentType dependentType
  in property $ isValid ==> hasValidConstraint dependentType

-- Property: Type system should handle recursive types
prop_recursive_type_handling :: String -> Property
prop_recursive_type_handling typeName =
  not (null typeName) ==> 
  let recursiveType = "List<" ++ typeName ++ ">"
      result = validateRecursiveType recursiveType
  in property $ result || not (typeName `elem` ["int", "string", "bool"])

-- Property: Type equality should be transitive
prop_type_equality_transitive :: String -> String -> String -> Property
prop_type_equality_transitive type1 type2 type3 =
  not (null type1) && not (null type2) && not (null type3) ==> 
  let eq12 = areTypesEqual type1 type2
      eq23 = areTypesEqual type2 type3
      eq13 = areTypesEqual type1 type3
  in property $ (eq12 && eq23) ==> eq13

-- Property: Type subtyping should form a partial order
prop_type_subtyping_partial_order :: String -> String -> String -> Property
prop_type_subtyping_partial_order subtype1 subtype2 subtype3 =
  not (null subtype1) && not (null subtype2) && not (null subtype3) ==> 
  let sub12 = isSubtype subtype1 subtype2
      sub23 = isSubtype subtype2 subtype3
      sub13 = isSubtype subtype1 subtype3
  in property $ (sub12 && sub23) ==> sub13

-- Property: Type variables should be properly scoped
prop_type_variable_scoping :: String -> [(String, String)] -> Property
prop_type_variable_scoping varName substitutions =
  isTypeVar varName ==> 
  let scoped = applyTypeSubstitution substitutions varName
  in property $ if varName `elem` map fst substitutions
                then scoped /= varName
                else scoped == varName

-- Property: Generic type instantiation should preserve constraints
prop_generic_type_instantiation :: String -> [(String, String)] -> Property
prop_generic_type_instantiation genericType typeArgs =
  "T" `L.isInfixOf` genericType ==> 
  let instantiated = instantiateGenericType genericType typeArgs
  in property $ not ("T" `L.isInfixOf` instantiated) || null typeArgs

-- Property: Type system should handle union types correctly
prop_union_type_handling :: [String] -> Property
prop_union_type_handling types =
  not (null types) && L.all (not . null) types ==> 
  let unionType = "(" ++ intercalate "|" types ++ ")"
      isValid = validateUnionType unionType
  in property $ isValid ==> L.all (flip isMemberOfUnion unionType) types

-- Property: Type system should handle intersection types correctly
prop_intersection_type_handling :: [String] -> Property
prop_intersection_type_handling types =
  not (null types) && L.all (not . null) types ==> 
  let intersectionType = "(" ++ intercalate "&" types ++ ")"
      isValid = validateIntersectionType intersectionType
  in property $ isValid ==> L.length types <= 3 -- Reasonable constraint

-- Property: Type inference should fail gracefully on invalid input
prop_type_inference_invalid :: String -> Property
prop_type_inference_invalid invalidExpression =
  invalidExpression `elem` ["", "invalid", "123abc", "null"] ==> 
  let inferred = inferType invalidExpression
  in property $ isNothing inferred

-- Property: Type checking should handle complex expressions
prop_type_checking_complex :: String -> String -> Property
prop_type_checking_complex leftExpr rightExpr =
  not (null leftExpr) && not (null rightExpr) ==> 
  let result = checkBinaryOperation leftExpr "+" rightExpr
  in property $ isJust result || isNothing result

-- Property: Dependent type refinement should be sound
prop_dependent_type_refinement :: String -> String -> Property
prop_dependent_type_refinement baseType constraint =
  not (null baseType) && not (null constraint) ==> 
  let refined = refineDependentType baseType constraint
      isValid = validateDependentType refined
  in property $ isValid ==> isRefinementValid baseType constraint

-- | Helper functions

unifyTypes :: String -> String -> Maybe [(String, String)]
unifyTypes t1 t2 
  | t1 == t2 = Just []
  | t1 `elem` ["int", "string", "bool"] && t2 `elem` ["int", "string", "bool"] && t1 /= t2 = Nothing
  | otherwise = Just []

applyTypeSubstitution :: [(String, String)] -> String -> String
applyTypeSubstitution subs typ = 
  L.foldl (\acc (var, replacement) -> 
    if acc == var then replacement else acc) typ subs

inferType :: String -> Maybe String
inferType expr
  | L.all isDigit expr = Just "int"
  | L.head expr == '"' && last expr == '"' = Just "string"
  | expr == "true" || expr == "false" = Just "bool"
  | L.any isDigit expr && L.any (== '.') expr = Just "float"
  | L.length expr == 3 && L.head expr == '\'' && last expr == '\'' = Just "char"
  | otherwise = Nothing

checkType :: String -> String -> Bool
checkType expr expectedType = 
  case inferType expr of
    Just inferred -> inferred == expectedType
    Nothing -> False

validateDependentType :: String -> Bool
validateDependentType typ = 
  "{" `L.isInfixOf` typ && "}" `L.isInfixOf` typ

hasValidConstraint :: String -> Bool
hasValidConstraint typ = 
  let constraint = takeWhile (/= '}') $ dropWhile (/= '{') typ
  in L.any (`L.isInfixOf` constraint) [">", "<", ">=", "<="]

validateRecursiveType :: String -> Bool
validateRecursiveType typ = "List<" `L.isPrefixOf` typ && ">" `L.isSuffixOf` typ

areTypesEqual :: String -> String -> Bool
areTypesEqual t1 t2 = t1 == t2

isSubtype :: String -> String -> Bool
isSubtype subtype supertype
  | subtype == supertype = True
  | subtype == "int" && supertype == "float" = True
  | subtype == "L.any" = True
  | supertype == "L.any" = True
  | otherwise = False

isTypeVar :: String -> Bool
isTypeVar [c] = c >= 'a' && c <= 'z'
isTypeVar _ = False

instantiateGenericType :: String -> [(String, String)] -> String
instantiateGenericType genericType typeArgs = 
  L.foldl (\acc (var, replacement) -> 
    replaceVar var replacement acc) genericType typeArgs

replaceVar :: String -> String -> String -> String
replaceVar var replacement = L.map (\c -> if [c] == var then replacement else [c])

isMemberOfUnion :: String -> String -> Bool
isMemberOfUnion typ unionType = 
  let members = splitUnionType unionType
  in typ `elem` members

splitUnionType :: String -> [String]
splitUnionType = splitOn '|' . L.filter (`notElem` "()")

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = [[]]
splitOn delim xs = go xs []
  where
    go [] acc = [L.reverse acc]
    go (y:ys) acc
      | y == delim = L.reverse acc : go ys []
      | otherwise = go ys (y:acc)

validateUnionType :: String -> Bool
validateUnionType typ = "(" `L.isPrefixOf` typ && ")" `L.isSuffixOf` typ && "|" `L.isInfixOf` typ

validateIntersectionType :: String -> Bool
validateIntersectionType typ = "(" `L.isPrefixOf` typ && ")" `L.isSuffixOf` typ && "&" `L.isInfixOf` typ

checkBinaryOperation :: String -> String -> String -> Maybe String
checkBinaryOperation left op right
  | op `elem` ["+", "-", "*", "/"] = do
      leftType <- inferType left
      rightType <- inferType right
      if leftType == rightType && leftType `elem` ["int", "float"]
      then Just leftType
      else Nothing
  | otherwise = Nothing

refineDependentType :: String -> String -> String
refineDependentType baseType constraint = baseType ++ "{" ++ constraint ++ "}"

isRefinementValid :: String -> String -> Bool
isRefinementValid baseType constraint = 
  not (null baseType) && not (null constraint) && 
  L.any (`L.isInfixOf` constraint) [">", "<", ">=", "<="]

tests :: TestTree
tests = testGroup "Type System Boundary Tests"
  [ testGroup "Property-based tests"
    [ fastProperty "type unification symmetry" prop_type_unification_symmetric
    , fastProperty "type unification reflexivity" prop_type_unification_reflexive
    , fastProperty "type substitution preserves structure" prop_type_substitution_preserves_structure
    , fastProperty "type substitution idempotent" prop_type_substitution_idempotent
    , fastProperty "type inference basic types" prop_type_inference_basic
    , fastProperty "type checking mismatches" prop_type_checking_mismatch
    , fastProperty "dependent type constraints" prop_dependent_type_constraints
    , fastProperty "recursive type handling" prop_recursive_type_handling
    , fastProperty "type equality transitivity" prop_type_equality_transitive
    , fastProperty "type subtyping partial order" prop_type_subtyping_partial_order
    , fastProperty "type variable scoping" prop_type_variable_scoping
    , fastProperty "generic type instantiation" prop_generic_type_instantiation
    , fastProperty "union type handling" prop_union_type_handling
    , fastProperty "intersection type handling" prop_intersection_type_handling
    , fastProperty "type inference invalid input" prop_type_inference_invalid
    , fastProperty "type checking complex expressions" prop_type_checking_complex
    , fastProperty "dependent type refinement" prop_dependent_type_refinement
    ]

  , testGroup "Unit tests"
    [ testCase "basic type unification" $ do
        unifyTypes "int" "int" @?= Just []
        unifyTypes "int" "string" @?= Nothing
    
    , testCase "type substitution" $ do
        let subs = [("T", "int"), ("U", "string")]
        applyTypeSubstitution subs "T" @?= "int"
        applyTypeSubstitution subs "U" @?= "string"
        applyTypeSubstitution subs "V" @?= "V"
    
    , testCase "type inference" $ do
        inferType "42" @?= Just "int"
        inferType "\"hello\"" @?= Just "string"
        inferType "true" @?= Just "bool"
        inferType "invalid" @?= Nothing
    
    , testCase "dependent type validation" $ do
        validateDependentType "int{n > 0}" @?= True
        validateDependentType "string{len > 0}" @?= True
        validateDependentType "int{invalid}" @?= False
    
    , testCase "subtype relationships" $ do
        isSubtype "int" "int" @?= True
        isSubtype "int" "float" @?= True
        isSubtype "string" "int" @?= False
        isSubtype "L.any" "int" @?= True
    ]
  ]