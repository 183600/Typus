{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.AdditionalDependentTypesQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, oneof, elements, listOf, choose)
import TestSupport.Arbitrary

import DependentTypesParser
  ( TypeRef(..)
  , TypeBody(..)
  , Field(..)
  , TypeParameter(..)
  , TypeConstraint(..)
  , DependentType(..)
  , DependentTypeError(..)
  )

import Data.List (sort, nub)
import Data.Maybe (isJust, isNothing)

-- ============================================================================
-- Additional QuickCheck Tests for DependentTypes Module
-- ============================================================================

-- Property: TypeRef equality consistency
prop_type_ref_equality_consistent :: String -> [String] -> String -> [String] -> Property
prop_type_ref_equality_consistent name1 args1 name2 args2 =
  let typeRef1 = TypeRef name1 args1
      typeRef2 = TypeRef name2 args2
  in property $ (name1 == name2 && args1 == args2) ==> (typeRef1 == typeRef2)

-- Property: TypeRef inequality
prop_type_ref_inequality :: String -> [String] -> String -> [String] -> Property
prop_type_ref_inequality name1 args1 name2 args2 =
  let typeRef1 = TypeRef name1 args1
      typeRef2 = TypeRef name2 args2
  in property $ (name1 /= name2 || args1 /= args2) ==> (typeRef1 /= typeRef2)

-- Property: TypeRef with no args
prop_type_ref_no_args :: String -> Property
prop_type_ref_no_args name =
  let typeRef = TypeRef name []
  in property $ refName typeRef === name .&&. refArgs typeRef === []

-- Property: TypeRef with nested args
prop_type_ref_nested_args :: String -> [String] -> [String] -> Property
prop_type_ref_nested_args name args1 args2 =
  let nestedType1 = TypeRef name args1
      nestedType2 = TypeRef "Nested" args2
      topLevel = TypeRef "Top" [nestedType1, nestedType2]
  in property $ refName topLevel === "Top" .&&.
     L.length (refArgs topLevel) === 2 .&&.
     refArgs topLevel !! 0 === nestedType1 .&&.
     refArgs topLevel !! 1 === nestedType2

-- Property: Field equality consistency
prop_field_equality_consistent :: String -> String -> String -> [String] -> String -> [String] -> Property
prop_field_equality_consistent name1 type1 args1 name2 type2 args2 =
  let field1 = Field name1 (TypeRef type1 args1)
      field2 = Field name2 (TypeRef type2 args2)
  in property $ (name1 == name2 && type1 == type2 && args1 == args2) ==> (field1 == field2)

-- Property: Field inequality
prop_field_inequality :: String -> String -> String -> [String] -> String -> [String] -> Property
prop_field_inequality name1 type1 args1 name2 type2 args2 =
  let field1 = Field name1 (TypeRef type1 args1)
      field2 = Field name2 (TypeRef type2 args2)
  in property $ (name1 /= name2 || type1 /= type2 || args1 /= args2) ==> (field1 /= field2)

-- Property: TypeBody equality for StructBody
prop_type_body_struct_equality :: [String] -> [String] -> [String] -> [String] -> Property
prop_type_body_struct_equality names1 types1 names2 types2 =
  let fields1 = zipWith (\n t -> Field n (TypeRef t [])) names1 types1
      fields2 = zipWith (\n t -> Field n (TypeRef t [])) names2 types2
      body1 = StructBody fields1
      body2 = StructBody fields2
  in property $ (names1 == names2 && types1 == types2) ==> (body1 == body2)

-- Property: TypeParameter equality consistency
prop_type_parameter_equality_consistent :: String -> String -> [String] -> [String] -> String -> [String] -> [String] -> [String] -> Property
prop_type_parameter_equality_consistent name1 type1 args1 constraints1 name2 type2 args2 constraints2 =
  let param1 = TypeParameter name1 (TypeRef type1 args1) constraints1
      param2 = TypeParameter name2 (TypeRef type2 args2) constraints2
  in property $ (name1 == name2 && type1 == type2 && args1 == args2 && constraints1 == constraints2) ==> (param1 == param2)

-- Property: TypeConstraint equality for different constraint types
prop_type_constraint_equality :: String -> String -> String -> Int -> Int -> String -> Int -> [String] -> Property
prop_type_constraint_equality var1 var2 var3 minVal maxVal var4 sizeVal preds =
  let equality = EqualityConstraint var1 var2
      inequality = InequalityConstraint var1 var2
      range = RangeConstraint var3 minVal maxVal
      size = SizeConstraint var4 sizeVal
      nonempty = NonEmptyConstraint var1
      predicate = PredicateConstraint var1 preds
  in property $ equality /= inequality .&&.
     equality /= range .&&.
     equality /= size .&&.
     equality /= nonempty .&&.
     equality /= predicate

-- Property: DependentType equality for TypeDecl
prop_dependent_type_type_decl_equality :: String -> [String] -> [String] -> [String] -> String -> [String] -> [String] -> [String] -> Property
prop_dependent_type_type_decl_equality name1 paramNames1 paramTypes1 fieldNames1 fieldTypes1 name2 paramNames2 paramTypes2 fieldNames2 fieldTypes2 =
  let params1 = zipWith (\n t -> TypeParameter n (TypeRef t []) []) paramNames1 paramTypes1
      params2 = zipWith (\n t -> TypeParameter n (TypeRef t []) []) paramNames2 paramTypes2
      fields1 = zipWith (\n t -> Field n (TypeRef t [])) fieldNames1 fieldTypes1
      fields2 = zipWith (\n t -> Field n (TypeRef t [])) fieldNames2 fieldTypes2
      type1 = TypeDecl name1 params1 (StructBody fields1) []
      type2 = TypeDecl name2 params2 (StructBody fields2) []
  in property $ (name1 == name2 && paramNames1 == paramNames2 && paramTypes1 == paramTypes2 && fieldNames1 == fieldNames2 && fieldTypes1 == fieldTypes2) ==> (type1 == type2)

-- Property: DependentType equality for DependentFunction
prop_dependent_type_function_equality :: String -> [String] -> [String] -> String -> [String] -> String -> [String] -> [String] -> [String] -> Property
prop_dependent_type_function_equality name1 paramNames1 paramTypes1 retType1 name2 paramNames2 paramTypes2 retType2 =
  let params1 = zipWith (\n t -> (n, TypeRef t [])) paramNames1 paramTypes1
      params2 = zipWith (\n t -> (n, TypeRef t [])) paramNames2 paramTypes2
      func1 = DependentFunction name1 params1 (TypeRef retType1 []) []
      func2 = DependentFunction name2 params2 (TypeRef retType2 []) []
  in property $ (name1 == name2 && paramNames1 == paramNames2 && paramTypes1 == paramTypes2 && retType1 == retType2) ==> (func1 == func2)

-- Property: DependentType equality for TypeAlias
prop_dependent_type_alias_equality :: String -> String -> [String] -> String -> [String] -> [String] -> Property
prop_dependent_type_alias_equality name1 type1 args1 name2 type2 args2 =
  let alias1 = TypeAlias name1 (TypeRef type1 args1) []
      alias2 = TypeAlias name2 (TypeRef type2 args2) []
  in property $ (name1 == name2 && type1 == type2 && args1 == args2) ==> (alias1 == alias2)

-- Property: DependentTypeError equality consistency
prop_dependent_type_error_equality :: String -> String -> String -> String -> Property
prop_dependent_type_error_equality msg1 file1 msg2 file2 =
  let syntax1 = SyntaxError msg1 0 file1
      syntax2 = SyntaxError msg2 0 file2
      invalid1 = InvalidTypeSyntax msg1
      invalid2 = InvalidTypeSyntax msg2
  in property $ (msg1 == msg2 && file1 == file2) ==> (syntax1 == syntax2) .&&.
     (msg1 == msg2) ==> (invalid1 == invalid2)

-- Property: Complex nested TypeRef
prop_complex_nested_type_ref :: String -> [[String]] -> Property
prop_complex_nested_type_ref baseName argLists =
  let buildNestedType name [] = TypeRef name []
      buildNestedType name (args:rest) = TypeRef name [buildNestedType "Nested" rest]
      complexType = buildNestedType baseName argLists
  in property $ refName complexType === baseName

-- Property: TypeRef sorting preserves structure
prop_type_ref_sorting_preserves_structure :: [TypeRef] -> Property
prop_type_ref_sorting_preserves_structure typeRefs =
  let sorted = sort typeRefs
      names = map refName sorted
      originalNames = map refName typeRefs
  in property $ sort originalNames === names

-- Property: Field sorting preserves order by name
prop_field_sorting_by_name :: [String] -> [String] -> Property
prop_field_sorting_by_name names types =
  let fields = zipWith (\n t -> Field n (TypeRef t [])) names types
      sorted = sort fields
      sortedNames = map fieldName sorted
  in property $ sort names === sortedNames

-- Property: TypeConstraint with special characters
prop_type_constraint_special_characters :: String -> String -> Property
prop_type_constraint_special_characters var1 var2 =
  let specialVar1 = var1 ++ "!@#$%^&*()"
      specialVar2 = var2 ++ "!@#$%^&*()"
      equality = EqualityConstraint specialVar1 specialVar2
      inequality = InequalityConstraint specialVar1 specialVar2
      nonempty = NonEmptyConstraint specialVar1
  in property $ show equality `contains` specialVar1 .&&.
     show equality `contains` specialVar2 .&&.
     show inequality `contains` specialVar1 .&&.
     show inequality `contains` specialVar2 .&&.
     show nonempty `contains` specialVar1

-- Helper function to check substring containment
contains :: String -> String -> Bool
contains sub str = sub `L.isInfixOf` str

-- Property: DependentType with empty parameters
prop_dependent_type_empty_params :: String -> Property
prop_dependent_type_empty_params name =
  let typeDecl = TypeDecl name [] (StructBody []) []
      func = DependentFunction name [] (TypeRef "Int" []) []
      alias = TypeAlias name (TypeRef "Int") []
  in property $ show typeDecl `contains` name .&&.
     show func `contains` name .&&.
     show alias `contains` name

-- Property: Complex dependent type with multiple constraints
prop_complex_dependent_type_multiple_constraints :: String -> [String] -> [String] -> Property
prop_complex_dependent_type_multiple_constraints name paramNames fieldNames =
  not (null paramNames) && not (null fieldNames) ==> 
  let params = zipWith (\n -> TypeParameter n (TypeRef "Int") []) paramNames (repeat [])
      fields = zipWith (\n -> Field n (TypeRef "String")) fieldNames
      constraints = [EqualityConstraint "x" "y", SizeConstraint "arr" 10, NonEmptyConstraint "list"]
      complexType = TypeDecl name params (StructBody fields) constraints
  in property $ show complexType `contains` name .&&.
     L.length (params complexType) === L.length paramNames .&&.
     case typeBody complexType of
       StructBody fs -> L.length fs === L.length fieldNames
       _ -> property False

-- Property: TypeRef roundtrip through show/read (conceptual)
prop_type_ref_conceptual_roundtrip :: TypeRef -> Property
prop_type_ref_conceptual_roundtrip typeRef =
  let str = show typeRef
      expectedPrefix = "TypeRef {refName = \"" ++ refName typeRef ++ "\""
  in property $ str `startsWith` expectedPrefix

-- Helper function to check string prefix
startsWith :: String -> String -> Bool
startsWith prefix str = take (L.length prefix) str == prefix

-- Property: TypeConstraint creation consistency
prop_type_constraint_creation_consistency :: String -> String -> Int -> Int -> Int -> [String] -> Property
prop_type_constraint_creation_consistency var1 var2 minVal maxVal sizeVal preds =
  let equality = EqualityConstraint var1 var2
      inequality = InequalityConstraint var1 var2
      range = RangeConstraint var1 minVal maxVal
      size = SizeConstraint var1 sizeVal
      nonempty = NonEmptyConstraint var1
      predicate = PredicateConstraint var1 preds
  in property $ show equality /= show inequality .&&.
     show equality /= show range .&&.
     show equality /= show size .&&.
     show equality /= show nonempty .&&.
     show inequality /= show range .&&.
     show inequality /= show size .&&.
     show inequality /= show nonempty

-- Property: DependentTypeError with different error types
prop_dependent_type_error_different_types :: String -> Int -> String -> String -> String -> String -> Property
prop_dependent_type_error_different_types msg line snippet var param constraint =
  let syntax = SyntaxError msg line snippet
      invalid = InvalidTypeSyntax msg
      missing = MissingConstraint var
      invalidParam = InvalidParameter param
      constraintParse = ConstraintParseError constraint
  in property $ syntax /= invalid .&&.
     syntax /= missing .&&.
     syntax /= invalidParam .&&.
     syntax /= constraintParse .&&.
     invalid /= missing .&&.
     invalid /= invalidParam .&&.
     invalid /= constraintParse .&&.
     missing /= invalidParam .&&.
     missing /= constraintParse .&&.
     invalidParam /= constraintParse

-- ============================================================================
-- Test Collection
-- ============================================================================

tests :: TestTree
tests = testGroup "Additional DependentTypes QuickCheck Tests"
  [ fastProperty "TypeRef equality consistency" prop_type_ref_equality_consistent
  , fastProperty "TypeRef inequality" prop_type_ref_inequality
  , fastProperty "TypeRef with no args" prop_type_ref_no_args
  , fastProperty "TypeRef with nested args" prop_type_ref_nested_args
  , fastProperty "Field equality consistency" prop_field_equality_consistent
  , fastProperty "Field inequality" prop_field_inequality
  , fastProperty "TypeBody equality for StructBody" prop_type_body_struct_equality
  , fastProperty "TypeParameter equality consistency" prop_type_parameter_equality_consistent
  , fastProperty "TypeConstraint equality for different constraint types" prop_type_constraint_equality
  , fastProperty "DependentType equality for TypeDecl" prop_dependent_type_type_decl_equality
  , fastProperty "DependentType equality for DependentFunction" prop_dependent_type_function_equality
  , fastProperty "DependentType equality for TypeAlias" prop_dependent_type_alias_equality
  , fastProperty "DependentTypeError equality consistency" prop_dependent_type_error_equality
  , fastProperty "Complex nested TypeRef" prop_complex_nested_type_ref
  , fastProperty "TypeRef sorting preserves structure" prop_type_ref_sorting_preserves_structure
  , fastProperty "Field sorting preserves order by name" prop_field_sorting_by_name
  , fastProperty "TypeConstraint with special characters" prop_type_constraint_special_characters
  , fastProperty "DependentType with empty parameters" prop_dependent_type_empty_params
  , fastProperty "Complex dependent type with multiple constraints" prop_complex_dependent_type_multiple_constraints
  , fastProperty "TypeRef conceptual roundtrip" prop_type_ref_conceptual_roundtrip
  , fastProperty "TypeConstraint creation consistency" prop_type_constraint_creation_consistent
  , fastProperty "DependentTypeError with different error types" prop_dependent_type_error_different_types
  ]