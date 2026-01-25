{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Test.Unit.CompilerCoreFunctionalityTestSpec where



import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperties, Arbitrary(..), Gen, choose, listOf, elements, oneof, vectorOf, property, (===), forAll)
import Test.QuickCheck (Property, (==>))
import Compiler.TypeChecker (Type(..), FunctionParam(..), FunctionSignature(..), TypeCheckDiagnostic(..))

-- Helper generators
genTypeName :: Gen String
genTypeName = do
  first <- elements ['A'..'Z']
  rest <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']
  return (first : rest)

genVarName :: Gen String
genVarName = do
  first <- elements ['a'..'z']
  rest <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']
  return (first : rest)

genSimpleType :: Gen Type
genSimpleType = oneof
  [ TypeName <$> genTypeName
  , return UnknownType
  ]

genFunctionType :: Gen Type
genFunctionType = do
  paramCount <- choose (0, 3)
  paramTypes <- vectorOf paramCount genSimpleType
  returnType <- genSimpleType
  return $ TypeFunction paramTypes returnType

genRecordType :: Gen Type
genRecordType = do
  fieldCount <- choose (0, 3)
  fields <- vectorOf fieldCount $ do
    fieldName <- genVarName
    fieldType <- genSimpleType
    return (fieldName, fieldType)
  return $ TypeRecord fields

genUnionType :: Gen Type
genUnionType = do
  typeCount <- choose (2, 4)
  types <- vectorOf typeCount genSimpleType
  return $ TypeUnion types

instance Arbitrary Type where
  arbitrary = oneof
    [ genSimpleType
    , genFunctionType
    , genRecordType
    , genUnionType
    ]

genFunctionParam :: Gen FunctionParam
genFunctionParam = do
  useName <- arbitrary
  name <- if useName 
           then Just <$> genVarName
           else return Nothing
  paramType <- arbitrary
  variadic <- elements [False, True]
  return $ FunctionParam name paramType variadic

instance Arbitrary FunctionParam where
  arbitrary = genFunctionParam

genFunctionSignature :: Gen FunctionSignature
genFunctionSignature = do
  paramCount <- choose (0, 3)
  params <- vectorOf paramCount genFunctionParam
  returnCount <- choose (0, 2)
  returnTypes <- vectorOf returnCount genSimpleType
  return $ FunctionSignature params returnTypes

instance Arbitrary FunctionSignature where
  arbitrary = genFunctionSignature

-- Test properties for compiler core functionality

-- Property 1: Type equality is reflexive
prop_typeEqualityReflexive :: Type -> Property
prop_typeEqualityReflexive t = property $ t == t

-- Property 2: Function signature equality is reflexive
prop_functionSignatureEqualityReflexive :: FunctionSignature -> Property
prop_functionSignatureEqualityReflexive fs = property $ fs == fs

-- Property 3: Type names are preserved in TypeName constructor
prop_typeNamePreservation :: String -> Property
prop_typeNamePreservation name = 
  let t = TypeName name
  in property $ case t of
    TypeName n -> n == name
    _ -> False

-- Property 4: Function types have correct parameter count
prop_functionTypeParamCount :: [Type] -> Type -> Property
prop_functionTypeParamCount params returnType =
  let funcType = TypeFunction params returnType
  in property $ case funcType of
    TypeFunction ps rt -> length ps == length params && rt == returnType
    _ -> False

-- Property 5: Record types preserve field names
prop_recordTypeFieldNames :: [(String, Type)] -> Property
prop_recordTypeFieldNames fields =
  let recordType = TypeRecord fields
      fieldNames = map fst fields
  in property $ case recordType of
    TypeRecord fs -> map fst fs == fieldNames
    _ -> False

-- Property 6: Union types contain all variant types
prop_unionTypeContainsAllTypes :: [Type] -> Property
prop_unionTypeContainsAllTypes types =
  not (null types) ==> 
    let unionType = TypeUnion types
    in case unionType of
      TypeUnion ts -> all (`elem` ts) types
      _ -> False

-- Property 7: Function parameters preserve their names
prop_functionParamPreservesName :: Maybe String -> Type -> Property
prop_functionParamPreservesName name paramType =
  let param = FunctionParam name paramType False
  in property $ fpName param == name

-- Property 8: Function signatures preserve parameter and return type counts
prop_functionSignaturePreservesCounts :: [FunctionParam] -> [Type] -> Property
prop_functionSignaturePreservesCounts params returnTypes =
  let sig = FunctionSignature params returnTypes
  in property $ length (fsParams sig) == length params && 
              length (fsReturns sig) == length returnTypes

compilerCoreFunctionalityTests :: TestTree
compilerCoreFunctionalityTests = testGroup "Compiler Core Functionality Tests"
  [ testProperties "Type Properties"
    [ ("Type equality is reflexive", property prop_typeEqualityReflexive)
    , ("Type names are preserved", property prop_typeNamePreservation)
    , ("Function types have correct parameter count", property prop_functionTypeParamCount)
    , ("Record types preserve field names", property prop_recordTypeFieldNames)
    , ("Union types contain all variant types", property prop_unionTypeContainsAllTypes)
    ]
  , testProperties "Function Signature Properties"
    [ ("Function signature equality is reflexive", property prop_functionSignatureEqualityReflexive)
    , ("Function parameters preserve their names", property prop_functionParamPreservesName)
    , ("Function signatures preserve parameter and return type counts", property prop_functionSignaturePreservesCounts)
    ]
  ]