{-# LANGUAGE CPP #-}
module Test.Unit.DependentTypeConstraintSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool, assertFailure)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck ((===), Property, forAll, Gen, choose, listOf, elements)
import Data.List (sort, nub, length, intercalate)
import Data.Maybe (isJust, isNothing, fromMaybe)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import DependentTypesParser
  ( DependentTypeError(..)
  , TypeRef(..)
  , TypeBody(..)
  , Field(..)
  , TypeParameter(..)
  , TypeConstraint(..)
  , DependentType(..)
  , DependentTypesParser(..)
  , runDependentTypesParser
  , parseDependentType
  , parseTypeDeclaration
  , validateDependentTypeSyntax
  , tInt
  , tVoid
  )

-- | Constraint and validation tests for DependentTypes module
tests :: TestTree
tests =
  testGroup "DependentType Constraint Tests"
    [ testGroup "TypeRef properties"
        [ fastProperty "TypeRef equality is reflexive" prop_typeRefEquality
        , fastProperty "TypeRef with same args are equal" prop_typeRefSameArgs
        , fastProperty "TypeRef with different args are not equal" prop_typeRefDifferentArgs
        , fastProperty "TypeRef Show is informative" prop_typeRefShow
        ]

    , testGroup "Field properties"
        [ fastProperty "Field equality is reflexive" prop_fieldEquality
        , fastProperty "Field with same name and type are equal" prop_fieldSameNameType
        , fastProperty "Field Show is informative" prop_fieldShow
        ]

    , testGroup "TypeConstraint properties"
        [ fastProperty "TypeConstraint equality is reflexive" prop_constraintEquality
        , fastProperty "EqualityConstraint preserves values" prop_equalityConstraintPreserves
        , fastProperty "RangeConstraint preserves bounds" prop_rangeConstraintPreserves
        , fastProperty "SizeConstraint preserves size" prop_sizeConstraintPreserves
        , fastProperty "NonEmptyConstraint preserves variable" prop_nonEmptyConstraintPreserves
        ]

    , testGroup "TypeBody properties"
        [ fastProperty "StructBody with same fields are equal" prop_structBodySameFields
        , fastProperty "StructBody Show is informative" prop_structBodyShow
        ]

    , testGroup "TypeParameter properties"
        [ fastProperty "TypeParameter equality is reflexive" prop_typeParamEquality
        , fastProperty "TypeParameter preserves all fields" prop_typeParamPreservesFields
        ]

    , testGroup "DependentType properties"
        [ fastProperty "DependentType equality is reflexive" prop_dependentTypeEquality
        , fastProperty "TypeDecl preserves components" prop_typeDeclPreserves
        , fastProperty "TypeAlias preserves components" prop_typeAliasPreserves
        , fastProperty "DependentFunction preserves components" prop_dependentFunctionPreserves
        ]

    , testGroup "DependentTypesParser properties"
        [ testCase "new parser starts with empty state" $ do
            let parser = DependentTypesParser [] Map.empty "test"
            parserErrors parser @?= []
            typeScope parser @?= Map.empty
            sourceName parser @?= "test"

        , testCase "parser can collect errors" $ do
            let errors = [SyntaxError "test error" 1 "fragment"]
                parser = DependentTypesParser errors Map.empty "test"
            length (parserErrors parser) @?= 1
            case head (parserErrors parser) of
              SyntaxError msg line fragment -> do
                msg @?= "test error"
                line @?= 1
                fragment @?= "fragment"
              _ -> assertFailure "Expected SyntaxError"

        , testCase "parser can maintain type scope" $ do
            let typeDecl = TypeDecl "TestType" [] (StructBody []) []
                scope = Map.singleton "TestType" typeDecl
                parser = DependentTypesParser [] scope "test"
            Map.size (typeScope parser) @?= 1
            case Map.lookup "TestType" (typeScope parser) of
              Just decl -> decl @?= typeDecl
              Nothing -> assertFailure "Expected type declaration in scope"
        ]

    , testGroup "Complex constraint scenarios"
        [ testCase "multiple constraints can be combined" $ do
            let constraints = 
                  [ EqualityConstraint "x" "y"
                  , RangeConstraint "size" 1 100
                  , NonEmptyConstraint "list"
                  , PredicateConstraint "valid" ["param1", "param2"]
                  ]
            length constraints @?= 4
            let constraintTypes = map (\c -> case c of
                  EqualityConstraint _ _ -> "Equality"
                  InequalityConstraint _ _ -> "Inequality"
                  RangeConstraint _ _ _ -> "Range"
                  SizeConstraint _ _ -> "Size"
                  NonEmptyConstraint _ -> "NonEmpty"
                  PredicateConstraint _ _ -> "Predicate"
                  TypeClassConstraint _ _ -> "TypeClass"
                  CustomConstraint _ _ -> "Custom"
                  ) constraints
            Set.fromList constraintTypes @?= Set.fromList ["Equality", "Range", "NonEmpty", "Predicate"]

        , testCase "nested TypeRef structures are preserved" $ do
            let innerType = TypeRef "Key" []
                outerType = TypeRef "Map" [innerType, TypeRef "Value" []]
                deeplyNested = TypeRef "Container" [outerType, TypeRef "List" [outerType]]
            refName deeplyNested @?= "Container"
            length (refArgs deeplyNested) @?= 2
            let firstArg = head (refArgs deeplyNested)
            refName firstArg @?= "Map"
            length (refArgs firstArg) @?= 2

        , testCase "complex dependent type definitions are handled" $ do
            let fields = 
                  [ Field "id" tInt
                  , Field "name" (TypeRef "String" [])
                  , Field "data" (TypeRef "Map" [TypeRef "String" [], tInt])
                  ]
                params = 
                  [ TypeParameter "T" tInt [RangeConstraint "T" 0 1000]
                  , TypeParameter "N" tInt [SizeConstraint "N" 10]
                  ]
                constraints = 
                  [ NonEmptyConstraint "name"
                  , PredicateConstraint "valid" ["id", "name"]
                  ]
                typeDecl = TypeDecl "ComplexType" params (StructBody fields) constraints
            case typeDecl of
              TypeDecl name params' body' constraints' -> do
                name @?= "ComplexType"
                length params' @?= 2
                length constraints' @?= 3
                case body' of
                  StructBody fields' -> length fields' @?= 3
                  _ -> assertFailure "Expected StructBody"
              _ -> assertFailure "Expected TypeDecl"

        , testCase "type aliases with constraints work correctly" $ do
            let alias = TypeAlias "StringMap" (TypeRef "Map" [TypeRef "String" [], TypeRef "String" []])
                        [NonEmptyConstraint "key", SizeConstraint "value" 100]
            case alias of
              TypeAlias name typeRef constraints -> do
                name @?= "StringMap"
                refName typeRef @?= "Map"
                length (refArgs typeRef) @?= 2
                length constraints @?= 2
              _ -> assertFailure "Expected TypeAlias"

        , testCase "dependent functions with complex signatures" $ do
            let params = [("x", tInt), ("y", TypeRef "String" [])]
                returnType = TypeRef "Result" [tInt]
                constraints = [EqualityConstraint "x" "y", RangeConstraint "x" 0 100]
                func = DependentFunction "process" params returnType constraints
            case func of
              DependentFunction name params' retType constraints' -> do
                name @?= "process"
                length params' @?= 2
                refName retType @?= "Result"
                length constraints' @?= 2
              _ -> assertFailure "Expected DependentFunction"
        ]

    , testGroup "Error handling scenarios"
        [ testCase "syntax errors are properly categorized" $ do
            let errors = 
                  [ SyntaxError "Unexpected token" 10 "token"
                  , InvalidTypeSyntax "Invalid type definition"
                  , MissingConstraint "Required constraint missing"
                  , InvalidParameter "Invalid parameter"
                  , ConstraintParseError "Cannot parse constraint"
                  , TypeVariableError "Type variable error"
                  ]
            length errors @?= 6
            let errorTypes = map (\e -> case e of
                  SyntaxError _ _ _ -> "Syntax"
                  InvalidTypeSyntax _ -> "InvalidType"
                  MissingConstraint _ -> "MissingConstraint"
                  InvalidParameter _ -> "InvalidParameter"
                  ConstraintParseError _ -> "ConstraintParse"
                  TypeVariableError _ -> "TypeVariable"
                  ) errors
            Set.fromList errorTypes @?= Set.fromList 
              ["Syntax", "InvalidType", "MissingConstraint", "InvalidParameter", "ConstraintParse", "TypeVariable"]
        ]
    ]

-- Helper generators for testing
genTypeRef :: Gen TypeRef
genTypeRef = do
  name <- elements ["Int", "String", "Map", "List", "Result", "Option"]
  args <- listOf genTypeRef
  return $ TypeRef name args

genField :: Gen Field
genField = do
  name <- elements ["id", "name", "value", "data", "key"]
  fieldType <- genTypeRef
  return $ Field name fieldType

genTypeConstraint :: Gen TypeConstraint
genTypeConstraint = do
  var <- elements ["x", "y", "z", "param", "value"]
  var2 <- elements ["a", "b", "c", "other"]
  int1 <- choose (0, 100)
  int2 <- choose (0, 100)
  stringList <- listOf (elements ["arg1", "arg2", "arg3"])
  elements
    [ EqualityConstraint var var2
    , InequalityConstraint var var2
    , RangeConstraint var int1 int2
    , SizeConstraint var int1
    , NonEmptyConstraint var
    , PredicateConstraint var stringList
    , TypeClassConstraint "Show" (TypeRef "Type" [])
    , CustomConstraint var "custom"
    ]

genTypeParameter :: Gen TypeParameter
genTypeParameter = do
  name <- elements ["T", "U", "V", "K", "N"]
  paramType <- genTypeRef
  constraints <- listOf genTypeConstraint
  return $ TypeParameter name paramType constraints

-- Property: TypeRef equality is reflexive
prop_typeRefEquality :: TypeRef -> Property
prop_typeRefEquality typeRef = typeRef === typeRef

-- Property: TypeRef with same args are equal
prop_typeRefSameArgs :: String -> [TypeRef] -> Property
prop_typeRefSameArgs name args = 
  let typeRef1 = TypeRef name args
      typeRef2 = TypeRef name args
  in typeRef1 === typeRef2

-- Property: TypeRef with different args are not equal
prop_typeRefDifferentArgs :: String -> TypeRef -> TypeRef -> Property
prop_typeRefDifferentArgs name arg1 arg2 =
  let typeRef1 = TypeRef name [arg1]
      typeRef2 = TypeRef name [arg2]
  in if arg1 == arg2 
     then typeRef1 === typeRef2
     else typeRef1 /= typeRef2

-- Property: TypeRef Show is informative
prop_typeRefShow :: TypeRef -> Property
prop_typeRefShow typeRef = 
  let typeString = show typeRef
  in length typeString > 0 && refName typeRef `isInfixOf` typeString
  where
    isInfixOf needle haystack = needle `elem` [take (length needle) $ drop i haystack | i <- [0..length haystack - length needle]]

-- Property: Field equality is reflexive
prop_fieldEquality :: Field -> Property
prop_fieldEquality field = field === field

-- Property: Field with same name and type are equal
prop_fieldSameNameType :: String -> TypeRef -> Property
prop_fieldSameNameType name fieldType =
  let field1 = Field name fieldType
      field2 = Field name fieldType
  in field1 === field2

-- Property: Field Show is informative
prop_fieldShow :: Field -> Property
prop_fieldShow field =
  let fieldString = show field
  in length fieldString > 0 && 
     fieldName field `isInfixOf` fieldString
  where
    isInfixOf needle haystack = needle `elem` [take (length needle) $ drop i haystack | i <- [0..length haystack - length needle]]

-- Property: TypeConstraint equality is reflexive
prop_constraintEquality :: TypeConstraint -> Property
prop_constraintEquality constraint = constraint === constraint

-- Property: EqualityConstraint preserves values
prop_equalityConstraintPreserves :: String -> String -> Property
prop_equalityConstraintPreserves var1 var2 =
  let constraint = EqualityConstraint var1 var2
  in case constraint of
       EqualityConstraint v1 v2 -> v1 === var1 && v2 === var2
       _ -> property False

-- Property: RangeConstraint preserves bounds
prop_rangeConstraintPreserves :: String -> Int -> Int -> Property
prop_rangeConstraintPreserves var minVal maxVal =
  let constraint = RangeConstraint var minVal maxVal
  in case constraint of
       RangeConstraint v mn mx -> v === var && mn === minVal && mx === maxVal
       _ -> property False

-- Property: SizeConstraint preserves size
prop_sizeConstraintPreserves :: String -> Int -> Property
prop_sizeConstraintPreserves var size =
  let constraint = SizeConstraint var size
  in case constraint of
       SizeConstraint v s -> v === var && s === size
       _ -> property False

-- Property: NonEmptyConstraint preserves variable
prop_nonEmptyConstraintPreserves :: String -> Property
prop_nonEmptyConstraintPreserves var =
  let constraint = NonEmptyConstraint var
  in case constraint of
       NonEmptyConstraint v -> v === var
       _ -> property False

-- Property: StructBody with same fields are equal
prop_structBodySameFields :: [Field] -> Property
prop_structBodySameFields fields =
  let body1 = StructBody fields
      body2 = StructBody fields
  in body1 === body2

-- Property: StructBody Show is informative
prop_structBodyShow :: [Field] -> Property
prop_structBodyShow fields =
  let body = StructBody fields
      bodyString = show body
  in length bodyString > 0 && "StructBody" `isInfixOf` bodyString
  where
    isInfixOf needle haystack = needle `elem` [take (length needle) $ drop i haystack | i <- [0..length haystack - length needle]]

-- Property: TypeParameter equality is reflexive
prop_typeParamEquality :: TypeParameter -> Property
prop_typeParamEquality param = param === param

-- Property: TypeParameter preserves all fields
prop_typeParamPreservesFields :: String -> TypeRef -> [TypeConstraint] -> Property
prop_typeParamPreservesFields name paramType constraints =
  let param = TypeParameter name paramType constraints
  in case param of
       TypeParameter n pt cs -> n === name && pt === paramType && cs === constraints
       _ -> property False

-- Property: DependentType equality is reflexive
prop_dependentTypeEquality :: DependentType -> Property
prop_dependentTypeEquality depType = depType === depType

-- Property: TypeDecl preserves components
prop_typeDeclPreserves :: String -> [TypeParameter] -> TypeBody -> [TypeConstraint] -> Property
prop_typeDeclPreserves name params body constraints =
  let typeDecl = TypeDecl name params body constraints
  in case typeDecl of
       TypeDecl n ps bs cs -> n === name && ps === params && bs === body && cs === constraints
       _ -> property False

-- Property: TypeAlias preserves components
prop_typeAliasPreserves :: String -> TypeRef -> [TypeConstraint] -> Property
prop_typeAliasPreserves name typeRef constraints =
  let alias = TypeAlias name typeRef constraints
  in case alias of
       TypeAlias n tr cs -> n === name && tr === typeRef && cs === constraints
       _ -> property False

-- Property: DependentFunction preserves components
prop_dependentFunctionPreserves :: String -> [(String, TypeRef)] -> TypeRef -> [TypeConstraint] -> Property
prop_dependentFunctionPreserves name params returnType constraints =
  let func = DependentFunction name params returnType constraints
  in case func of
       DependentFunction n ps rt cs -> n === name && ps === params && rt === returnType && cs === constraints
       _ -> property False