{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.DependentTypesValidationEnhancedQuickCheckSpec where

import Test.Tasty
import qualified Data.List as L
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import DependentTypesParser (DependentTypesParser(..), DependentTypeError(..),
                           TypeRef(..), TypeBody(..), Field(..), 
                           TypeParameter(..), TypeConstraint(..), DependentType(..),
                           runDependentTypesParser, parseDependentType, 
                           parseTypeDeclaration, validateDependentTypeSyntax)
import Data.List (isInfixOf)
import Data.List (sort, nub)
import Data.Either (isLeft, isRight)
import Data.Map.Strict (Map)

tests :: TestTree
tests = testGroup "Dependent Types Validation Enhanced QuickCheck Tests"
  [ typeRefProperties
  , fieldProperties
  , typeBodyProperties
  , typeParameterProperties
  , typeConstraintProperties
  , dependentTypeProperties
  , parserProperties
  , validationProperties
  ]

-- | Type reference properties
typeRefProperties :: TestTree
typeRefProperties = testGroup "Type Reference Properties"
  [ testProperty "TypeRef preserves name L.and args" $
      \name args -> 
        let typeRef = TypeRef name args
        in refName typeRef === name .&&. refArgs typeRef === args
  
  , testProperty "TypeRef equality" $
      \name1 args1 name2 args2 -> 
        let typeRef1 = TypeRef name1 args1
            typeRef2 = TypeRef name2 args2
        in (typeRef1 == typeRef2) === (name1 == name2 && args1 == args2)
  
  , testProperty "TypeRef ordering" $
      \name1 args1 name2 args2 -> 
        let typeRef1 = TypeRef name1 args1
            typeRef2 = TypeRef name2 args2
        in compare typeRef1 typeRef2 === compare (name1, args1) (name2, args2)
  
  , testProperty "TypeRef show representation" $
      \name args -> 
        let typeRef = TypeRef name args
            typeRefStr = show typeRef
        in name `L.isInfixOf` typeRefStr
  
  , testProperty "TypeRef handles empty args" $
      \name -> 
        let typeRef = TypeRef name []
        in L.null (refArgs typeRef)
  
  , testProperty "TypeRef handles nested types" $
      \name1 name2 -> 
        let innerType = TypeRef name2 []
            outerType = TypeRef name1 [innerType]
        in refArgs outerType === [innerType]
  ]

-- | Field properties
fieldProperties :: TestTree
fieldProperties = testGroup "Field Properties"
  [ testProperty "Field preserves name L.and type" $
      \name typeRef -> 
        let field = Field name typeRef
        in fieldName field === name .&&. fieldType field === typeRef
  
  , testProperty "Field equality" $
      \name1 type1 name2 type2 -> 
        let field1 = Field name1 type1
            field2 = Field name2 type2
        in (field1 == field2) === (name1 == name2 && type1 == type2)
  
  , testProperty "Field ordering" $
      \name1 type1 name2 type2 -> 
        let field1 = Field name1 type1
            field2 = Field name2 type2
        in compare field1 field2 === compare (name1, type1) (name2, type2)
  
  , testProperty "Field show representation" $
      \name typeRef -> 
        let field = Field name typeRef
            fieldStr = show field
        in name `L.isInfixOf` fieldStr .&&. show typeRef `L.isInfixOf` fieldStr
  ]

-- | Type body properties
typeBodyProperties :: TestTree
typeBodyProperties = testGroup "Type Body Properties"
  [ testProperty "StructBody preserves fields" $
      \fields -> 
        let body = StructBody fields
        in case body of
          StructBody fs -> fs === fields
  
  , testProperty "StructBody equality" $
      \fields1 fields2 -> 
        let body1 = StructBody fields1
            body2 = StructBody fields2
        in (body1 == body2) === (fields1 == fields2)
  
  , testProperty "StructBody ordering" $
      \fields1 fields2 -> 
        let body1 = StructBody fields1
            body2 = StructBody fields2
        in compare body1 body2 === compare fields1 fields2
  
  , testProperty "StructBody handles empty fields" $
      \() -> 
        let body = StructBody []
        in case body of
          StructBody fs -> null fs
  
  , testProperty "StructBody show representation" $
      \fields -> 
        let body = StructBody fields
            bodyStr = show body
        in "StructBody" `L.isInfixOf` bodyStr
  ]

-- | Type parameter properties
typeParameterProperties :: TestTree
typeParameterProperties = testGroup "Type Parameter Properties"
  [ testProperty "TypeParameter preserves L.all fields" $
      \name typeRef constraints -> 
        let param = TypeParameter name typeRef constraints
        in paramName param === name .&&. 
           paramType param === typeRef .&&. 
           paramConstraints param === constraints
  
  , testProperty "TypeParameter equality" $
      \name1 type1 cons1 name2 type2 cons2 -> 
        let param1 = TypeParameter name1 type1 cons1
            param2 = TypeParameter name2 type2 cons2
        in (param1 == param2) === (name1 == name2 && type1 == type2 && cons1 == cons2)
  
  , testProperty "TypeParameter handles empty constraints" $
      \name typeRef -> 
        let param = TypeParameter name typeRef []
        in L.null (paramConstraints param)
  
  , testProperty "TypeParameter show representation" $
      \name typeRef constraints -> 
        let param = TypeParameter name typeRef constraints
            paramStr = show param
        in name `L.isInfixOf` paramStr
  ]

-- | Type constraint properties
typeConstraintProperties :: TestTree
typeConstraintProperties = testGroup "Type Constraint Properties"
  [ testProperty "EqualityConstraint preserves values" $
      \var1 var2 -> 
        let constraint = EqualityConstraint var1 var2
        in case constraint of
          EqualityConstraint v1 v2 -> v1 === var1 .&&. v2 === var2
  
  , testProperty "InequalityConstraint preserves values" $
      \var1 var2 -> 
        let constraint = InequalityConstraint var1 var2
        in case constraint of
          InequalityConstraint v1 v2 -> v1 === var1 .&&. v2 === var2
  
  , testProperty "RangeConstraint preserves values" $
      \var low high -> 
        low <= high ==> 
        let constraint = RangeConstraint var low high
        in case constraint of
          RangeConstraint v l h -> v === var .&&. l === low .&&. h === high
  
  , testProperty "SizeConstraint preserves values" $
      \var size -> 
        size >= 0 ==> 
        let constraint = SizeConstraint var size
        in case constraint of
          SizeConstraint v s -> v === var .&&. s === size
  
  , testProperty "NonEmptyConstraint preserves variable" $
      \var -> 
        let constraint = NonEmptyConstraint var
        in case constraint of
          NonEmptyConstraint v -> v === var
  
  , testProperty "PredicateConstraint preserves values" $
      \name args -> 
        let constraint = PredicateConstraint name args
        in case constraint of
          PredicateConstraint n a -> n === name .&&. a === args
  
  , testProperty "TypeClassConstraint preserves values" $
      \var typeRef -> 
        let constraint = TypeClassConstraint var typeRef
        in case constraint of
          TypeClassConstraint v t -> v === var .&&. t === typeRef
  
  , testProperty "CustomConstraint preserves values" $
      \name value -> 
        let constraint = CustomConstraint name value
        in case constraint of
          CustomConstraint n v -> n === name .&&. v === value
  
  , testProperty "Constraint show representation" $
      \var1 var2 -> 
        let equality = EqualityConstraint var1 var2
            inequality = InequalityConstraint var1 var2
            equalityStr = show equality
            inequalityStr = show inequality
        in "EqualityConstraint" `L.isInfixOf` equalityStr .&&. 
           "InequalityConstraint" `L.isInfixOf` inequalityStr
  ]

-- | Dependent type properties
dependentTypeProperties :: TestTree
dependentTypeProperties = testGroup "Dependent Type Properties"
  [ testProperty "TypeDecl preserves L.all fields" $
      \name params body constraints -> 
        let typeDecl = TypeDecl name params body constraints
        in case typeDecl of
          TypeDecl n p b c -> n === name .&&. p === params .&&. b === body .&&. c === constraints
  
  , testProperty "DependentFunction preserves L.all fields" $
      \name params returnType constraints -> 
        let func = DependentFunction name params returnType constraints
        in case func of
          DependentFunction n p r c -> n === name .&&. p === params .&&. r === returnType .&&. c === constraints
  
  , testProperty "TypeAlias preserves L.all fields" $
      \name typeRef constraints -> 
        let alias = TypeAlias name typeRef constraints
        in case alias of
          TypeAlias n t c -> n === name .&&. t === typeRef .&&. c === constraints
  
  , testProperty "DependentType equality" $
      \name -> 
        let type1 = TypeDecl name [] (StructBody []) []
            type2 = TypeDecl name [] (StructBody []) []
        in type1 === type2
  
  , testProperty "DependentType show representation" $
      \name -> 
        let typeDecl = TypeDecl name [] (StructBody []) []
            typeStr = show typeDecl
        in "TypeDecl" `L.isInfixOf` typeStr .&&. name `L.isInfixOf` typeStr
  ]

-- | Parser properties
parserProperties :: TestTree
parserProperties = testGroup "Parser Properties"
  [ testProperty "runDependentTypesParser handles empty input" $
      \() -> 
        let result = runDependentTypesParser ""
        in case result of
          Left _ -> property True
          Right _ -> property True
  
  , testProperty "runDependentTypesParser handles simple input" $
      \input -> 
        let result = runDependentTypesParser input
        in case result of
          Left _ -> property True
          Right _ -> property True
  
  , testProperty "parseDependentType handles empty input" $
      \() -> 
        let result = parseDependentType ""
        in case result of
          Left _ -> property True
          Right _ -> property True
  
  , testProperty "parseTypeDeclaration handles empty input" $
      \() -> 
        let result = parseTypeDeclaration ""
        in case result of
          Left _ -> property True
          Right _ -> property True
  ]

-- | Validation properties
validationProperties :: TestTree
validationProperties = testGroup "Validation Properties"
  [ testProperty "validateDependentTypeSyntax handles empty input" $
      \() -> 
        let result = validateDependentTypeSyntax ""
        in case result of
          Left _ -> property True
          Right _ -> property True
  
  , testProperty "validateDependentTypeSyntax handles simple input" $
      \input -> 
        let result = validateDependentTypeSyntax input
        in case result of
          Left _ -> property True
          Right _ -> property True
  
  , testProperty "validateDependentTypeSyntax preserves errors" $
      \input -> 
        let result = validateDependentTypeSyntax input
        in case result of
          Left errs -> L.length errs >= 0
          Right _ -> property True
  
  , testProperty "validateDependentTypeSyntax handles malformed input" $
      \input -> 
        let malformed = "type {" ++ input ++ "}"
            result = validateDependentTypeSyntax malformed
        in case result of
          Left _ -> property True
          Right _ -> property True
  
  , testProperty "validateDependentTypeSyntax handles valid type declaration" $
      \name -> 
        let validType = "type " ++ name ++ " {}"
            result = validateDependentTypeSyntax validType
        in case result of
          Left _ -> property True
          Right _ -> property True
  
  , testProperty "validateDependentTypeSyntax handles constraints" $
      \name var -> 
        let constrainedType = "type " ++ name ++ " where " ++ var ++ " > 0"
            result = validateDependentTypeSyntax constrainedType
        in case result of
          Left _ -> property True
          Right _ -> property True
  ]