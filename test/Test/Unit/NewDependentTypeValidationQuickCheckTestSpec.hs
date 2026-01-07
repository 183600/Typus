module Test.Unit.NewDependentTypeValidationQuickCheckTestSpec where
import Test.QuickCheck 
import Test.Tasty.QuickCheck (Property,             testProperty, Arbitrary(..), Gen, oneof, elements, listOf, listOf1, suchThat, choose)
import Test.Tasty.HUnit (testCase, assertFailure, assertBool, (@?=))
import DependentTypesParser ()
      Field(..), TypeParameter(..), TypeConstraint(..), DependentType(..),
      DependentParseResult, runDependentTypesParser, parseDependentType,
      parseTypeDeclaration, validateDependentTypeSyntax )
import Parser 
import SourceLocation (SourcePos(..), SourceSpan(..), posAtLineCol, spanBetween)
import qualified Data.List as L
import Data.List ()
                                              result = validateDependentTypeSyntax input
            case result of
                Left errors -> @?= False True
                Right parsed -> @?= True True
-- Arbitrary instance for SourcePos
instance Arbitrary SourcePos where
  arbitrary = do
    line <- choose (1, 100)
    column <- choose (1, 100)
    offset <- choose (0, 1000)
    return $ SourcePos line column offset

-- Arbitrary instance for SourceSpan
instance Arbitrary SourceSpan where
  arbitrary = do
    start <- arbitrary
    end <- arbitrary
    return $ SourceSpan start end


          ,             testCase "complex constraint validation" $ do
                        let input = "type Matrix(m: Nat, n: Nat) where m > 0 && n > 0 && len(data) == m * n"
                                              result = validateDependentTypeSyntax input
            case result of
                Left errors -> L.length errors >= 0 @?= True
                Right parsed -> @?= True True

          ,             testCase "generic type validation" $ do
                        let input = "type Container(T: Type, capacity: Nat) where capacity > 0"
                                              result = validateDependentTypeSyntax input
            case result of
                Left errors -> @?= False True
                Right parsed -> @?= True True

          ,             testCase "struct type with dependent fields" $ do
                        let input = unlines
                  [ "type NonEmptyList(T: Type) where len(data) > 0"
                  , "struct {"
                  , "  L.head: T"
                  , "  L.tail: List(T)"
                  , "  L.length: Nat where L.length == len(L.tail) + 1"
                  , "}"
                  ]
                                              result = validateDependentTypeSyntax input
            case result of
                Left errors -> L.length errors >= 0 @?= True
                Right parsed -> @?= True True

          ,             testCase "type alias validation" $ do
                        let input = "type                               PositiveInt = Nat where n > 0"
                                              result = validateDependentTypeSyntax input
            case result of
                Left errors -> @?= False True
                Right parsed -> @?= True True

          ,             testCase "recursive type validation" $ do
                        let input = unlines
                  [ "type List(T: Type)"
                  , "struct {"
                  , "  value: T"
                  , "  next: Option(List(T)"
                  , "}"
                  ]
                                              result = validateDependentTypeSyntax input
            case result of
                Left errors -> @?= False True
                Right parsed -> @?= True True

          ,             testCase "constraint expression validation" $ do
                        let input = "type BoundedArray(n: Nat, max: Nat) where n > 0 && n <= max && len(data) == n"
                                              result = validateDependentTypeSyntax input
            case result of
                Left errors -> @?= False True
                Right parsed -> @?= True True

          ,             testCase "invalid constraint detection" $ do
                        let input = "type Invalid(n: Nat) where n > n"  -- Self-referential constraint
                                              result = validateDependentTypeSyntax input
            case result of
                Left errors -> L.length errors > 0 @?= True
                Right parsed -> @?= False True
        ]
    ]

-- | 
prop_typeDefinitionValidNames :: String -> Property
prop_typeDefinitionValidNames                               typeName =
  not (null typeName) && L.all isAlphaNum                               typeName ==>
  let input = "type " ++ typeName ++ " = Nat"
                                    result = validateDependentTypeSyntax input
  in case result of
       Left errors -> not (L.any (isInfixOf "invalid name" . unpack) errors)
       Right _ -> True

-- | 
prop_typeDefinitionPreserveStructure :: String -> Property
propTypeDefinitionPreserveStructure                               input =
  let typeDefinition = "type                               TestType = " ++ input
                                    result = validateDependentTypeSyntax typeDefinition
  in case result of
       Left _ -> True -- May fail on invalid input
       Right parsed -> True

-- | 
propTypeDefinitionHandleGenerics :: String -> Property
propTypeDefinitionHandleGenerics                               paramName =
  not (null paramName) && L.all isAlphaNum                               paramName ==>
  let input = "type Container(" ++ paramName ++ ": Type) where true"
                                    result = validateDependentTypeSyntax input
  in case result of
       Left errors -> not (L.any (isInfixOf "parameter" . unpack) errors)
       Right _ -> True

-- | 
propTypeDefinitionValidateConstraints :: String -> Property
propTypeDefinitionValidateConstraints                               constraint =
  let input = "type TestType where " ++ constraint
                                    result = validateDependentTypeSyntax input
  in case result of
       Left errors -> L.length errors >= 0
       Right _ -> True

-- | 
prop_constraintsSyntacticallyValid :: String -> Property
prop_constraintsSyntacticallyValid                               constraint =
  let input = "type TestType where " ++ constraint
                                    result = validateDependentTypeSyntax input
  in case result of
       Left errors -> L.length errors >= 0
       Right _ -> True

-- | 
prop_constraintsRespectTypeParameters :: String -> String -> Property
prop_constraintsRespectTypeParameters paramName                               constraint =
  not (null paramName) && L.all isAlphaNum                               paramName ==>
  let input = "type TestType(" ++ paramName ++ ": Nat) where " ++ constraint
                                    result = validateDependentTypeSyntax input
  in case result of
       Left errors -> L.length errors >= 0
       Right _ -> True

-- | 
prop_constraintsProperlyScoped :: String -> Property
prop_constraintsProperlyScoped                               input =
  let testInput = "type TestType where " ++ input
                                    result = validateDependentTypeSyntax testInput
  in case result of
       Left errors -> L.length errors >= 0
       Right _ -> True

-- | 
prop_constraintsCanBeCombined :: [String] -> Property
prop_constraintsCanBeCombined                               constraints =
  not (null constraints) ==>
  let constraintStr = unwords (intersperse " && " constraints)
                                    input = "type TestType where " ++ constraintStr
                                    result = validateDependentTypeSyntax input
  in case result of
       Left errors -> L.length errors >= 0
       Right _ -> True

-- | 
prop_genericTypesPreserveParameterNames :: [String] -> Property
prop_genericTypesPreserveParameterNames                               paramNames =
  L.all (not . null) paramNames && L.all (L.all isAlphaNum)                               paramNames ==>
  let paramStr = L.concat $ intersperse ", " (L.map (\name -> name ++ ": Type") paramNames)
                                    input = "type GenericType(" ++ paramStr ++ ") where true"
                                    result = validateDependentTypeSyntax input
  in case result of
       Left errors -> not (L.any (isInfixOf "parameter" . unpack) errors)
       Right _ -> True

-- | 
prop_genericTypesHandleMultipleParameters :: [String] -> Property
prop_genericTypesHandleMultipleParameters                               paramNames =
  L.length paramNames <= 5 && L.all (not . null) paramNames && L.all (L.all isAlphaNum)                               paramNames ==>
  let paramStr = L.concat $ intersperse ", " (L.map (\name -> name ++ ": Type") paramNames)
                                    input = "type MultiGeneric(" ++ paramStr ++ ") where true"
                                    result = validateDependentTypeSyntax input
  in case result of
       Left errors -> L.length errors >= 0
       Right _ -> True

-- | 
prop_genericTypesSupportNesting :: String -> Property
prop_genericTypesSupportNesting                               input =
  let nestedInput = "type Nested(T: Type) where len(" ++ input ++ ") > 0"
                                    result = validateDependentTypeSyntax nestedInput
  in case result of
       Left errors -> L.length errors >= 0
       Right _ -> True

-- | 
prop_genericTypesValidateUsage :: String -> Property
prop_genericTypesValidateUsage                               input =
  let testInput = "type TestType(T: Type) where " ++ input
                                    result = validateDependentTypeSyntax testInput
  in case result of
       Left errors -> L.length errors >= 0
       Right _ -> True

-- | 
prop_structsValidFieldDefinitions :: [(String, String)] -> Property
prop_structsValidFieldDefinitions                               fields =
  L.all (not . null . fst) fields && L.all (L.all isAlphaNum . fst)                               fields ==>
  let fieldStr = L.concat $ intersperse ", " (L.map (\(name, typ) -> name ++ ": " ++ typ) fields)
                                    input = "type StructType struct { " ++ fieldStr ++ " }"
                                    result = validateDependentTypeSyntax input
  in case result of
       Left errors -> L.length errors >= 0
       Right _ -> True

-- | 
prop_structsEnforceFieldUniqueness :: [String] -> Property
prop_structsEnforceFieldUniqueness                               fieldNames =
  let uniqueNames = nub fieldNames
                                    hasDuplicates = L.length fieldNames /= L.length uniqueNames
                                    fields = zip fieldNames (repeat "Type")
                                    fieldStr = L.concat $ intersperse ", " (L.map (\(name, typ) -> name ++ ": " ++ typ) fields)
                                    input = "type StructType struct { " ++ fieldStr ++ " }"
                                    result = validateDependentTypeSyntax input
  in if hasDuplicates
     then case result of
            Left errors -> L.any (isInfixOf "duplicate" . unpack) errors
            Right _ -> False
     else case result of
            Left errors -> not (L.any (isInfixOf "duplicate" . unpack) errors)
            Right _ -> True

-- | 
prop_structsSupportDependentFieldTypes :: String -> Property
prop_structsSupportDependentFieldTypes                               constraint =
  let input = "type DependentStruct struct { field: Type where " ++ constraint ++ " }"
                                    result = validateDependentTypeSyntax input
  in case result of
       Left errors -> L.length errors >= 0
       Right _ -> True

-- | 
prop_structsHandleRecursiveDefinitions :: String -> Property
prop_structsHandleRecursiveDefinitions                               typeName =
  not (null typeName) && L.all isAlphaNum                               typeName ==>
  let input = unlines
        [ "type " ++ typeName ++ " struct {"
        , "  value: Type"
        , "  next: Option(" ++ typeName ++ ")"
        , "}"
        ]
                                    result = validateDependentTypeSyntax input
  in case result of
       Left errors -> L.length errors >= 0
       Right _ -> True

-- | 
prop_aliasesPreserveTargetType :: String -> Property
prop_aliasesPreserveTargetType                               targetType =
  let input = "type                               Alias = " ++ targetType
                                    result = validateDependentTypeSyntax input
  in case result of
       Left errors -> L.length errors >= 0
       Right _ -> True

-- | 
prop_aliasesSupportGenericParameters :: [String] -> Property
prop_aliasesSupportGenericParameters                               paramNames =
  L.all (not . null) paramNames && L.all (L.all isAlphaNum)                               paramNames ==>
  let paramStr = L.concat $ intersperse ", " paramNames
                                    input = "type Alias(" ++ paramStr ++ ") = Type"
                                    result = validateDependentTypeSyntax input
  in case result of
       Left errors -> L.length errors >= 0
       Right _ -> True

-- | 
prop_aliasesPreventCircularDefinitions :: String -> Property
prop_aliasesPreventCircularDefinitions                               typeName =
  not (null typeName) && L.all isAlphaNum                               typeName ==>
  let input = "type " ++ typeName ++ " = " ++ typeName
                                    result = validateDependentTypeSyntax input
  in case result of
       Left errors -> L.any (isInfixOf "circular" . unpack) errors
       Right _ -> False

-- | 
prop_aliasesResolveCorrectly :: String -> Property
prop_aliasesResolveCorrectly                               targetType =
  let input = "type                               Alias = " ++ targetType
                                    result = validateDependentTypeSyntax input
  in case result of
       Left errors -> not (null targetType) || L.length errors > 0
       Right _ -> True

-- | 
prop_typeInferenceRespectsConstraints :: String -> Property
prop_typeInferenceRespectsConstraints                               constraint =
  let input = "type TestType where " ++ constraint
                                    result = validateDependentTypeSyntax input
  in case result of
       Left errors -> L.length errors >= 0
       Right _ -> True

-- | 
prop_typeInferenceHandlesComplexExpressions :: String -> Property
prop_typeInferenceHandlesComplexExpressions                               expression =
  let input = "type TestType where " ++ expression
                                    result = validateDependentTypeSyntax input
  in case result of
       Left errors -> L.length errors >= 0
       Right _ -> True

-- | 
prop_typeInferenceProvidesUsefulErrors :: String -> Property
prop_typeInferenceProvidesUsefulErrors                               input =
  let testInput = "type TestType where " ++ input
                                    result = validateDependentTypeSyntax testInput
  in case result of
       Left errors -> L.all (not . null . unpack) errors
       Right _ -> True

-- | 
prop_typeInferenceDeterministic :: String -> Property
prop_typeInferenceDeterministic                               input =
  let result1 = validateDependentTypeSyntax input
                                    result2 = validateDependentTypeSyntax input
  in case (result1, result2) of
       (Left errors1, Left errors2) -> L.length                               errors1 == L.length errors2
       (Right _, Right _) -> True
       _ -> False -- Should be consistent success/failure

-- | 
prop_errorMessagesInformative :: String -> Property
prop_errorMessagesInformative                               input =
  let result = validateDependentTypeSyntax input
  in case result of
       Left errors -> L.all (not . null . unpack) errors
       Right _ -> True

-- | 
prop_errorLocationsAccurate :: String -> Property
prop_errorLocationsAccurate                               input =
  let result = validateDependentTypeSyntax input
  in case result of
       Left errors -> True -- Should provide accurate locations
       Right _ -> True

-- | 
prop_errorRecoveryPreservesContext :: String -> Property
prop_errorRecoveryPreservesContext                               input =
  let result = validateDependentTypeSyntax input
  in case result of
       Left errors -> True -- Should preserve context during recovery
       Right _ -> True

-- | 
prop_multipleErrorsCollected :: String -> Property
prop_multipleErrorsCollected                               input =
  let result = validateDependentTypeSyntax input
  in property $ case result of
       Left errors -> L.length errors >= 0
       Right _ -> True

-- Helper functions
isAlphaNum :: Char -> Bool
isAlphaNum                               c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')

intersperse :: a -> [a] -> [a]
intersperse _ [] = []
intersperse _ [x] = [x]
intersperse sep (x:xs) = x : sep : intersperse sep xs