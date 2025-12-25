{-# LANGUAGE CPP #-}

module Test.Unit.DependentTypeSystemSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, assertFailure, testCase)
import Data.List (isInfixOf, isPrefixOf)

import DependentTypesParser
  ( TypeRef(..)
  , TypeBody(..)
  , Field(..)
  , TypeParameter(..)
  , TypeConstraint(..)
  , DependentType(..)
  , DependentTypesParser(..)
  , DependentTypeError(..)
  , runDependentTypesParser
  , parseDependentType
  , parseTypeDeclaration
  , validateDependentTypeSyntax
  )

tests :: TestTree
tests = testGroup "Dependent Type System"
  [ typeReferenceTests
  , typeConstraintTests
  , typeParameterTests
  , dependentTypeTests
  , parserErrorHandlingTests
  , complexTypeTests
  , validationTests
  ]

typeReferenceTests :: TestTree
typeReferenceTests = testGroup "Type Reference Tests"
  [ testCase "parses simple type reference" $ do
      let input = "int"
      case parseTypeDeclaration ("type Test struct { x: " ++ input ++ " }") of
        Left err -> assertFailure $ "Failed to parse simple type: " ++ err
        Right (TypeDecl _ _ (StructBody [Field _ fieldType]) _) -> do
          refName fieldType @?= "int"
          refArgs fieldType @?= []
        Right _ -> assertFailure "Unexpected structure"

  , testCase "parses generic type with single parameter" $ do
      let input = "List<int>"
      case parseTypeDeclaration ("type Test struct { x: " ++ input ++ " }") of
        Left err -> assertFailure $ "Failed to parse generic type: " ++ err
        Right (TypeDecl _ _ (StructBody [Field _ fieldType]) _) -> do
          refName fieldType @?= "List"
          length (refArgs fieldType) @?= 1
          let arg = head $ refArgs fieldType
          refName arg @?= "int"
        Right _ -> assertFailure "Unexpected structure"

  , testCase "parses generic type with multiple parameters" $ do
      let input = "Map<string, int>"
      case parseTypeDeclaration ("type Test struct { x: " ++ input ++ " }") of
        Left err -> assertFailure $ "Failed to parse multi-parameter generic: " ++ err
        Right (TypeDecl _ _ (StructBody [Field _ fieldType]) _) -> do
          refName fieldType @?= "Map"
          length (refArgs fieldType) @?= 2
          let [arg1, arg2] = refArgs fieldType
          refName arg1 @?= "string"
          refName arg2 @?= "int"
        Right _ -> assertFailure "Unexpected structure"

  , testCase "parses nested generic types" $ do
      let input = "Map<string, List<int>>"
      case parseTypeDeclaration ("type Test struct { x: " ++ input ++ " }") of
        Left err -> assertFailure $ "Failed to parse nested generic: " ++ err
        Right (TypeDecl _ _ (StructBody [Field _ fieldType]) _) -> do
          refName fieldType @?= "Map"
          length (refArgs fieldType) @?= 2
          let [arg1, arg2] = refArgs fieldType
          refName arg1 @?= "string"
          refName arg2 @?= "List"
          length (refArgs arg2) @?= 1
          let nestedArg = head $ refArgs arg2
          refName nestedArg @?= "int"
        Right _ -> assertFailure "Unexpected structure"
  ]

typeConstraintTests :: TestTree
typeConstraintTests = testGroup "Type Constraint Tests"
  [ testCase "parses equality constraint" $ do
      let input = "type Test<T> struct { x: T } where T == int"
      case parseTypeDeclaration input of
        Left err -> assertFailure $ "Failed to parse equality constraint: " ++ err
        Right (TypeDecl _ params _ constraints) -> do
          length params @?= 1
          length constraints @?= 1
          let constraint = head constraints
          case constraint of
            EqualityConstraint var value -> do
              var @?= "T"
              value @?= "int"
            _ -> assertFailure "Expected EqualityConstraint"
        Right _ -> assertFailure "Unexpected structure"

  , testCase "parses inequality constraint" $ do
      let input = "type Test<T> struct { x: T } where T != string"
      case parseTypeDeclaration input of
        Left err -> assertFailure $ "Failed to parse inequality constraint: " ++ err
        Right (TypeDecl _ _ _ constraints) -> do
          length constraints @?= 1
          let constraint = head constraints
          case constraint of
            InequalityConstraint var value -> do
              var @?= "T"
              value @?= "string"
            _ -> assertFailure "Expected InequalityConstraint"
        Right _ -> assertFailure "Unexpected structure"

  , testCase "parses range constraint" $ do
      let input = "type Test<T> struct { x: T } where T >= 0"
      case parseTypeDeclaration input of
        Left err -> assertFailure $ "Failed to parse range constraint: " ++ err
        Right (TypeDecl _ _ _ constraints) -> do
          length constraints @?= 1
          let constraint = head constraints
          case constraint of
            RangeConstraint var low high -> do
              var @?= "T"
              low @?= 0
              high @?= maxBound
            _ -> assertFailure "Expected RangeConstraint"
        Right _ -> assertFailure "Unexpected structure"

  , testCase "parses size constraint" $ do
      let input = "type Test<T> struct { x: T } where len T == 10"
      case parseTypeDeclaration input of
        Left err -> assertFailure $ "Failed to parse size constraint: " ++ err
        Right (TypeDecl _ _ _ constraints) -> do
          length constraints @?= 1
          let constraint = head constraints
          case constraint of
            SizeConstraint var size -> do
              var @?= "T"
              size @?= 10
            _ -> assertFailure "Expected SizeConstraint"
        Right _ -> assertFailure "Unexpected structure"

  , testCase "parses non-empty constraint" $ do
      let input = "type Test<T> struct { x: T } where nonempty T"
      case parseTypeDeclaration input of
        Left err -> assertFailure $ "Failed to parse non-empty constraint: " ++ err
        Right (TypeDecl _ _ _ constraints) -> do
          length constraints @?= 1
          let constraint = head constraints
          case constraint of
            NonEmptyConstraint var -> var @?= "T"
            _ -> assertFailure "Expected NonEmptyConstraint"
        Right _ -> assertFailure "Unexpected structure"

  , testCase "parses predicate constraint" $ do
      let input = "type Test<T> struct { x: T } where isValid(T)"
      case parseTypeDeclaration input of
        Left err -> assertFailure $ "Failed to parse predicate constraint: " ++ err
        Right (TypeDecl _ _ _ constraints) -> do
          length constraints @?= 1
          let constraint = head constraints
          case constraint of
            PredicateConstraint name args -> do
              name @?= "isValid"
              args @?= ["T"]
            _ -> assertFailure "Expected PredicateConstraint"
        Right _ -> assertFailure "Unexpected structure"

  , testCase "parses multiple constraints" $ do
      let input = "type Test<T> struct { x: T } where T >= 0 & T <= 100 & nonempty T"
      case parseTypeDeclaration input of
        Left err -> assertFailure $ "Failed to parse multiple constraints: " ++ err
        Right (TypeDecl _ _ _ constraints) -> do
          length constraints @?= 3
          let [c1, c2, c3] = constraints
          case c1 of
            RangeConstraint var low high -> do
              var @?= "T"
              low @?= 0
              high @?= maxBound
            _ -> assertFailure "Expected first constraint to be RangeConstraint"
          case c2 of
            RangeConstraint var low high -> do
              var @?= "T"
              low @?= minBound
              high @?= 100
            _ -> assertFailure "Expected second constraint to be RangeConstraint"
          case c3 of
            NonEmptyConstraint var -> var @?= "T"
            _ -> assertFailure "Expected third constraint to be NonEmptyConstraint"
        Right _ -> assertFailure "Unexpected structure"
  ]

typeParameterTests :: TestTree
typeParameterTests = testGroup "Type Parameter Tests"
  [ testCase "parses simple type parameter" $ do
      let input = "type Test<T> struct { x: T }"
      case parseTypeDeclaration input of
        Left err -> assertFailure $ "Failed to parse simple type parameter: " ++ err
        Right (TypeDecl _ params _ _) -> do
          length params @?= 1
          let param = head params
          paramName param @?= "T"
          refName (paramType param) @?= "int"
          paramConstraints param @?= []
        Right _ -> assertFailure "Unexpected structure"

  , testCase "parses type parameter with explicit type" $ do
      let input = "type Test<T: Type> struct { x: T }"
      case parseTypeDeclaration input of
        Left err -> assertFailure $ "Failed to parse typed parameter: " ++ err
        Right (TypeDecl _ params _ _) -> do
          length params @?= 1
          let param = head params
          paramName param @?= "T"
          refName (paramType param) @?= "Type"
        Right _ -> assertFailure "Unexpected structure"

  , testCase "parses type parameter with constraints" $ do
      let input = "type Test<T> struct { x: T } where T >= 0"
      case parseTypeDeclaration input of
        Left err -> assertFailure $ "Failed to parse parameter with constraints: " ++ err
        Right (TypeDecl _ params _ constraints) -> do
          length params @?= 1
          length constraints @?= 1
          let param = head params
          paramName param @?= "T"
          paramConstraints param @?= []  -- Constraints are separate from parameter
        Right _ -> assertFailure "Unexpected structure"

  , testCase "parses multiple type parameters" $ do
      let input = "type Test<T, U> struct { x: T, y: U }"
      case parseTypeDeclaration input of
        Left err -> assertFailure $ "Failed to parse multiple parameters: " ++ err
        Right (TypeDecl _ params _ _) -> do
          length params @?= 2
          let [param1, param2] = params
          paramName param1 @?= "T"
          paramName param2 @?= "U"
        Right _ -> assertFailure "Unexpected structure"

  , testCase "parses complex type parameters" $ do
      let input = "type Test<T: Comparable, U: Container<T>> struct { x: T, y: U }"
      case parseTypeDeclaration input of
        Left err -> assertFailure $ "Failed to parse complex parameters: " ++ err
        Right (TypeDecl _ params _ _) -> do
          length params @?= 2
          let [param1, param2] = params
          paramName param1 @?= "T"
          refName (paramType param1) @?= "Comparable"
          paramName param2 @?= "U"
          refName (paramType param2) @?= "Container"
          length (refArgs (paramType param2)) @?= 1
          refName (head $ refArgs (paramType param2)) @?= "T"
        Right _ -> assertFailure "Unexpected structure"
  ]

dependentTypeTests :: TestTree
dependentTypeTests = testGroup "Dependent Type Tests"
  [ testCase "parses simple struct type" $ do
      let input = "type Point struct { x: int, y: int }"
      case parseTypeDeclaration input of
        Left err -> assertFailure $ "Failed to parse simple struct: " ++ err
        Right (TypeDecl name params body constraints) -> do
          name @?= "Point"
          params @?= []
          constraints @?= []
          case body of
            StructBody fields -> do
              length fields @?= 2
              let [field1, field2] = fields
              fieldName field1 @?= "x"
              refName (fieldType field1) @?= "int"
              fieldName field2 @?= "y"
              refName (fieldType field2) @?= "int"
            _ -> assertFailure "Expected StructBody"
        Right _ -> assertFailure "Unexpected structure"

  , testCase "parses generic struct type" $ do
      let input = "type Container<T> struct { value: T }"
      case parseTypeDeclaration input of
        Left err -> assertFailure $ "Failed to parse generic struct: " ++ err
        Right (TypeDecl name params body constraints) -> do
          name @?= "Container"
          length params @?= 1
          constraints @?= []
          case body of
            StructBody fields -> do
              length fields @?= 1
              let [field] = fields
              fieldName field @?= "value"
              case fieldType field of
                TypeRef "T" [] -> return ()
                _ -> assertFailure "Expected type parameter T"
            _ -> assertFailure "Expected StructBody"
        Right _ -> assertFailure "Unexpected structure"

  , testCase "parses type alias" $ do
      let input = "alias Name = string"
      case parseTypeDeclaration input of
        Left err -> assertFailure $ "Failed to parse type alias: " ++ err
        Right (TypeAlias name target constraints) -> do
          name @?= "Name"
          refName target @?= "string"
          constraints @?= []
        Right _ -> assertFailure "Unexpected structure"

  , testCase "parses function declaration" $ do
      let input = "func add(x: int, y: int) -> int"
      case parseTypeDeclaration input of
        Left err -> assertFailure $ "Failed to parse function: " ++ err
        Right (DependentFunction name params retType constraints) -> do
          name @?= "add"
          length params @?= 2
          let [param1, param2] = params
          fst param1 @?= "x"
          refName (snd param1) @?= "int"
          fst param2 @?= "y"
          refName (snd param2) @?= "int"
          refName retType @?= "int"
          constraints @?= []
        Right _ -> assertFailure "Unexpected structure"

  , testCase "parses function with dependent return type" $ do
      let input = "func create<T>(value: T) -> Container<T>"
      case parseTypeDeclaration input of
        Left err -> assertFailure $ "Failed to parse function with dependent return: " ++ err
        Right (DependentFunction name params retType constraints) -> do
          name @?= "create"
          length params @?= 1
          let [param] = params
          fst param @?= "value"
          case snd param of
            TypeRef "T" [] -> return ()
            _ -> assertFailure "Expected type parameter T"
          case retType of
            TypeRef "Container" [TypeRef "T" []] -> return ()
            _ -> assertFailure "Expected Container<T>"
          constraints @?= []
        Right _ -> assertFailure "Unexpected structure"
  ]

parserErrorHandlingTests :: TestTree
parserErrorHandlingTests = testGroup "Parser Error Handling Tests"
  [ testCase "handles missing struct body" $ do
      let input = "type Test"
      case parseTypeDeclaration input of
        Left _ -> return ()  -- Expected to fail
        Right _ -> assertFailure "Expected parsing to fail"

  , testCase "handles invalid constraint syntax" $ do
      let input = "type Test<T> struct { x: T } where T === int"
      case parseTypeDeclaration input of
        Left _ -> return ()  -- Expected to fail
        Right _ -> assertFailure "Expected parsing to fail"

  , testCase "handles malformed generic syntax" $ do
      let input = "type Test<T struct { x: T }"
      case parseTypeDeclaration input of
        Left _ -> return ()  -- Expected to fail
        Right _ -> assertFailure "Expected parsing to fail"

  , testCase "handles multiple definitions with errors" $ do
      let input = unlines
            [ "type Valid struct { x: int }"
            , "type Invalid struct { x: }"  -- Missing type
            , "type AnotherValid struct { y: string }"
            ]
      case runDependentTypesParser input of
        Left _ -> assertFailure "Should parse some definitions despite errors"
        Right (defs, parser) -> do
          let errors = parserErrors parser
          assertBool "should collect parsing errors" $ not $ null errors
          assertBool "should parse valid definitions" $ length defs >= 2

  , testCase "detects duplicate type definitions" $ do
      let input = unlines
            [ "type Duplicate struct { x: int }"
            , "type Duplicate struct { y: string }"
            ]
      case runDependentTypesParser input of
        Left _ -> assertFailure "Should parse with duplicate error"
        Right (_, parser) -> do
          let errors = parserErrors parser
          assertBool "should detect duplicate definition" $ 
            any (\case InvalidTypeSyntax msg -> "重复定义" `isInfixOf` msg; _ -> False) errors
  ]

complexTypeTests :: TestTree
complexTypeTests = testGroup "Complex Type Tests"
  [ testCase "parses deeply nested generics" $ do
      let input = "type Complex struct { x: Map<string, List<Container<Pair<int, double>>>> }"
      case parseTypeDeclaration input of
        Left err -> assertFailure $ "Failed to parse deeply nested generics: " ++ err
        Right (TypeDecl _ _ (StructBody [Field _ fieldType]) _) -> do
          let TypeRef "Map" [keyType, valueType] = fieldType
          refName keyType @?= "string"
          let TypeRef "List" [listArg] = valueType
          let TypeRef "Container" [containerArg] = listArg
          let TypeRef "Pair" [pairArg1, pairArg2] = containerArg
          refName pairArg1 @?= "int"
          refName pairArg2 @?= "double"
        Right _ -> assertFailure "Unexpected structure"

  , testCase "parses complex constraints" $ do
      let input = unlines
            [ "type Matrix<T, N> struct { data: List<List<T>> }"
            , "where N >= 1"
            , "where N <= 1000"
            , "where len data == N"
            , "where all isValid data"
            ]
      case parseTypeDeclaration input of
        Left err -> assertFailure $ "Failed to parse complex constraints: " ++ err
        Right (TypeDecl _ params _ constraints) -> do
          length params @?= 2
          length constraints @?= 4
          let [param1, param2] = params
          paramName param1 @?= "T"
          paramName param2 @?= "N"
        Right _ -> assertFailure "Unexpected structure"

  , testCase "parses function with complex signature" $ do
      let input = unlines
            [ "func process<T, R>(data: List<T>, transformer: func(T) -> R) -> List<R>"
            , "where nonempty data"
            , "where all isValid data"
            ]
      case parseTypeDeclaration input of
        Left err -> assertFailure $ "Failed to parse complex function: " ++ err
        Right (DependentFunction name params retType constraints) -> do
          name @?= "process"
          length params @?= 2
          length constraints @?= 2
          let [param1, param2] = params
          fst param1 @?= "data"
          fst param2 @?= "transformer"
          case retType of
            TypeRef "List" [TypeRef "R" []] -> return ()
            _ -> assertFailure "Expected List<R>"
        Right _ -> assertFailure "Unexpected structure"
  ]

validationTests :: TestTree
validationTests = testGroup "Validation Tests"
  [ testCase "validates correct syntax" $ do
      let input = "type Test struct { x: int }"
      let errors = validateDependentTypeSyntax input
      assertBool "valid syntax should have no errors" $ null errors

  , testCase "detects syntax errors" $ do
      let input = "type Test struct { x: }"
      let errors = validateDependentTypeSyntax input
      assertBool "invalid syntax should have errors" $ not $ null errors

  , testCase "validates multiple definitions" $ do
      let input = unlines
            [ "type First struct { x: int }"
            , "type Second struct { y: string }"
            , "func test() -> int"
            ]
      let errors = validateDependentTypeSyntax input
      assertBool "multiple valid definitions should have no errors" $ null errors

  , testCase "validates complex but correct syntax" $ do
      let input = unlines
            [ "type Container<T> struct { items: List<T> }"
            , "where nonempty items"
            , "alias StringMap = Map<string, int>"
            , "func process<T>(c: Container<T>) -> T"
            , "where nonempty c.items"
            ]
      let errors = validateDependentTypeSyntax input
      assertBool "complex valid syntax should have no errors" $ null errors
  ]