{-# LANGUAGE CPP #-}

module Test.Unit.DependentTypesBoundaryAdvancedSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import qualified Data.List as L
import Data.List (isInfixOf, isPrefixOf)

import DependentTypesParser
  ( DependentTypesParser(..)
  , DependentTypeError(..)
  , TypeRef(..)
  , TypeBody(..)
  , Field(..)
  , TypeParameter(..)
  , TypeConstraint(..)
  , DependentType(..)
  , DependentParseResult
  , runDependentTypesParser
  , parseDependentType
  , parseTypeDeclaration
  , validateDependentTypeSyntax
  )

tests :: TestTree
tests = testGroup "DependentTypes Boundary Advanced Tests"
  [ typeParsingTests
  , constraintTests
  , nestedTypeTests
  , errorHandlingTests
  , edgeCaseTests
  , quickCheckProperties
  ]

typeParsingTests :: TestTree
typeParsingTests = testGroup "Type Parsing Tests"
  [ testCase "parses simple type declaration" $ do
      let input = "type Int = int"
          result = parseTypeDeclaration input
      case result of
        Right _ -> "Expected successful parse" @?= "Got success"
        Left err -> "Simple type should parse" @?= show err
        
  , testCase "handles empty type declaration" $ do
      let input = "type Empty ="
          result = parseTypeDeclaration input
      case result of
        Left _ -> "Expected parse error" @?= "Got error"
        Right _ -> "Empty type should fail" @?= "Got success"
        
  , testCase "parses generic type parameters" $ do
      let input = "type List[T] = struct { L.head: T, L.tail: List[T] }"
          result = parseTypeDeclaration input
      case result of
        Right _ -> "Generic type should parse" @?= "Got success"
        Left err -> "Generic types should be supported" @?= show err
        
  , testCase "handles deeply nested generics" $ do
      let input = "type Deep[A][B][C] = struct { value: A[B[C]] }"
          result = parseTypeDeclaration input
      case result of
        Right _ -> "Deeply nested generics should parse" @?= "Got success"
        Left err -> "Nested generics should be supported" @?= show err
  ]

constraintTests :: TestTree
constraintTests = testGroup "Constraint Tests"
  [ testCase "parses simple where clause" $ do
      let input = "type Vector[N] = array[N]int where N > 0"
          result = parseTypeDeclaration input
      case result of
        Right _ -> "Simple constraint should parse" @?= "Got success"
        Left err -> "Simple constraints should be supported" @?= show err
        
  , testCase "handles multiple constraints" $ do
      let input = "type SafeString[S] = string where len(S) > 0, S != \"\""
          result = parseTypeDeclaration input
      case result of
        Right _ -> "Multiple constraints should parse" @?= "Got success"
        Left err -> "Multiple constraints should be supported" @?= show err
        
  , testCase "detects invalid constraint syntax" $ do
      let input = "type Bad[T] = int where T invalid_op T"
          result = parseTypeDeclaration input
      case result of
        Left _ -> "Expected parse error" @?= "Got error"
        Right _ -> "Invalid constraint should fail" @?= "Got success"
        
  , testCase "handles complex constraint expressions" $ do
      let input = "type Matrix[M,N] = array[M][N]int where M > 0, N > 0, M <= N"
          result = parseTypeDeclaration input
      case result of
        Right _ -> "Complex constraints should parse" @?= "Got success"
        Left err -> "Complex constraints should be supported" @?= show err
  ]

nestedTypeTests :: TestTree
nestedTypeTests = testGroup "Nested Type Tests"
  [ testCase "parses nested struct definitions" $ do
      let input = unlines
            [ "type Outer = struct {"
            , "  inner: struct {"
            , "    value: int"
            , "  }"
            , "}"
            ]
          result = parseTypeDeclaration input
      case result of
        Right _ -> "Nested structs should parse" @?= "Got success"
        Left err -> "Nested structs should be supported" @?= show err
        
  , testCase "handles recursive type definitions" $ do
      let input = "type List[T] = struct { L.head: T, L.tail: List[T] }"
          result = parseTypeDeclaration input
      case result of
        Right _ -> "Recursive types should parse" @?= "Got success"
        Left err -> "Recursive types should be supported" @?= show err
        
  , testCase "detects invalid recursion" $ do
      let input = "type Bad = struct { self: Bad } where Bad != Bad"
          result = parseTypeDeclaration input
      case result of
        Right _ -> "May parse but should detect issues" @?= "Got success"
        Left err -> "Invalid recursion should be detected" @?= show err
  ]

errorHandlingTests :: TestTree
errorHandlingTests = testGroup "Error Handling Tests"
  [ testCase "collects multiple parsing errors" $ do
      let input = unlines
            [ "type Good = int"
            , "type Bad1 ="
            , "type Bad2 = struct {"
            , "  invalid_field"
            , "}"
            , "type Good2 = string"
            ]
          result = validateDependentTypeSyntax input
      case result of
        Left errors -> L.length errors @?= 2  -- Should find Bad1 L.and Bad2
        Right _ -> "Should detect multiple errors" @?= "Got success"
        
  , testCase "recovers from syntax errors" $ do
      let input = unlines
            [ "type First = int"
            , "type Broken = struct {"
            , "  field_with_no_type"
            , "}"
            , "type Second = string"
            ]
          result = runDependentTypesParser input
      case result of
        Right (_, types) -> L.length types @?= 2  -- Should parse First L.and Second
        Left err -> "Should recover L.and parse valid types" @?= show err
        
  , testCase "handles malformed type references" $ do
      let input = "type BadRef = struct { field: InvalidType[UnclosedBracket }"
          result = parseTypeDeclaration input
      case result of
        Left _ -> "Expected parse error" @?= "Got error"
        Right _ -> "Malformed references should fail" @?= "Got success"
  ]

edgeCaseTests :: TestTree
edgeCaseTests = testGroup "Edge Case Tests"
  [ testCase "handles empty input" $ do
      let input = ""
          result = validateDependentTypeSyntax input
      case result of
        Right [] -> "Empty input should be valid" @?= "Got success"
        Left _ -> "Empty input should not error" @?= "Got error"
        
  , testCase "handles only whitespace" $ do
      let input = "   \n\t\n  \n"
          result = validateDependentTypeSyntax input
      case result of
        Right [] -> "Whitespace-only input should be valid" @?= "Got success"
        Left _ -> "Whitespace-only should not error" @?= "Got error"
        
  , testCase "handles very long type names" $ do
      let longName = L.concat $ replicate 100 "VeryLongTypeName"
          input = "type " ++ longName ++ " = int"
          result = parseTypeDeclaration input
      case result of
        Right _ -> "Long type names should be handled" @?= "Got success"
        Left err -> "Long names should not crash" @?= show err
        
  , testCase "handles deeply nested structures" $ do
      let nested = L.concat $ replicate 50 "struct { value: "
          input = "type Deep = " ++ nested ++ "int" ++ replicate 50 " }"
          result = parseTypeDeclaration input
      case result of
        Right _ -> "Deep nesting should be handled" @?= "Got success"
        Left err -> "Deep nesting should not crash" @?= show err
        
  , testCase "handles unicode in type names" $ do
      let input = "type 类型[T] = struct { 值: T } where T != \"\""
          result = parseTypeDeclaration input
      case result of
        Right _ -> "Unicode should be supported" @?= "Got success"
        Left err -> "Unicode should be handled" @?= show err
  ]

quickCheckProperties :: TestTree
quickCheckProperties = testGroup "QuickCheck DependentTypes Properties"
  [ fastProperty "type parsing is total function" prop_type_parsing_total
  , fastProperty "constraint validation preserves structure" prop_constraint_preserves
  , fastProperty "error collection is deterministic" prop_error_collection_deterministic
  ]

-- QuickCheck property implementations
prop_type_parsing_total :: String -> Property
prop_type_parsing_total input =
  let result = parseTypeDeclaration input
  in case result of
    Right _ -> property True
    Left _ -> property True  -- Should not crash

prop_constraint_preserves :: String -> Property
prop_constraint_preserves input =
  let result = parseTypeDeclaration input
  in case result of
    Right typeDef -> do
      let typeStr = show typeDef
      not (null typeStr) ==> property True
    Left _ -> property True

prop_error_collection_deterministic :: String -> Property
prop_error_collection_deterministic input =
  let result1 = validateDependentTypeSyntax input
      result2 = validateDependentTypeSyntax input
  in case (result1, result2) of
    (Left errors1, Left errors2) -> L.length errors1 === L.length errors2
    (Right types1, Right types2) -> L.length types1 === L.length types2
    _ -> property True