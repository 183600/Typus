{-# LANGUAGE CPP #-}
module Test.Unit.NewTypeSystemSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, assertFailure, testCase)
import Data.List (isInfixOf)
import qualified Data.Map.Strict as Map

import Compiler.TypeChecker
  ( Type(..)
  , TypeEnv(..)
  , TypeCheckDiagnostic(..)
  , FunctionInfo(..)
  , FunctionSignature(..)
  , FunctionParam(..)
  , hasTypeErrors
  , diagnoseTypeErrors
  , extractDeclarations
  , extractFunctionCalls
  , buildTypeEnv
  , buildTypeEnvFromPairs
  , isMethodDeclaration
  , checkTypeError
  , hasMalformedSyntax
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
  )
import Parser
  ( parseTypus
  , TypusFile(..)
  )
import Dependencies.TypeSystem
  ( TypeVar(..)
  , TypeConstraint(..)
  , DependentTypeError(..)
  , TypeDef(..)
  , TypeEnv(..)
  , DependentTypeChecker(..)
  , newDependentTypeChecker
  , convertTypeExpr
  , addType
  , addConstraint
  , lookupTypeDef
  , checkType
  , solveConstraints
  , unify
  )
import Dependencies.AST
  ( TypeExpr(..)
  , Constraint(..)
  )

tests :: TestTree
tests =
  testGroup "New Type System Tests"
    [ testCase "creates and manipulates basic types" $ do
        let intType = TypeName "int"
            stringType = TypeName "string"
            funcType = TypeFunction [intType] stringType
        
        intType @?= TypeName "int"
        stringType @?= TypeName "string"
        case funcType of
          TypeFunction [TypeName "int"] (TypeName "string") -> assertBool "function type created" True
          _ -> assertFailure "function type not created correctly"

    , testCase "builds type environment from pairs" $ do
        let pairs = [("x", TypeName "int"), ("y", TypeName "string")]
            typeEnv = buildTypeEnvFromPairs pairs
        case lookupVariable "x" typeEnv of
          Just (TypeName "int") -> assertBool "x type found" True
          _ -> assertFailure "x type not found"
        case lookupVariable "y" typeEnv of
          Just (TypeName "string") -> assertBool "y type found" True
          _ -> assertFailure "y type not found"

    , testCase "adds and retrieves types from environment" $ do
        let initialEnv = TypeEnv Map.empty Map.empty
            envWithInt = addType "Int" (TypeName "int") initialEnv
        case lookupType "Int" envWithInt of
          Just (TypeName "int") -> assertBool "type added and retrieved" True
          _ -> assertFailure "type not found after adding"

    , testCase "adds and retrieves functions from environment" $ do
        let initialEnv = TypeEnv Map.empty Map.empty
            signature = FunctionSignature 
              [FunctionParam (Just "x") (TypeName "int") False]
              [TypeName "int"]
            envWithFunc = addFunction "identity" signature initialEnv
        case lookupVariable "identity" envWithFunc of
          Just (TypeFunction [TypeName "int"] (TypeName "int")) -> assertBool "function added" True
          _ -> assertFailure "function not found"

    , testCase "checks function signatures" $ do
        let signature = FunctionSignature 
              [FunctionParam (Just "x") (TypeName "int") False]
              [TypeName "int"]
        case checkFunctionSignature signature of
          Right _ -> assertBool "signature is valid" True
          Left err -> assertFailure $ "signature validation failed: " ++ show err

    , testCase "infers basic expression types" $ do
        let env = buildTypeEnvFromPairs [("x", TypeName "int")]
        case inferExpressionType "x" env of
          Right (TypeName "int") -> assertBool "inferred x as int" True
          _ -> assertFailure "failed to infer x type"

    , testCase "unifies compatible types" $ do
        case unifyTypes (TypeName "int") (TypeName "int") of
          Right _ -> assertBool "types unified" True
          _ -> assertFailure "failed to unify identical types"

    , testCase "fails to unify incompatible types" $ do
        case unifyTypes (TypeName "int") (TypeName "string") of
          Left _ -> assertBool "correctly rejected incompatible types" True
          Right _ -> assertFailure "should not unify incompatible types"

    , testCase "substitutes types in expressions" $ do
        let substitution = Map.singleton "T" (TypeName "int")
            genericType = TypeVar "TVVar" "T"
        case substituteType substitution genericType of
          TypeName "int" -> assertBool "type substituted correctly" True
          _ -> assertFailure "type substitution failed"

    , testCase "instantiates generic types" $ do
        let genericFunc = TypeFunction [TypeVar "TVVar" "T"] (TypeVar "TVVar" "T")
            concreteType = TypeName "int"
        case instantiateGeneric genericFunc [concreteType] of
          Right (TypeFunction [TypeName "int"] (TypeName "int")) -> assertBool "generic instantiated" True
          _ -> assertFailure "generic instantiation failed"

    , testCase "checks type compatibility" $ do
        let compatible = areTypesCompatible (TypeName "int") (TypeName "int")
            incompatible = areTypesCompatible (TypeName "int") (TypeName "string")
        assertBool "identical types are compatible" compatible
        assertBool "different types are not compatible" (not incompatible)

    , testCase "validates function parameters" $ do
        let signature = FunctionSignature 
              [FunctionParam (Just "x") (TypeName "int") False]
              [TypeName "int"]
            args = [TypeName "int"]
        case checkFunctionParameters signature args of
          Right _ -> assertBool "parameters match" True
          _ -> assertFailure "parameter validation failed"

    , testCase "infers function return types" $ do
        let signature = FunctionSignature 
              [FunctionParam Nothing (TypeName "int") False]
              [TypeName "string"]
        case inferFunctionReturnType signature of
          Right [TypeName "string"] -> assertBool "return type inferred" True
          _ -> assertFailure "return type inference failed"

    , testCase "validates recursive types" $ do
        let env = TypeEnv Map.empty Map.empty
            recursiveType = TypeName "Node"
        case validateRecursiveType recursiveType env of
          Right _ -> assertBool "recursive type validated" True
          _ -> assertFailure "recursive type validation failed"

    , testCase "checks interface implementation" $ do
        let interfaceType = TypeName "Writer"
            implementationType = TypeName "FileWriter"
        case checkInterfaceImplementation interfaceType implementationType of
          Right _ -> assertBool "interface implementation valid" True
          _ -> assertFailure "interface implementation check failed"

    , testCase "checks type coercion" $ do
        let fromType = TypeName "int"
            toType = TypeName "float64"
        case canCoerce fromType toType of
          Right _ -> assertBool "coercion possible" True
          _ -> assertFailure "coercion check failed"

    , testCase "checks subtype relationships" $ do
        let parentType = TypeName "Animal"
            childType = TypeName "Dog"
        case isSubtype childType parentType of
          Right True -> assertBool "subtype relationship valid" True
          _ -> assertFailure "subtype check failed"

    , testCase "compares types for equality" $ do
        let type1 = TypeName "int"
            type2 = TypeName "int"
            type3 = TypeName "string"
        assertBool "identical types are equal" (typesEqual type1 type2)
        assertBool "different types are not equal" (not $ typesEqual type1 type3)

    , testCase "constructs higher-kinded types" $ do
        case constructHigherKindedType "List" [TypeName "int"] of
          Right (TypeApp "List" [TypeName "int"]) -> assertBool "higher-kinded type constructed" True
          _ -> assertFailure "higher-kinded type construction failed"

    , testCase "computes type levels" $ do
        let simpleType = TypeName "int"
            complexType = TypeFunction [TypeName "int"] (TypeName "string")
        computeTypeLevel simpleType @?= 0
        computeTypeLevel complexType @?= 1

    , testCase "validates dependent types" $ do
        let constraint = Predicate "Positive" [TypeVar "TVVar" "x"]
        case validateDependentType constraint of
          Right _ -> assertBool "dependent type valid" True
          _ -> assertFailure "dependent type validation failed"

    , testCase "applies type constraints" $ do
        let constraints = [Equal (TypeVar "TVVar" "T") (TypeName "int")]
            typeVar = TypeVar "TVVar" "T"
        case applyConstraints constraints typeVar of
          Right (TypeName "int") -> assertBool "constraints applied" True
          _ -> assertFailure "constraint application failed"

    , testCase "checks constraint satisfaction" $ do
        let typeVar = TypeVar "TVVar" "x"
            constraint = Predicate "Positive" [typeVar]
        case satisfiesConstraints [constraint] typeVar of
          Right True -> assertBool "constraints satisfied" True
          _ -> assertFailure "constraint satisfaction check failed"

    , testCase "detects malformed syntax" $ do
        let source = unlines
              [ "package main"
              , "func main() {"
              , "    if true"  -- Missing opening brace
              , "        println(\"hello\")"
              , "    }"
              , "}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " ++ err
          Right typusFile -> do
            let malformed = hasMalformedSyntax typusFile
            assertBool "should detect malformed syntax" malformed

    , testCase "diagnoses type errors" $ do
        let source = unlines
              [ "package main"
              , "func main() {"
              , "    var x int = \"string\""  -- Type mismatch
              , "}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " ++ err
          Right typusFile -> do
            case diagnoseTypeErrors typusFile of
              Left errors -> do
                assertBool "should detect type errors" (not $ null errors)
              Right _ -> assertFailure "expected type errors"

    , testCase "extracts function declarations" $ do
        let source = unlines
              [ "package main"
              , "func add(x int, y int) int {"
              , "    return x + y"
              , "}"
              , "func main() {}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " ++ err
          Right typusFile -> do
            let declarations = extractDeclarations typusFile
                addFunction = filter (\decl -> "add" `isInfixOf` show decl) declarations
            assertBool "should extract add function" (not $ null addFunction)

    , testCase "extracts function calls" $ do
        let source = unlines
              [ "package main"
              , "func main() {"
              , "    println(\"hello\")"
              , "    add(1, 2)"
              , "}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " ++ err
          Right typusFile -> do
            let calls = extractFunctionCalls typusFile
                printlnCalls = filter (\call -> "println" `isInfixOf` show call) calls
                addCalls = filter (\call -> "add" `isInfixOf` show call) calls
            assertBool "should extract println call" (not $ null printlnCalls)
            assertBool "should extract add call" (not $ null addCalls)

    , testCase "identifies method declarations" $ do
        let methodDecl = "func (s *Struct) Method() int {}"
            funcDecl = "func Function() int {}"
        assertBool "should identify method declaration" (isMethodDeclaration methodDecl)
        assertBool "should not identify regular function as method" (not $ isMethodDeclaration funcDecl)

    , testCase "creates dependent type checker" $ do
        let checker = newDependentTypeChecker
        case checker of
          DependentTypeChecker _ _ -> assertBool "dependent type checker created" True

    , testCase "converts type expressions" $ do
        let typeExpr = SimpleT "int"
        case convertTypeExpr typeExpr of
          Right (TypeName "int") -> assertBool "type expression converted" True
          _ -> assertFailure "type expression conversion failed"

    , testCase "adds and solves constraints" $ do
        let checker = newDependentTypeChecker
            constraint = Equal (TypeVar "TVVar" "T") (TypeName "int")
        case addConstraint constraint checker of
          Right updatedChecker -> do
            case solveConstraints updatedChecker of
              Right _ -> assertBool "constraints solved" True
              _ -> assertFailure "constraint solving failed"
          _ -> assertFailure "constraint addition failed"

    , testCase "unifies dependent types" $ do
        let type1 = TypeVar "TVVar" "T"
            type2 = TypeName "int"
        case unify type1 type2 of
          Right _ -> assertBool "dependent types unified" True
          _ -> assertFailure "dependent type unification failed"
    ]