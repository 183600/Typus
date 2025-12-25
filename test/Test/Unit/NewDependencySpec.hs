{-# LANGUAGE CPP #-}
module Test.Unit.NewDependencySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, assertFailure, testCase)
import Data.List (isInfixOf)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Dependencies.AST
  ( TypeExpr(..)
  , Constraint(..)
  , Dependency(..)
  , DependencyGraph(..)
  )
import Dependencies.Analyzer
  ( analyzeDependencies
  , buildDependencyGraph
  , detectCircularDependencies
  , computeTopologicalOrder
  , findMissingDependencies
  , validateDependencies
  )
import Dependencies.Inference
  ( inferTypes
  , inferConstraints
  , refineTypes
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
import Parser
  ( parseTypus
  , TypusFile(..)
  )

tests :: TestTree
tests =
  testGroup "New Dependency Tests"
    [ testCase "analyzes simple dependencies" $ do
        let source = unlines
              [ "package main"
              , "import \"fmt\""
              , "func main() {"
              , "    fmt.Println(\"hello\")"
              , "}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " ++ err
          Right typusFile -> do
            let dependencies = analyzeDependencies typusFile
            assertBool "should detect fmt dependency" (hasImportDependency dependencies "fmt")

    , testCase "detects multiple imports" $ do
        let source = unlines
              [ "package main"
              , "import ("
              , "    \"fmt\""
              , "    \"os\""
              , "    \"strings\""
              , ")"
              , "func main() {}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " ++ err
          Right typusFile -> do
            let dependencies = analyzeDependencies typusFile
            assertBool "should detect fmt dependency" (hasImportDependency dependencies "fmt")
            assertBool "should detect os dependency" (hasImportDependency dependencies "os")
            assertBool "should detect strings dependency" (hasImportDependency dependencies "strings")

    , testCase "builds dependency graph" $ do
        let source = unlines
              [ "package main"
              , "import \"fmt\""
              , "func greet() { fmt.Println(\"hello\") }"
              , "func main() { greet() }"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " ++ err
          Right typusFile -> do
            let graph = buildDependencyGraph typusFile
            assertBool "should build graph" (not $ Map.null $ dependencyNodes graph)

    , testCase "detects circular dependencies" $ do
        let source = unlines
              [ "package main"
              , "func a() { b() }"
              , "func b() { a() }"
              , "func main() { a() }"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " ++ err
          Right typusFile -> do
            let graph = buildDependencyGraph typusFile
                circular = detectCircularDependencies graph
            assertBool "should detect circular dependency" (not $ null circular)

    , testCase "computes topological order" $ do
        let source = unlines
              [ "package main"
              , "func c() {}"
              , "func b() { c() }"
              , "func a() { b() }"
              , "func main() { a() }"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " ++ err
          Right typusFile -> do
            let graph = buildDependencyGraph typusFile
                order = computeTopologicalOrder graph
            assertBool "should compute topological order" (not $ null order)

    , testCase "finds missing dependencies" $ do
        let source = unlines
              [ "package main"
              , "func main() {"
              , "    undefinedFunction()"
              , "}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " ++ err
          Right typusFile -> do
            let graph = buildDependencyGraph typusFile
                missing = findMissingDependencies graph
            assertBool "should find missing dependencies" (not $ null missing)

    , testCase "validates dependencies" $ do
        let source = unlines
              [ "package main"
              , "import \"fmt\""
              , "func main() {"
              , "    fmt.Println(\"hello\")"
              , "}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " ++ err
          Right typusFile -> do
            let graph = buildDependencyGraph typusFile
                validation = validateDependencies graph
            assertBool "should validate dependencies" validation

    , testCase "infers types from dependencies" $ do
        let source = unlines
              [ "package main"
              , "func add(x int, y int) int {"
              , "    return x + y"
              , "}"
              , "func main() {"
              , "    result := add(1, 2)"
              , "}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " ++ err
          Right typusFile -> do
            let types = inferTypes typusFile
            assertBool "should infer types" (not $ Map.null types)

    , testCase "infers constraints from dependencies" $ do
        let source = unlines
              [ "package main"
              , "//! dependent_types: on"
              , "func process[T any](value T) T {"
              , "    return value"
              , "}"
              , "func main() {"
              , "    result := process(42)"
              , "}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " ++ err
          Right typusFile -> do
            let constraints = inferConstraints typusFile
            assertBool "should infer constraints" (not $ null constraints)

    , testCase "refines types with constraints" $ do
        let baseType = TypeVar "TVVar" "T"
            constraints = [Equal baseType (TypeName "int")]
        case refineTypes baseType constraints of
          Right (TypeName "int") -> assertBool "type refined correctly" True
          _ -> assertFailure "type refinement failed"

    , testCase "converts type expressions to dependent types" $ do
        let typeExpr = SimpleT "int"
        case convertTypeExpr typeExpr of
          Right (TypeName "int") -> assertBool "type expression converted" True
          _ -> assertFailure "type expression conversion failed"

    , testCase "converts generic type expressions" $ do
        let typeExpr = GenericT "List" [SimpleT "int"]
        case convertTypeExpr typeExpr of
          Right (TypeApp "List" [TypeName "int"]) -> assertBool "generic type converted" True
          _ -> assertFailure "generic type conversion failed"

    , testCase "converts refined type expressions" $ do
        let typeExpr = RefineT (SimpleT "int") [SizeGE (SimpleT "x") 0]
        case convertTypeExpr typeExpr of
          Right _ -> assertBool "refined type converted" True
          _ -> assertFailure "refined type conversion failed"

    , testCase "adds and retrieves type definitions" $ do
        let checker = newDependentTypeChecker
            typeDef = TypeDefDecl ["T"] [Equal (TypeVar "TVVar" "T") (TypeName "int")]
        case addType "MyType" typeDef checker of
          Right updatedChecker -> do
            case lookupTypeDef "MyType" updatedChecker of
              Just _ -> assertBool "type definition added and retrieved" True
              _ -> assertFailure "type definition not found"
          _ -> assertFailure "failed to add type definition"

    , testCase "adds and solves constraints" $ do
        let checker = newDependentTypeChecker
            constraint = Equal (TypeVar "TVVar" "T") (TypeName "int")
        case addConstraint constraint checker of
          Right updatedChecker -> do
            case solveConstraints updatedChecker of
              Right _ -> assertBool "constraints solved" True
              _ -> assertFailure "constraint solving failed"
          _ -> assertFailure "failed to add constraint"

    , testCase "unifies dependent types" $ do
        let type1 = TypeVar "TVVar" "T"
            type2 = TypeName "int"
        case unify type1 type2 of
          Right _ -> assertBool "types unified" True
          _ -> assertFailure "type unification failed"

    , testCase "checks type validity" $ do
        let checker = newDependentTypeChecker
            typeVar = TypeVar "TVVar" "x"
        case checkType typeVar checker of
          Right _ -> assertBool "type is valid" True
          _ -> assertFailure "type validation failed"

    , testCase "handles complex dependency scenarios" $ do
        let source = unlines
              [ "package main"
              , "import ("
              , "    \"fmt\""
              , "    \"strings\""
              , ")"
              , "type Processor struct {"
              , "    formatter func(string) string"
              , "}"
              , "func NewProcessor() *Processor {"
              , "    return &Processor{"
              , "        formatter: strings.ToUpper,"
              , "    }"
              , "}"
              , "func (p *Processor) Process(input string) string {"
              , "    return p.formatter(input)"
              , "}"
              , "func main() {"
              , "    processor := NewProcessor()"
              , "    result := processor.Process(\"hello\")"
              , "    fmt.Println(result)"
              , "}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " ++ err
          Right typusFile -> do
            let dependencies = analyzeDependencies typusFile
                graph = buildDependencyGraph typusFile
            assertBool "should detect fmt dependency" (hasImportDependency dependencies "fmt")
            assertBool "should detect strings dependency" (hasImportDependency dependencies "strings")
            assertBool "should build complex graph" (not $ Map.null $ dependencyNodes graph)

    , testCase "detects indirect circular dependencies" $ do
        let source = unlines
              [ "package main"
              , "func a() { b() }"
              , "func b() { c() }"
              , "func c() { a() }"
              , "func main() { a() }"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " ++ err
          Right typusFile -> do
            let graph = buildDependencyGraph typusFile
                circular = detectCircularDependencies graph
            assertBool "should detect indirect circular dependency" (not $ null circular)

    , testCase "handles dependency validation errors" $ do
        let source = unlines
              [ "package main"
              , "func main() {"
              , "    // Reference to undefined function"
              , "    undefined()"
              , "}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " ++ err
          Right typusFile -> do
            let graph = buildDependencyGraph typusFile
                validation = validateDependencies graph
                missing = findMissingDependencies graph
            assertBool "validation should fail for missing dependencies" (not validation)
            assertBool "should find missing dependencies" (not $ null missing)

    , testCase "computes dependency statistics" $ do
        let source = unlines
              [ "package main"
              , "import ("
              , "    \"fmt\""
              , "    \"os\""
              , "    \"strings\""
              , ")"
              , "func helper() { fmt.Println(\"helper\") }"
              , "func processor() {"
              , "    helper()"
              , "    strings.ToUpper(\"test\")"
              , "}"
              , "func main() {"
              , "    processor()"
              , "    os.Exit(0)"
              , "}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " ++ err
          Right typusFile -> do
            let dependencies = analyzeDependencies typusFile
                graph = buildDependencyGraph typusFile
            assertBool "should have multiple dependencies" (length dependencies >= 3)
            assertBool "should have multiple nodes in graph" (Map.size (dependencyNodes graph) >= 4)
    ]
  where
    hasImportDependency deps packageName = any (\dep -> 
      case dep of
        ImportDependency name _ -> name == packageName
        _ -> False) deps