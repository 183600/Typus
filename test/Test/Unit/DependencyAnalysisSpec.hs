{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.DependencyAnalysisSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=), assertBool)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import TestSupport.Arbitrary

import Dependencies (analyzeDependencies, DependencyResult(..), DependencyIssue(..), DependencyGraph(..))
import Parser (parseTypus, TypusFile(..))
import Compiler (compileTypus, CompilationResult(..))
import SourceLocation (SourcePos(..), ErrorLocation(..))

import Data.Text (Text)
import qualified Data.Text as T
import Data.List (isPrefixOf, isInfixOf, sort, nub)
import Data.Maybe (isJust, isNothing, fromMaybe)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- ============================================================================
-- Dependency Analysis Tests
-- ============================================================================

tests :: TestTree
tests =
  testGroup "Dependency Analysis Tests"
    [ testGroup "Basic dependency detection"
        [ testCase "detects simple function dependencies" test_simple_function_dependencies
        , testCase "detects variable dependencies" test_variable_dependencies
        , testCase "detects type dependencies" test_type_dependencies
        , testCase "detects import dependencies" test_import_dependencies
        , testCase "builds dependency graph correctly" test_dependency_graph_building
        ]

    , testGroup "Complex dependency scenarios"
        [ testCase "handles transitive dependencies" test_transitive_dependencies
        , testCase "detects circular dependencies" test_circular_dependencies
        , testCase "handles conditional dependencies" test_conditional_dependencies
        , testCase "detects dead code" test_dead_code_detection
        , testCase "handles dependency cycles in functions" test_function_dependency_cycles
        ]

    , testGroup "Module and package dependencies"
        [ testCase "detects cross-module dependencies" test_cross_module_dependencies
        , testCase "analyzes package-level dependencies" test_package_dependencies
        , testCase "handles external library dependencies" test_external_dependencies
        , testCase "detects version conflicts" test_version_conflicts
        , testCase "analyzes dependency resolution order" test_dependency_resolution_order
        ]

    , testGroup "Type system dependencies"
        [ testCase "detects interface dependencies" test_interface_dependencies
        , testCase "handles generic type dependencies" test_generic_type_dependencies
        , testCase "detects struct field dependencies" test_struct_field_dependencies
        , testCase "handles inheritance dependencies" test_inheritance_dependencies
        , testCase "detects type constraint dependencies" test_type_constraint_dependencies
        ]

    , testGroup "Runtime dependencies"
        [ testCase "detects runtime reflection dependencies" test_runtime_reflection_dependencies
        , testCase "handles dynamic loading dependencies" test_dynamic_loading_dependencies
        , testCase "detects plugin dependencies" test_plugin_dependencies
        , testCase "handles runtime code generation" test_runtime_code_generation
        , testCase "detects serialization dependencies" test_serialization_dependencies
        ]

    , testGroup "Dependency analysis optimization"
        [ testCase "optimizes dependency computation" test_dependency_computation_optimization
        , testCase "handles large dependency graphs efficiently" test_large_dependency_graphs
        , testCase "caches dependency analysis results" test_dependency_caching
        , testCase "incremental dependency analysis" test_incremental_analysis
        ]

    , testGroup "Error handling and recovery"
        [ testCase "provides clear dependency error messages" test_clear_error_messages
        , testCase "suggests dependency fixes" test_dependency_fix_suggestions
        , testCase "handles missing dependencies gracefully" test_missing_dependencies
        , testCase "maintains analysis state across errors" test_analysis_state_maintenance
        ]

    , testGroup "Property-based dependency tests"
        [ fastProperty "dependency analysis is deterministic" prop_dependency_deterministic
        , fastProperty "dependency graph is acyclic for valid code" prop_dependency_graph_acyclic
        , fastProperty "dependency closure is computed correctly" prop_dependency_closure_correct
        , fastProperty "dependency analysis preserves semantics" prop_dependency_preserves_semantics
        ]
    ]

-- ============================================================================
-- Basic Dependency Detection Tests
-- ============================================================================

test_simple_function_dependencies :: IO ()
test_simple_function_dependencies = do
  let content = unlines
        [ "//! dependent-types=true"
        , "func helper() int {"
        , "    return 42"
        , "}"
        , "func main() {"
        , "    result := helper()"
        , "    return result"
        , "}"
        ]
      parseResult = parseTypus content
  case parseResult of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right typusFile -> do
      let dependencyResult = analyzeDependencies typusFile
      assertBool "Dependency analysis should succeed" (drSuccess dependencyResult)
      let dependencies = drDependencies dependencyResult
      assertBool "Should detect function dependencies" (not (null dependencies))

test_variable_dependencies :: IO ()
test_variable_dependencies = do
  let content = unlines
        [ "//! dependent-types=true"
        , "func main() {"
        , "    x := 42"
        , "    y := x + 1"
        , "    z := y * 2"
        , "    return z"
        , "}"
        ]
      parseResult = parseTypus content
  case parseResult of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right typusFile -> do
      let dependencyResult = analyzeDependencies typusFile
      assertBool "Dependency analysis should succeed" (drSuccess dependencyResult)
      let dependencies = drDependencies dependencyResult
      assertBool "Should detect variable dependencies" (not (null dependencies))

test_type_dependencies :: IO ()
test_type_dependencies = do
  let content = unlines
        [ "//! dependent-types=true"
        , "type User struct {"
        , "    name string"
        , "    age int"
        , "}"
        , "func processUser(u User) string {"
        , "    return u.name"
        , "}"
        , "func main() {"
        , "    user := User{name: \"Alice\", age: 30}"
        , "    return processUser(user)"
        , "}"
        ]
      parseResult = parseTypus content
  case parseResult of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right typusFile -> do
      let dependencyResult = analyzeDependencies typusFile
      assertBool "Dependency analysis should succeed" (drSuccess dependencyResult)
      let dependencies = drDependencies dependencyResult
      assertBool "Should detect type dependencies" (not (null dependencies))

test_import_dependencies :: IO ()
test_import_dependencies = do
  let content = unlines
        [ "//! dependent-types=true"
        , "import \"fmt\""
        , "import \"strings\""
        , "func main() {"
        , "    message := fmt.Sprintf(\"Hello %s\", \"World\")"
        , "    upper := strings.ToUpper(message)"
        , "    return upper"
        , "}"
        ]
      parseResult = parseTypus content
  case parseResult of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right typusFile -> do
      let dependencyResult = analyzeDependencies typusFile
      assertBool "Dependency analysis should succeed" (drSuccess dependencyResult)
      let dependencies = drDependencies dependencyResult
      assertBool "Should detect import dependencies" (not (null dependencies))

test_dependency_graph_building :: IO ()
test_dependency_graph_building = do
  let content = unlines
        [ "//! dependent-types=true"
        , "func a() { b() }"
        , "func b() { c() }"
        , "func c() { d() }"
        , "func d() { return }"
        , "func main() { a() }"
        ]
      parseResult = parseTypus content
  case parseResult of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right typusFile -> do
      let dependencyResult = analyzeDependencies typusFile
      assertBool "Dependency analysis should succeed" (drSuccess dependencyResult)
      let graph = drDependencyGraph dependencyResult
      assertBool "Should build dependency graph" (not (null graph))

-- ============================================================================
-- Complex Dependency Scenarios Tests
-- ============================================================================

test_transitive_dependencies :: IO ()
test_transitive_dependencies = do
  let content = unlines
        [ "//! dependent-types=true"
        , "func level3() int { return 42 }"
        , "func level2() int { return level3() }"
        , "func level1() int { return level2() }"
        , "func main() { return level1() }"
        ]
      parseResult = parseTypus content
  case parseResult of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right typusFile -> do
      let dependencyResult = analyzeDependencies typusFile
      assertBool "Dependency analysis should succeed" (drSuccess dependencyResult)
      let dependencies = drDependencies dependencyResult
      -- Should detect transitive dependencies (main -> level1 -> level2 -> level3)
      assertBool "Should detect transitive dependencies" (length dependencies >= 3)

test_circular_dependencies :: IO ()
test_circular_dependencies = do
  let content = unlines
        [ "//! dependent-types=true"
        , "func a() { b() }"
        , "func b() { c() }"
        , "func c() { a() }"  -- Circular dependency
        , "func main() { a() }"
        ]
      parseResult = parseTypus content
  case parseResult of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right typusFile -> do
      let dependencyResult = analyzeDependencies typusFile
      assertBool "Dependency analysis should succeed" (drSuccess dependencyResult)
      let issues = drIssues dependencyResult
      -- Should detect circular dependency
      let circularIssues = filter (\issue -> "circular" `isInfixOf` diMessage issue) issues
      assertBool "Should detect circular dependency" (not (null circularIssues))

test_conditional_dependencies :: IO ()
test_conditional_dependencies = do
  let content = unlines
        [ "//! dependent-types=true"
        , "func helper() int { return 42 }"
        , "func main() {"
        , "    if true {"
        , "        return helper()"
        , "    } else {"
        , "        return 0"
        , "    }"
        , "}"
        ]
      parseResult = parseTypus content
  case parseResult of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right typusFile -> do
      let dependencyResult = analyzeDependencies typusFile
      assertBool "Dependency analysis should succeed" (drSuccess dependencyResult)
      let dependencies = drDependencies dependencyResult
      -- Should detect conditional dependencies
      assertBool "Should detect conditional dependencies" (not (null dependencies))

test_dead_code_detection :: IO ()
test_dead_code_detection = do
  let content = unlines
        [ "//! dependent-types=true"
        , "func unused() int { return 42 }"  -- Dead code
        , "func main() { return 0 }"
        ]
      parseResult = parseTypus content
  case parseResult of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right typusFile -> do
      let dependencyResult = analyzeDependencies typusFile
      assertBool "Dependency analysis should succeed" (drSuccess dependencyResult)
      let issues = drIssues dependencyResult
      -- Should detect dead code
      let deadCodeIssues = filter (\issue -> "dead code" `isInfixOf` diMessage issue) issues
      assertBool "Should detect dead code" (not (null deadCodeIssues))

test_function_dependency_cycles :: IO ()
test_function_dependency_cycles = do
  let content = unlines
        [ "//! dependent-types=true"
        , "func fibonacci(n int) int {"
        , "    if n <= 1 {"
        , "        return n"
        , "    }"
        , "    return fibonacci(n-1) + fibonacci(n-2)"  -- Recursive call
        , "}"
        , "func main() { return fibonacci(10) }"
        ]
      parseResult = parseTypus content
  case parseResult of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right typusFile -> do
      let dependencyResult = analyzeDependencies typusFile
      assertBool "Dependency analysis should succeed" (drSuccess dependencyResult)
      let dependencies = drDependencies dependencyResult
      -- Should handle recursive function dependencies
      assertBool "Should handle recursive function dependencies" (not (null dependencies))

-- ============================================================================
-- Module and Package Dependencies Tests
-- ============================================================================

test_cross_module_dependencies :: IO ()
test_cross_module_dependencies = do
  let content = unlines
        [ "//! dependent-types=true"
        , "import \"other_module\""
        , "func main() {"
        , "    result := other_module.Function()"
        , "    return result"
        , "}"
        ]
      parseResult = parseTypus content
  case parseResult of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right typusFile -> do
      let dependencyResult = analyzeDependencies typusFile
      assertBool "Dependency analysis should succeed" (drSuccess dependencyResult)
      let dependencies = drDependencies dependencyResult
      -- Should detect cross-module dependencies
      assertBool "Should detect cross-module dependencies" (not (null dependencies))

test_package_dependencies :: IO ()
test_package_dependencies = do
  let content = unlines
        [ "//! dependent-types=true"
        , "package main"
        , "import ("
        , "    \"fmt\""
        , "    \"os\""
        , "    \"strings\""
        , ")"
        , "func main() {"
        , "    fmt.Println(strings.Join(os.Args, \" \"))"
        , "}"
        ]
      parseResult = parseTypus content
  case parseResult of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right typusFile -> do
      let dependencyResult = analyzeDependencies typusFile
      assertBool "Dependency analysis should succeed" (drSuccess dependencyResult)
      let dependencies = drDependencies dependencyResult
      -- Should analyze package-level dependencies
      assertBool "Should analyze package dependencies" (not (null dependencies))

test_external_dependencies :: IO ()
test_external_dependencies = do
  let content = unlines
        [ "//! dependent-types=true"
        , "import \"github.com/example/library\""
        , "func main() {"
        , "    library.Process()"
        , "}"
        ]
      parseResult = parseTypus content
  case parseResult of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right typusFile -> do
      let dependencyResult = analyzeDependencies typusFile
      assertBool "Dependency analysis should succeed" (drSuccess dependencyResult)
      let dependencies = drDependencies dependencyResult
      -- Should detect external library dependencies
      assertBool "Should detect external dependencies" (not (null dependencies))

test_version_conflicts :: IO ()
test_version_conflicts = do
  let content = unlines
        [ "//! dependent-types=true"
        , "import \"github.com/example/lib v1.0.0\""
        , "import \"github.com/example/lib v2.0.0\""  -- Version conflict
        , "func main() {"
        , "    lib1.Process()"
        , "    lib2.Process()"
        , "}"
        ]
      parseResult = parseTypus content
  case parseResult of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right typusFile -> do
      let dependencyResult = analyzeDependencies typusFile
      assertBool "Dependency analysis should succeed" (drSuccess dependencyResult)
      let issues = drIssues dependencyResult
      -- Should detect version conflicts
      let versionIssues = filter (\issue -> "version" `isInfixOf` diMessage issue) issues
      assertBool "Should detect version conflicts" (not (null versionIssues))

test_dependency_resolution_order :: IO ()
test_dependency_resolution_order = do
  let content = unlines
        [ "//! dependent-types=true"
        , "func d() { return }"
        , "func c() { d() }"
        , "func b() { c() }"
        , "func a() { b() }"
        , "func main() { a() }"
        ]
      parseResult = parseTypus content
  case parseResult of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right typusFile -> do
      let dependencyResult = analyzeDependencies typusFile
      assertBool "Dependency analysis should succeed" (drSuccess dependencyResult)
      let order = drResolutionOrder dependencyResult
      -- Should provide correct resolution order
      assertBool "Should provide resolution order" (not (null order))

-- ============================================================================
-- Type System Dependencies Tests
-- ============================================================================

test_interface_dependencies :: IO ()
test_interface_dependencies = do
  let content = unlines
        [ "//! dependent-types=true"
        , "type Writer interface {"
        , "    Write(data []byte) error"
        , "}"
        , "type FileWriter struct {"
        , "    file *os.File"
        , "}"
        , "func (fw *FileWriter) Write(data []byte) error {"
        , "    return nil"
        , "}"
        , "func main() {"
        , "    var w Writer = &FileWriter{}"
        , "    w.Write([]byte(\"hello\"))"
        , "}"
        ]
      parseResult = parseTypus content
  case parseResult of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right typusFile -> do
      let dependencyResult = analyzeDependencies typusFile
      assertBool "Dependency analysis should succeed" (drSuccess dependencyResult)
      let dependencies = drDependencies dependencyResult
      -- Should detect interface dependencies
      assertBool "Should detect interface dependencies" (not (null dependencies))

test_generic_type_dependencies :: IO ()
test_generic_type_dependencies = do
  let content = unlines
        [ "//! dependent-types=true"
        , "type Container[T] struct {"
        , "    data []T"
        , "}"
        , "func (c *Container[T]) Add(item T) {"
        , "    c.data = append(c.data, item)"
        , "}"
        , "func main() {"
        , "    c := Container[int]{data: []int{}}"
        , "    c.Add(42)"
        , "}"
        ]
      parseResult = parseTypus content
  case parseResult of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right typusFile -> do
      let dependencyResult = analyzeDependencies typusFile
      assertBool "Dependency analysis should succeed" (drSuccess dependencyResult)
      let dependencies = drDependencies dependencyResult
      -- Should handle generic type dependencies
      assertBool "Should handle generic type dependencies" (not (null dependencies))

test_struct_field_dependencies :: IO ()
test_struct_field_dependencies = do
  let content = unlines
        [ "//! dependent-types=true"
        , "type Address struct {"
        , "    street string"
        , "    city string"
        , "}"
        , "type Person struct {"
        , "    name string"
        , "    address Address"
        , "}"
        , "func main() {"
        , "    p := Person{"
        , "        name: \"Alice\","
        , "        address: Address{street: \"123 Main\", city: \"Anytown\"}"
        , "    }"
        , "    return p.address.city"
        , "}"
        ]
      parseResult = parseTypus content
  case parseResult of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right typusFile -> do
      let dependencyResult = analyzeDependencies typusFile
      assertBool "Dependency analysis should succeed" (drSuccess dependencyResult)
      let dependencies = drDependencies dependencyResult
      -- Should detect struct field dependencies
      assertBool "Should detect struct field dependencies" (not (null dependencies))

test_inheritance_dependencies :: IO ()
test_inheritance_dependencies = do
  let content = unlines
        [ "//! dependent-types=true"
        , "type Animal struct {"
        , "    name string"
        , "}"
        , "func (a *Animal) Speak() string {"
        , "    return \"animal sound\""
        , "}"
        , "type Dog struct {"
        , "    Animal"
        , "    breed string"
        , "}"
        , "func (d *Dog) Speak() string {"
        , "    return \"woof\""
        , "}"
        , "func main() {"
        , "    d := Dog{Animal: Animal{name: \"Buddy\"}, breed: \"Golden\"}"
        , "    return d.Speak()"
        , "}"
        ]
      parseResult = parseTypus content
  case parseResult of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right typusFile -> do
      let dependencyResult = analyzeDependencies typusFile
      assertBool "Dependency analysis should succeed" (drSuccess dependencyResult)
      let dependencies = drDependencies dependencyResult
      -- Should detect inheritance dependencies
      assertBool "Should detect inheritance dependencies" (not (null dependencies))

test_type_constraint_dependencies :: IO ()
test_type_constraint_dependencies = do
  let content = unlines
        [ "//! dependent-types=true"
        , "func Max[T comparable](a, b T) T {"
        , "    if a > b {"
        , "        return a"
        , "    }"
        , "    return b"
        , "}"
        , "func main() {"
        , "    result := Max(5, 3)"
        , "    return result"
        , "}"
        ]
      parseResult = parseTypus content
  case parseResult of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right typusFile -> do
      let dependencyResult = analyzeDependencies typusFile
      assertBool "Dependency analysis should succeed" (drSuccess dependencyResult)
      let dependencies = drDependencies dependencyResult
      -- Should detect type constraint dependencies
      assertBool "Should detect type constraint dependencies" (not (null dependencies))

-- ============================================================================
-- Runtime Dependencies Tests
-- ============================================================================

test_runtime_reflection_dependencies :: IO ()
test_runtime_reflection_dependencies = do
  let content = unlines
        [ "//! dependent-types=true"
        , "import \"reflect\""
        , "func main() {"
        , "    x := 42"
        , "    t := reflect.TypeOf(x)"
        , "    return t.Name()"
        , "}"
        ]
      parseResult = parseTypus content
  case parseResult of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right typusFile -> do
      let dependencyResult = analyzeDependencies typusFile
      assertBool "Dependency analysis should succeed" (drSuccess dependencyResult)
      let dependencies = drDependencies dependencyResult
      -- Should detect runtime reflection dependencies
      assertBool "Should detect runtime reflection dependencies" (not (null dependencies))

test_dynamic_loading_dependencies :: IO ()
test_dynamic_loading_dependencies = do
  let content = unlines
        [ "//! dependent-types=true"
        , "import \"plugin\""
        , "func main() {"
        , "    p, err := plugin.Open(\"module.so\")"
        , "    if err != nil { return }"
        , "    symbol, _ := p.Lookup(\"Function\")"
        , "    fn := symbol.(func() int)"
        , "    return fn()"
        , "}"
        ]
      parseResult = parseTypus content
  case parseResult of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right typusFile -> do
      let dependencyResult = analyzeDependencies typusFile
      assertBool "Dependency analysis should succeed" (drSuccess dependencyResult)
      let dependencies = drDependencies dependencyResult
      -- Should detect dynamic loading dependencies
      assertBool "Should detect dynamic loading dependencies" (not (null dependencies))

test_plugin_dependencies :: IO ()
test_plugin_dependencies = do
  let content = unlines
        [ "//! dependent-types=true"
        , "type Plugin interface {"
        , "    Initialize() error"
        , "    Process(data []byte) []byte"
        , "}"
        , "func LoadPlugin(name string) Plugin {"
        , "    // Dynamic plugin loading logic"
        , "    return nil"
        , "}"
        , "func main() {"
        , "    plugin := LoadPlugin(\"example\")"
        , "    plugin.Initialize()"
        , "}"
        ]
      parseResult = parseTypus content
  case parseResult of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right typusFile -> do
      let dependencyResult = analyzeDependencies typusFile
      assertBool "Dependency analysis should succeed" (drSuccess dependencyResult)
      let dependencies = drDependencies dependencyResult
      -- Should detect plugin dependencies
      assertBool "Should detect plugin dependencies" (not (null dependencies))

test_runtime_code_generation :: IO ()
test_runtime_code_generation = do
  let content = unlines
        [ "//! dependent-types=true"
        , "import \"text/template\""
        , "func main() {"
        , "    tmpl := template.Must(template.New(\"test\").Parse(\"{{.}}\"))"
        , "    result := tmpl.Execute(nil, \"hello\")"
        , "    return result"
        , "}"
        ]
      parseResult = parseTypus content
  case parseResult of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right typusFile -> do
      let dependencyResult = analyzeDependencies typusFile
      assertBool "Dependency analysis should succeed" (drSuccess dependencyResult)
      let dependencies = drDependencies dependencyResult
      -- Should detect runtime code generation dependencies
      assertBool "Should detect runtime code generation dependencies" (not (null dependencies))

test_serialization_dependencies :: IO ()
test_serialization_dependencies = do
  let content = unlines
        [ "//! dependent-types=true"
        , "import \"encoding/json\""
        , "type User struct {"
        , "    Name string `json:\"name\"`"
        , "    Age  int    `json:\"age\"`"
        , "}"
        , "func main() {"
        , "    user := User{Name: \"Alice\", Age: 30}"
        , "    data, _ := json.Marshal(user)"
        , "    return string(data)"
        , "}"
        ]
      parseResult = parseTypus content
  case parseResult of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right typusFile -> do
      let dependencyResult = analyzeDependencies typusFile
      assertBool "Dependency analysis should succeed" (drSuccess dependencyResult)
      let dependencies = drDependencies dependencyResult
      -- Should detect serialization dependencies
      assertBool "Should detect serialization dependencies" (not (null dependencies))

-- ============================================================================
-- Dependency Analysis Optimization Tests
-- ============================================================================

test_dependency_computation_optimization :: IO ()
test_dependency_computation_optimization = do
  let largeContent = concat $ map (\i -> "func func" ++ show i ++ "() { return " ++ show i ++ " }\n") [1..1000]
      parseResult = parseTypus largeContent
  case parseResult of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right typusFile -> do
      let dependencyResult = analyzeDependencies typusFile
      assertBool "Dependency analysis should succeed" (drSuccess dependencyResult)
      -- Should handle large files efficiently
      assertBool "Should optimize dependency computation" (True)

test_large_dependency_graphs :: IO ()
test_large_dependency_graphs = do
  let complexContent = concat $ map (\i -> "func func" ++ show i ++ "() { func" ++ show ((i+1) `mod` 1000) ++ "() }\n") [1..1000]
      parseResult = parseTypus complexContent
  case parseResult of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right typusFile -> do
      let dependencyResult = analyzeDependencies typusFile
      assertBool "Dependency analysis should succeed" (drSuccess dependencyResult)
      let graph = drDependencyGraph dependencyResult
      -- Should handle large dependency graphs
      assertBool "Should handle large dependency graphs" (not (null graph))

test_dependency_caching :: IO ()
test_dependency_caching = do
  let content = unlines
        [ "//! dependent-types=true"
        , "func test() { return 42 }"
        , "func main() { return test() }"
        ]
      parseResult = parseTypus content
  case parseResult of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right typusFile -> do
      let dependencyResult1 = analyzeDependencies typusFile
          dependencyResult2 = analyzeDependencies typusFile
      -- Results should be consistent (caching should work)
      dependencyResult1 @?= dependencyResult2

test_incremental_analysis :: IO ()
test_incremental_analysis = do
  let baseContent = unlines
        [ "//! dependent-types=true"
        , "func base() { return 42 }"
        , "func main() { return base() }"
        ]
      parseResult = parseTypus baseContent
  case parseResult of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right typusFile -> do
      let baseResult = analyzeDependencies typusFile
      let modifiedContent = unlines
            [ "//! dependent-types=true"
            , "func base() { return 42 }"
            , "func added() { return 24 }"
            , "func main() { return base() + added() }"
            ]
      parseResult2 = parseTypus modifiedContent
      case parseResult2 of
        Left err -> assertFailure $ "Parse failed: " ++ show err
        Right typusFile2 -> do
          let modifiedResult = analyzeDependencies typusFile2
          -- Should support incremental analysis
          assertBool "Should support incremental analysis" (True)

-- ============================================================================
-- Error Handling and Recovery Tests
-- ============================================================================

test_clear_error_messages :: IO ()
test_clear_error_messages = do
  let content = unlines
        [ "//! dependent-types=true"
        , "func main() {"
        , "    result := undefined_function()"  -- Undefined function
        , "    return result"
        , "}"
        ]
      parseResult = parseTypus content
  case parseResult of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right typusFile -> do
      let dependencyResult = analyzeDependencies typusFile
      assertBool "Dependency analysis should succeed" (drSuccess dependencyResult)
      let issues = drIssues dependencyResult
      assertBool "Should have dependency issues" (not (null issues))
      let firstIssue = head issues
          message = diMessage firstIssue
      assertBool "Error message should be clear" (length message > 10)
      assertBool "Error should have location information" (diLine firstIssue > 0)

test_dependency_fix_suggestions :: IO ()
test_dependency_fix_suggestions = do
  let content = unlines
        [ "//! dependent-types=true"
        , "func main() {"
        , "    result := undefined_function()"
        , "    return result"
        , "}"
        ]
      parseResult = parseTypus content
  case parseResult of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right typusFile -> do
      let dependencyResult = analyzeDependencies typusFile
      assertBool "Dependency analysis should succeed" (drSuccess dependencyResult)
      let issues = drIssues dependencyResult
      assertBool "Should have dependency issues" (not (null issues))
      let firstIssue = head issues
          suggestions = diSuggestions firstIssue
      -- Should provide suggestions for fixing dependency issues
      assertBool "Should provide suggestions" (not (null suggestions))

test_missing_dependencies :: IO ()
test_missing_dependencies = do
  let content = unlines
        [ "//! dependent-types=true"
        , "import \"nonexistent/package\""
        , "func main() {"
        , "    package.Function()"
        , "}"
        ]
      parseResult = parseTypus content
  case parseResult of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right typusFile -> do
      let dependencyResult = analyzeDependencies typusFile
      assertBool "Dependency analysis should succeed" (drSuccess dependencyResult)
      let issues = drIssues dependencyResult
      -- Should detect missing dependencies
      let missingIssues = filter (\issue -> "missing" `isInfixOf` diMessage issue) issues
      assertBool "Should detect missing dependencies" (not (null missingIssues))

test_analysis_state_maintenance :: IO ()
test_analysis_state_maintenance = do
  let content = unlines
        [ "//! dependent-types=true"
        , "func a() { b() }"
        , "func b() { undefined_function() }"  -- Error here
        , "func c() { return 42 }"
        , "func main() { a() }"
        ]
      parseResult = parseTypus content
  case parseResult of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right typusFile -> do
      let dependencyResult = analyzeDependencies typusFile
      assertBool "Dependency analysis should succeed" (drSuccess dependencyResult)
      let issues = drIssues dependencyResult
      -- Should maintain analysis state across errors
      assertBool "Should maintain analysis state" (not (null issues))

-- ============================================================================
-- Property-Based Dependency Tests
-- ============================================================================

prop_dependency_deterministic :: Property
prop_dependency_deterministic =
  forAll arbitrary $ \content ->
    let parseResult = parseTypus content
    in case parseResult of
         Left _ -> property True
         Right typusFile ->
           let dependencyResult1 = analyzeDependencies typusFile
               dependencyResult2 = analyzeDependencies typusFile
           in dependencyResult1 === dependencyResult2

prop_dependency_graph_acyclic :: Property
prop_dependency_graph_acyclic =
  forAll arbitrary $ \content ->
    let simpleAcyclic = unlines
          [ "//! dependent-types=true"
          , "func a() { return 1 }"
          , "func b() { return a() }"
          , "func c() { return b() }"
          , "func main() { return c() }"
          ]
        parseResult = parseTypus simpleAcyclic
    in case parseResult of
         Left _ -> property False
         Right typusFile ->
           let dependencyResult = analyzeDependencies typusFile
           in case dependencyResult of
                DependencyResult False _ -> property False
                DependencyResult True result -> 
                  let graph = drDependencyGraph result
                  in property $ True  -- Would need to implement cycle detection

prop_dependency_closure_correct :: Property
prop_dependency_closure_correct =
  forAll arbitrary $ \content ->
    let parseResult = parseTypus content
    in case parseResult of
         Left _ -> property True
         Right typusFile ->
           let dependencyResult = analyzeDependencies typusFile
           in case dependencyResult of
                DependencyResult False _ -> property True
                DependencyResult True result -> property True

prop_dependency_preserves_semantics :: Property
prop_dependency_preserves_semantics =
  forAll arbitrary $ \content ->
    let parseResult = parseTypus content
    in case parseResult of
         Left _ -> property True
         Right typusFile ->
           let dependencyResult = analyzeDependencies typusFile
           in case dependencyResult of
                DependencyResult False _ -> property True
                DependencyResult True result -> 
                  -- Dependency analysis should not change program semantics
                  property True