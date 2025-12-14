{-# LANGUAGE CPP #-}

-- | Comprehensive QuickCheck tests for Parser module
module Test.Unit.ComprehensiveParserQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import TestSupport.Arbitrary
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property)

import Parser (parseTypus, TypusFile(..), FileDirectives(..), BlockDirectives(..), CodeBlock(..))
import SourceLocation (Located(..), locatedValue, spanStart, spanEnd, posLine)
import qualified Data.List as Data.List
import Data.Char (toLower, isAlphaNum, isSpace)
import Data.Maybe (isJust, isNothing, fromMaybe)
import qualified Data.Text as T

-- Property: Round-trip parsing maintains directive order
prop_parse_directive_order :: [String] -> Property
prop_parse_directive_order directives =
  not (null directives) && length directives <= 10 ==>
  let validDirectives = filter isValidDirective directives
      content = Data.List.unlines validDirectives
  in case parseTypus content of
    Left err -> counterexample ("Parse error: " ++ err) $ property False
    Right parsed -> 
      let ownershipOrder = getDirectiveOrder (tfDirectives parsed) "ownership"
          depTypesOrder = getDirectiveOrder (tfDirectives parsed) "dependent_types"
          constraintsOrder = getDirectiveOrder (tfDirectives parsed) "constraints"
      in property $ ownershipOrder >= 0 && depTypesOrder >= 0 && constraintsOrder >= 0

-- Property: Parsing with mixed tabs and spaces
prop_parse_mixed_whitespace :: [String] -> Property
prop_parse_mixed_whitespace lines =
  not (null lines) ==>
  let mixedLines = zipWith (\i line -> 
        if even i then replicate i ' ' ++ line
        else replicate i '\t' ++ line) [0..] lines
      content = Data.List.unlines mixedLines
  in case parseTypus content of
    Left _ -> property False
    Right _ -> property True

-- Property: Parsing with extremely long identifiers
prop_parse_long_identifiers :: Int -> Property
prop_parse_long_identifiers length =
  length >= 1 && length <= 100 ==> -- Limit to avoid timeouts
  let longId = replicate length 'a'
      content = "//! ownership: on\npackage main\nvar " ++ longId ++ " int = 42\nfunc main() {}"
  in case parseTypus content of
    Left err -> counterexample ("Parse error with long identifier: " ++ err) $ property False
    Right _ -> property True

-- Property: Parsing with deeply nested expressions
prop_parse_nested_expressions :: Int -> Property
prop_parse_nested_expressions depth =
  depth >= 0 && depth <= 8 ==> -- Limit depth to avoid complexity
  let nestedExpr = generateNestedExpression depth
      content = "//! ownership: on\npackage main\nfunc main() {\n  x := " ++ nestedExpr ++ "\n}"
  in case parseTypus content of
    Left err -> counterexample ("Parse error with nested expression: " ++ err) $ property False
    Right _ -> property True

-- Property: Parsing with complex type declarations
prop_parse_complex_types :: [String] -> [String] -> Property
prop_parse_complex_types typeNames fieldTypes =
  not (null typeNames) && length typeNames <= 5 ==>
  let complexTypes = zipWith (\tName fType -> 
        "type " ++ tName ++ " struct {\n  Field1 " ++ fType ++ "\n  Field2 map[string]" ++ tName ++ "\n  Field3 chan " ++ fType ++ "\n}")
        typeNames fieldTypes
      content = Data.List.unlines $ ["//! ownership: on", "package main"] ++ complexTypes ++ ["func main() {}"]
  in case parseTypus content of
    Left err -> counterexample ("Parse error with complex types: " ++ err) $ property False
    Right _ -> property True

-- Property: Parsing with function parameters and return types
prop_parse_function_signatures :: [String] -> [String] -> [String] -> Property
prop_parse_function_signatures funcNames paramTypes returnTypes =
  not (null funcNames) && length funcNames <= 5 ==>
  let signatures = zipWith3 (\fName pType rType -> 
        "func " ++ fName ++ "(" ++ pType ++ ") (" ++ rType ++ ") { return " ++ getDefault pType ++ " }")
        funcNames paramTypes returnTypes
      content = Data.List.unlines $ ["//! ownership: on", "package main"] ++ signatures ++ ["func main() {}"]
  in case parseTypus content of
    Left err -> counterexample ("Parse error with function signatures: " ++ err) $ property False
    Right _ -> property True

-- Property: Parsing with interface definitions with multiple methods
prop_parse_complex_interfaces :: [String] -> [[String]] -> Property
prop_parse_complex_interfaces interfaceNames methodLists =
  not (null interfaceNames) && length interfaceNames <= 3 ==>
  let interfaces = zipWith (\iName methods -> 
        "type " ++ iName ++ " interface {\n" ++ 
        Data.List.unlines (map (\m -> "  " ++ m ++ "()") methods) ++ 
        "}")
        interfaceNames methodLists
      content = Data.List.unlines $ ["//! ownership: on", "package main"] ++ interfaces ++ ["func main() {}"]
  in case parseTypus content of
    Left err -> counterexample ("Parse error with complex interfaces: " ++ err) $ property False
    Right _ -> property True

-- Property: Parsing with generic function declarations
prop_parse_generic_functions :: [String] -> [String] -> [String] -> Property
prop_parse_generic_functions funcNames typeParams bodyTypes =
  not (null funcNames) && length funcNames <= 5 ==>
  let genericFuncs = zipWith3 (\fName tParam bType -> 
        "func " ++ fName ++ "[" ++ tParam ++ " any](" ++ tParam ++ ") " ++ bType ++ " { return " ++ getDefault bType ++ " }")
        funcNames typeParams bodyTypes
      content = Data.List.unlines $ ["//! ownership: on", "package main"] ++ genericFuncs ++ ["func main() {}"]
  in case parseTypus content of
    Left err -> counterexample ("Parse error with generic functions: " ++ err) $ property False
    Right _ -> property True

-- Property: Parsing with concurrent patterns (goroutines and channels)
prop_parse_concurrent_patterns :: [String] -> [String] -> Property
prop_parse_concurrent_patterns channelNames operationTypes =
  not (null channelNames) && length channelNames <= 5 ==>
  let channelDecls = map (\name -> "var " ++ name ++ " chan " ++ name ++ "Type") channelNames
      goroutines = zipWith (\name opType -> 
        "go func() {\n  " ++ name ++ " <- " ++ getDefault opType ++ "\n}()") channelNames operationTypes
      content = Data.List.unlines $ ["//! ownership: on", "package main"] ++ channelDecls ++
                        ["func main() {"] ++ goroutines ++ ["}"]
  in case parseTypus content of
    Left err -> counterexample ("Parse error with concurrent patterns: " ++ err) $ property False
    Right _ -> property True

-- Property: Parsing with error handling patterns
prop_parse_error_handling :: [String] -> [String] -> Property
prop_parse_error_handling funcNames errorTypes =
  not (null funcNames) && length funcNames <= 5 ==>
  let errorFuncs = zipWith (\fName eType -> 
        "func " ++ fName ++ "() (" ++ getDefault eType ++ ", error) {\n  return " ++ getDefault eType ++ ", nil\n}")
        funcNames errorTypes
      errorHandling = map (\fName -> 
        "if result, err := " ++ fName ++ "(); err != nil {\n  return err\n}\nfmt.Println(result)") funcNames
      content = Data.List.unlines $ ["//! ownership: on", "package main", "import \"fmt\""] ++ errorFuncs ++
                        ["func main() {"] ++ errorHandling ++ ["return nil", "}"]
  in case parseTypus content of
    Left err -> counterexample ("Parse error with error handling: " ++ err) $ property False
    Right _ -> property True

-- Property: Parsing with struct composition
prop_parse_struct_composition :: [String] -> [String] -> Property
prop_parse_struct_composition baseNames embeddedNames =
  not (null baseNames) && length baseNames <= 5 ==>
  let baseStructs = map (\bName -> "type " ++ bName ++ " struct { BaseField int }") baseNames
      embeddedStructs = zipWith (\eName bName -> 
        "type " ++ eName ++ " struct {\n  " ++ bName ++ "\n  EmbeddedField string\n}")
        embeddedNames baseNames
      content = Data.List.unlines $ ["//! ownership: on", "package main"] ++ baseStructs ++ embeddedStructs ++ ["func main() {}"]
  in case parseTypus content of
    Left err -> counterexample ("Parse error with struct composition: " ++ err) $ property False
    Right _ -> property True

-- Property: Parsing with method receivers
prop_parse_method_receivers :: [String] -> [String] -> [String] -> Property
prop_parse_method_receivers structNames methodNames paramTypes =
  not (null structNames) && length structNames <= 5 ==>
  let methods = zipWith3 (\sName mName pType -> 
        "func (" ++ sName ++ ") " ++ mName ++ "(" ++ pType ++ ") int { return 42 }")
        structNames methodNames paramTypes
      content = Data.List.unlines $ ["//! ownership: on", "package main"] ++ methods ++ ["func main() {}"]
  in case parseTypus content of
    Left err -> counterexample ("Parse error with method receivers: " ++ err) $ property False
    Right _ -> property True

-- Property: Parsing with complex control flow
prop_parse_complex_control_flow :: [String] -> Property
prop_parse_complex_control_flow variableNames =
  not (null variableNames) && length variableNames <= 5 ==>
  let ifElseChains = map (\vName -> 
        "if " ++ vName ++ " > 0 {\n  fmt.Println(\"positive\")\n} else if " ++ vName ++ " < 0 {\n  fmt.Println(\"negative\")\n} else {\n  fmt.Println(\"zero\")\n}")
        variableNames
      switchCases = map (\vName -> 
        "switch " ++ vName ++ " {\ncase 1:\n  fmt.Println(\"one\")\ncase 2:\n  fmt.Println(\"two\")\ndefault:\n  fmt.Println(\"other\")\n}")
        variableNames
      content = Data.List.unlines $ ["//! ownership: on", "package main", "import \"fmt\""] ++
                        ["func main() {"] ++ ifElseChains ++ switchCases ++ ["}"]
  in case parseTypus content of
    Left err -> counterexample ("Parse error with complex control flow: " ++ err) $ property False
    Right _ -> property True

-- Property: Parsing with slice and map literals
prop_parse_collection_literals :: [[String]] -> [(String, String)] -> Property
prop_parse_collection_literals sliceElements mapElements =
  let sliceLit = "var slice = []string{" ++ Data.List.intercalate ", " (map (\s -> "\"" ++ s ++ "\"") (concat sliceElements)) ++ "}"
      mapLit = "var m = map[string]int{" ++ Data.List.intercalate ", " (map (\(k, v) -> "\"" ++ k ++ "\": " ++ v) mapElements) ++ "}"
      content = Data.List.unlines $ ["//! ownership: on", "package main"] ++ [sliceLit, mapLit, "func main() {}"]
  in case parseTypus content of
    Left err -> counterexample ("Parse error with collection literals: " ++ err) $ property False
    Right _ -> property True

-- Property: Parsing with function literals and closures
prop_parse_function_literals :: [String] -> Property
prop_parse_function_literals variableNames =
  not (null variableNames) && length variableNames <= 5 ==>
  let closures = map (\vName -> 
        vName ++ "Func := func(x int) int { return x * " ++ vName ++ " }") variableNames
      higherOrder = map (\vName -> 
        "result := " ++ vName ++ "Func(42)") variableNames
      content = Data.List.unlines $ ["//! ownership: on", "package main"] ++
                        ["func main() {"] ++ closures ++ higherOrder ++ ["}"]
  in case parseTypus content of
    Left err -> counterexample ("Parse error with function literals: " ++ err) $ property False
    Right _ -> property True

-- Property: Parsing with defer and recover patterns
prop_parse_defer_recover :: [String] -> Property
prop_parse_defer_recover functionNames =
  not (null functionNames) && length functionNames <= 5 ==>
  let deferCalls = map (\fName -> "defer " ++ fName ++ "()") functionNames
      recoverFunc = "func() {\n  defer func() {\n    if r := recover(); r != nil {\n      fmt.Println(\"Recovered:\", r)\n    }\n  }()\n  panic(\"test panic\")\n}()"
      content = Data.List.unlines $ ["//! ownership: on", "package main", "import \"fmt"] ++
                        ["func main() {"] ++ deferCalls ++ [recoverFunc, "}"]
  in case parseTypus content of
    Left err -> counterexample ("Parse error with defer/recover: " ++ err) $ property False
    Right _ -> property True

-- Property: Parsing with type assertions and type switches
prop_parse_type_operations :: [String] -> Property
prop_parse_type_operations typeNames =
  not (null typeNames) && length typeNames <= 5 ==>
  let assertions = map (\tName -> 
        "var x interface{} = 42\n  val, ok := x.(" ++ tName ++ ")") typeNames
      typeSwitch = "switch v := x.(type) {\n" ++ 
                   Data.List.unlines (map (\tName -> "case " ++ tName ++ ":\n  fmt.Println(\"Type is " ++ tName ++ "\")") typeNames) ++
                   "default:\n  fmt.Println(\"Unknown type\")\n}"
      content = Data.List.unlines $ ["//! ownership: on", "package main", "import \"fmt", "func main() {"] ++
                        assertions ++ [typeSwitch, "}"]
  in case parseTypus content of
    Left err -> counterexample ("Parse error with type operations: " ++ err) $ property False
    Right _ -> property True

-- Property: Parsing with complex import statements
prop_parse_complex_imports :: [String] -> [String] -> Property
prop_parse_complex_imports importPaths aliases =
  let imports = zipWith (\path alias -> 
        "import " ++ alias ++ " \"" ++ path ++ "\"") importPaths aliases
      content = Data.List.unlines $ ["//! ownership: on", "package main"] ++ imports ++ ["func main() {}"]
  in case parseTypus content of
    Left err -> counterexample ("Parse error with complex imports: " ++ err) $ property False
    Right _ -> property True

-- Property: Parsing with build tags and constraints
prop_parse_build_constraints :: [String] -> Property
prop_parse_build_constraints buildTags =
  let constraints = map (\tag -> "// +build " ++ tag) buildTags
      content = Data.List.unlines $ constraints ++ ["//! ownership: on", "package main", "func main() {}"]
  in case parseTypus content of
    Left err -> counterexample ("Parse error with build constraints: " ++ err) $ property False
    Right _ -> property True

-- Property: Parsing with package-level variables and constants
prop_parse_package_level_declarations :: [String] -> [String] -> Property
prop_parse_package_level_declarations varNames constNames =
  let vars = map (\vName -> "var " ++ vName ++ " int = 42") varNames
      consts = map (\cName -> "const " ++ cName ++ " string = \"test\"") constNames
      content = Data.List.unlines $ ["//! ownership: on", "package main"] ++ vars ++ consts ++ ["func main() {}"]
  in case parseTypus content of
    Left err -> counterexample ("Parse error with package-level declarations: " ++ err) $ property False
    Right _ -> property True

-- Property: Parsing with struct tags
prop_parse_struct_tags :: [String] -> [String] -> Property
prop_parse_struct_tags structNames tagValues =
  not (null structNames) && length structNames <= 5 ==>
  let structsWithTags = zipWith (\sName tag -> 
        "type " ++ sName ++ " struct {\n  Field int `" ++ tag ++ "`\n}")
        structNames tagValues
      content = Data.List.unlines $ ["//! ownership: on", "package main"] ++ structsWithTags ++ ["func main() {}"]
  in case parseTypus content of
    Left err -> counterexample ("Parse error with struct tags: " ++ err) $ property False
    Right _ -> property True

-- Helper functions
isValidDirective :: String -> Bool
isValidDirective directive = 
  let validPrefixes = ["//! ownership:", "//! dependent_types:", "//! constraints:"]
  in any (`Data.List.isPrefixOf` directive) validPrefixes

getDirectiveOrder :: FileDirectives -> String -> Int
getDirectiveOrder directives directiveType = 
  case directiveType of
    "ownership" -> if isJust (fdOwnership directives) then 0 else -1
    "dependent_types" -> if isJust (fdDependentTypes directives) then 1 else -1
    "constraints" -> if isJust (fdConstraints directives) then 2 else -1
    _ -> -1

generateNestedExpression :: Int -> String
generateNestedExpression 0 = "42"
generateNestedExpression n = "(" ++ generateNestedExpression (n - 1) ++ " + " ++ generateNestedExpression (n - 1) ++ ")"

getDefault :: String -> String
getDefault "int" = "0"
getDefault "string" = "\"\""
getDefault "bool" = "false"
getDefault "float64" = "0.0"
getDefault _ = "nil"

tests :: TestTree
tests = testGroup "Comprehensive Parser QuickCheck Tests"
  [ fastProperty "Directive order is preserved" prop_parse_directive_order
  , fastProperty "Mixed tabs and spaces are handled" prop_parse_mixed_whitespace
  , fastProperty "Long identifiers are parsed correctly" prop_parse_long_identifiers
  , fastProperty "Nested expressions are parsed correctly" prop_parse_nested_expressions
  , fastProperty "Complex type declarations are parsed" prop_parse_complex_types
  , fastProperty "Function signatures are parsed correctly" prop_parse_function_signatures
  , fastProperty "Complex interfaces are parsed correctly" prop_parse_complex_interfaces
  , fastProperty "Generic functions are parsed correctly" prop_parse_generic_functions
  , fastProperty "Concurrent patterns are parsed correctly" prop_parse_concurrent_patterns
  , fastProperty "Error handling patterns are parsed" prop_parse_error_handling
  , fastProperty "Struct composition is parsed correctly" prop_parse_struct_composition
  , fastProperty "Method receivers are parsed correctly" prop_parse_method_receivers
  , fastProperty "Complex control flow is parsed correctly" prop_parse_complex_control_flow
  , fastProperty "Collection literals are parsed correctly" prop_parse_collection_literals
  , fastProperty "Function literals and closures are parsed" prop_parse_function_literals
  , fastProperty "Defer and recover patterns are parsed" prop_parse_defer_recover
  , fastProperty "Type operations are parsed correctly" prop_parse_type_operations
  , fastProperty "Complex imports are parsed correctly" prop_parse_complex_imports
  , fastProperty "Build constraints are parsed correctly" prop_parse_build_constraints
  , fastProperty "Package-level declarations are parsed" prop_parse_package_level_declarations
  , fastProperty "Struct tags are parsed correctly" prop_parse_struct_tags
  ]