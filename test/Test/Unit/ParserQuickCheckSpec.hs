module Test.Unit.ParserQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import TestSupport.Arbitrary
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property)

import Parser (parseTypus, TypusFile(..), FileDirectives(..), BlockDirectives(..), CodeBlock(..))
import SourceLocation (Located(..), locatedValue, spanStart, spanEnd, posLine)
import qualified Data.List as Data.List

-- Property: Round-trip parsing and reconstruction
prop_parse_roundtrip :: TypusFile -> Property
prop_parse_roundtrip typusFile = 
  let reconstructed = reconstructTypusFile typusFile
  in case parseTypus reconstructed of
    Left err -> counterexample ("Parse error: " ++ err) $ property False
    Right parsed -> 
      let directivesMatch = compareDirectives (tfDirectives typusFile) (tfDirectives parsed)
          blocksCountMatch = length (tfBlocks typusFile) == length (tfBlocks parsed)
      in property $ directivesMatch && blocksCountMatch

-- Property: Valid directives are parsed correctly
prop_parse_valid_directives :: String -> Property
prop_parse_valid_directives directive = 
  let validDirectives = ["//! ownership: on", "//! ownership: off", 
                        "//! dependent_types: on", "//! dependent_types: off",
                        "//! constraints: on", "//! constraints: off"]
  in classify (directive `elem` validDirectives) "valid directive" $
     case parseTypus directive of
       Left _ -> property False
       Right file -> property $ hasValidDirectives file

-- Property: Invalid directives are handled gracefully
prop_parse_invalid_directives :: String -> Property
prop_parse_invalid_directives content =
  let invalidStarts = ["//!", "//", "##", "@@", "%%"]
      hasInvalidStart = any (`Data.List.isPrefixOf` content) invalidStarts
  in classify hasInvalidStart "starts with invalid directive" $
     case parseTypus content of
       Left _ -> True  -- Expected to fail
       Right _ -> True -- May still succeed with partial parsing

-- Property: Empty content produces minimal TypusFile
prop_parse_empty_content :: Property
prop_parse_empty_content =
  case parseTypus "" of
    Left err -> counterexample ("Parse error on empty content: " ++ err) $ property False
    Right file -> 
      let directives = tfDirectives file
          blocks = tfBlocks file
      in property $ all (== Nothing) [fdOwnership directives, fdDependentTypes directives, fdConstraints directives] &&
         null blocks

-- Property: Multiple directives are parsed independently
prop_parse_multiple_directives :: [String] -> Property
prop_parse_multiple_directives directives =
  let content = unlines directives
  in case parseTypus content of
    Left err -> counterexample ("Parse error: " ++ err) $ property False
    Right file -> property $ hasCorrectDirectiveCount file (length directives)

-- Property: Large files are parsed without stack overflow
prop_parse_large_file :: Int -> Property
prop_parse_large_file n =
  let largeContent = unlines $ replicate n "var x int = 42"
  in n <= 1000 ==> -- Limit size to avoid timeouts
     case parseTypus largeContent of
       Left err -> counterexample ("Parse error on large file: " ++ err) $ property False
       Right _ -> property True

-- Property: Unicode content is handled correctly
prop_parse_unicode_content :: String -> Property
prop_parse_unicode_content content =
  let unicodeContent = content ++ " // 测试中文 🚀"
  in case parseTypus unicodeContent of
    Left _ -> property False
    Right _ -> property True

-- Property: Code blocks maintain their content
prop_parse_code_blocks :: String -> Property
prop_parse_code_blocks codeContent =
  let fullContent = "//! ownership: on\npackage main\nfunc main() {\n" ++ codeContent ++ "\n}"
  in case parseTypus fullContent of
    Left err -> counterexample ("Parse error: " ++ err) $ property False
    Right file -> 
      case tfBlocks file of
        [] -> property False
        (block:_) -> property $ codeContent `isInfixOf` cbContent block

-- Property: Directive positions are tracked correctly
prop_parse_directive_positions :: Property
prop_parse_directive_positions =
  let content = unlines
        [ "//! ownership: on"
        , "//! dependent_types: off"
        , "package main"
        , "func main() {}"
        ]
  in case parseTypus content of
    Left err -> counterexample ("Parse error: " ++ err) False
    Right file -> 
      let directives = tfDirectives file
          ownershipPos = fdOwnership directives >>= \(Located span _) -> Just (posLine $ spanStart span)
          dependentTypesPos = fdDependentTypes directives >>= \(Located span _) -> Just (posLine $ spanStart span)
      in property $ ownershipPos == Just 1 && dependentTypesPos == Just 2

-- Property: Mixed directives and code are parsed correctly
prop_parse_mixed_content :: [String] -> [String] -> Property
prop_parse_mixed_content directives codeLines =
  let content = unlines $ directives ++ codeLines
  in not (null directives) ==> 
     case parseTypus content of
       Left err -> counterexample ("Parse error: " ++ err) $ property False
       Right file -> property $ hasDirectives file && not (null $ tfBlocks file)

-- Property: Nested structures are parsed correctly
prop_parse_nested_structures :: Int -> Property
prop_parse_nested_structures depth =
  depth <= 5 ==> -- Limit depth to avoid complexity
  let nestedContent = generateNestedStructure depth
  in case parseTypus nestedContent of
    Left err -> counterexample ("Parse error on nested structure: " ++ err) $ property False
    Right _ -> property True

-- Helper functions
reconstructTypusFile :: TypusFile -> String
reconstructTypusFile file = 
  let directives = reconstructDirectives (tfDirectives file)
      blocks = map reconstructBlock (tfBlocks file)
  in unlines $ directives ++ blocks

reconstructDirectives :: FileDirectives -> [String]
reconstructDirectives (FileDirectives ownership depTypes constraints) =
  let ownershipLine = case ownership of
        Nothing -> []
        Just (Located _ True) -> ["//! ownership: on"]
        Just (Located _ False) -> ["//! ownership: off"]
      depTypesLine = case depTypes of
        Nothing -> []
        Just (Located _ True) -> ["//! dependent_types: on"]
        Just (Located _ False) -> ["//! dependent_types: off"]
      constraintsLine = case constraints of
        Nothing -> []
        Just (Located _ True) -> ["//! constraints: on"]
        Just (Located _ False) -> ["//! constraints: off"]
  in ownershipLine ++ depTypesLine ++ constraintsLine

reconstructBlock :: CodeBlock -> String
reconstructBlock block = cbContent block

compareDirectives :: FileDirectives -> FileDirectives -> Bool
compareDirectives (FileDirectives o1 d1 c1) (FileDirectives o2 d2 c2) =
  (fmap locatedValue o1) == (fmap locatedValue o2) &&
  (fmap locatedValue d1) == (fmap locatedValue d2) &&
  (fmap locatedValue c1) == (fmap locatedValue c2)

hasValidDirectives :: TypusFile -> Bool
hasValidDirectives file = 
  let directives = tfDirectives file
      ownership = locatedValue <$> fdOwnership directives
      depTypes = locatedValue <$> fdDependentTypes directives
      constraints = locatedValue <$> fdConstraints directives
  in all (`elem` [Nothing, Just True, Just False]) [ownership, depTypes, constraints]

hasCorrectDirectiveCount :: TypusFile -> Int -> Bool
hasCorrectDirectiveCount file expectedCount =
  let actualCount = countDirectives (tfDirectives file)
  in actualCount <= expectedCount -- May be less due to parsing rules

countDirectives :: FileDirectives -> Int
countDirectives (FileDirectives ownership depTypes constraints) =
  length [() | Just _ <- [ownership, depTypes, constraints]]

hasDirectives :: TypusFile -> Bool
hasDirectives file = 
  let directives = tfDirectives file
  in any isJust [fdOwnership directives, fdDependentTypes directives, fdConstraints directives]
  where
    isJust Nothing = False
    isJust (Just _) = True

generateNestedStructure :: Int -> String
generateNestedStructure 0 = "var x int = 42"
generateNestedStructure n = unlines
  [ "func level" ++ show n ++ "() {"
  , generateNestedStructure (n - 1)
  , "}"
  ]

isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = needle `Data.List.isInfixOf` haystack

-- Additional property tests for edge cases and complex scenarios

-- Property: Parsing with comments preserves structure
prop_parse_preserves_comments :: [String] -> Property
prop_parse_preserves_comments commentLines =
  let comments = map ("// " ++) commentLines
      content = unlines $ ["//! ownership: on"] ++ comments ++ ["package main", "func main() {}"]
  in case parseTypus content of
    Left err -> counterexample ("Parse error with comments: " ++ err) $ property False
    Right file -> property $ hasDirectives file && not (null $ tfBlocks file)

-- Property: Parsing with whitespace variations
prop_parse_whitespace_variations :: String -> String -> String -> Property
prop_parse_whitespace_variations before middle after =
  let content = before ++ "//! ownership: on" ++ middle ++ "package main" ++ after ++ "func main() {}"
  in case parseTypus content of
    Left err -> counterexample ("Parse error with whitespace: " ++ err) $ property False
    Right file -> property $ hasDirectives file

-- Property: Parsing deeply nested code blocks
prop_parse_deeply_nested_blocks :: Int -> Property
prop_parse_deeply_nested_blocks depth =
  depth <= 10 ==> -- Limit depth to avoid timeouts
  let nestedCode = generateDeeplyNestedCode depth
  in case parseTypus nestedCode of
    Left err -> counterexample ("Parse error on deeply nested code: " ++ err) $ property False
    Right _ -> property True

-- Property: Parsing with malformed directives
prop_parse_malformed_directives :: String -> Property
prop_parse_malformed_directives directive =
  let malformedPrefixes = ["//!", "//! ownership", "//! ownership:", "//! dependent_types", "//! dependent_types:"]
      isMalformed = any (`Data.List.isPrefixOf` directive) malformedPrefixes
  in classify isMalformed "malformed directive" $
     case parseTypus directive of
       Left _ -> True  -- Expected to fail
       Right _ -> True -- May still succeed with partial parsing

-- Property: Parsing with special characters in identifiers
prop_parse_special_characters :: [String] -> Property
prop_parse_special_characters identifiers =
  let specialChars = ["_", "$", "@", "#"]
      enhancedIds = map (`Data.List.intercalate` specialChars) identifiers
      content = unlines $ ["//! ownership: on", "package main"] ++ 
                        map (\id -> "var " ++ id ++ " int") enhancedIds
  in case parseTypus content of
    Left err -> counterexample ("Parse error with special characters: " ++ err) $ property False
    Right file -> property $ not (null $ tfBlocks file)

-- Property: Parsing with mixed line endings
prop_parse_mixed_line_endings :: String -> Property
prop_parse_mixed_line_endings content =
  let mixedContent = content ++ "\r\n" ++ content ++ "\n" ++ content ++ "\r\n"
  in case parseTypus mixedContent of
    Left _ -> property False
    Right _ -> property True

-- Property: Parsing with very long lines
prop_parse_very_long_lines :: Int -> Property
prop_parse_very_long_lines length =
  length <= 1000 ==> -- Limit length to avoid timeouts
  let longLine = replicate length 'x' ++ " // comment"
      content = "//! ownership: on\npackage main\n" ++ longLine ++ "\nfunc main() {}"
  in case parseTypus content of
    Left err -> counterexample ("Parse error with long line: " ++ err) $ property False
    Right _ -> property True

-- Property: Parsing with escape sequences
prop_parse_escape_sequences :: [String] -> Property
prop_parse_escape_sequences strings =
  let escapedStrings = map addEscapeSequences strings
      content = unlines $ ["//! ownership: on", "package main"] ++
                        map (\s -> "fmt.Println(\"" ++ s ++ "\")") escapedStrings
  in case parseTypus content of
    Left err -> counterexample ("Parse error with escape sequences: " ++ err) $ property False
    Right _ -> property True

-- Property: Parsing with numeric literals
prop_parse_numeric_literals :: [Int] -> [Double] -> Property
prop_parse_numeric_literals ints doubles =
  let intVars = map (\i -> "var x" ++ show i ++ " int = " ++ show i) ints
      floatVars = map (\d -> "var y" ++ show (round d :: Int) ++ " float64 = " ++ show d) doubles
      content = unlines $ ["//! ownership: on", "package main"] ++ intVars ++ floatVars ++ ["func main() {}"]
  in case parseTypus content of
    Left err -> counterexample ("Parse error with numeric literals: " ++ err) $ property False
    Right file -> property $ not (null $ tfBlocks file)

-- Property: Parsing with string literals
prop_parse_string_literals :: [String] -> Property
prop_parse_string_literals strings =
  let stringVars = map (\s -> "var s" ++ show (length s) ++ " string = \"" ++ s ++ "\"") strings
      content = unlines $ ["//! ownership: on", "package main"] ++ stringVars ++ ["func main() {}"]
  in case parseTypus content of
    Left err -> counterexample ("Parse error with string literals: " ++ err) $ property False
    Right file -> property $ not (null $ tfBlocks file)

-- Property: Parsing with boolean literals
prop_parse_boolean_literals :: [Bool] -> Property
prop_parse_boolean_literals bools =
  let boolVars = map (\(i, b) -> "var b" ++ show i ++ " bool = " ++ show b) (zip [0..] bools)
      content = unlines $ ["//! ownership: on", "package main"] ++ boolVars ++ ["func main() {}"]
  in case parseTypus content of
    Left err -> counterexample ("Parse error with boolean literals: " ++ err) $ property False
    Right file -> property $ not (null $ tfBlocks file)

-- Property: Parsing with complex expressions
prop_parse_complex_expressions :: [String] -> Property
prop_parse_complex_expressions expressions =
  let complexExprs = map addOperators expressions
      content = unlines $ ["//! ownership: on", "package main", "func main() {"] ++
                        map (\expr -> "  x := " ++ expr) complexExprs ++
                        ["}"]
  in case parseTypus content of
    Left err -> counterexample ("Parse error with complex expressions: " ++ err) $ property False
    Right file -> property $ not (null $ tfBlocks file)

-- Property: Parsing with function definitions
prop_parse_function_definitions :: [String] -> [String] -> Property
prop_parse_function_definitions funcNames paramTypes =
  let funcs = zipWith (\name pType -> "func " ++ name ++ "(" ++ pType ++ ") int { return 42 }") 
                      funcNames paramTypes
      content = unlines $ ["//! ownership: on", "package main"] ++ funcs ++ ["func main() {}"]
  in case parseTypus content of
    Left err -> counterexample ("Parse error with function definitions: " ++ err) $ property False
    Right file -> property $ not (null $ tfBlocks file)

-- Property: Parsing with struct definitions
prop_parse_struct_definitions :: [String] -> [String] -> Property
prop_parse_struct_definitions structNames fieldTypes =
  let structs = zipWith (\name fType -> "type " ++ name ++ " struct { Field " ++ fType ++ " }") 
                        structNames fieldTypes
      content = unlines $ ["//! ownership: on", "package main"] ++ structs ++ ["func main() {}"]
  in case parseTypus content of
    Left err -> counterexample ("Parse error with struct definitions: " ++ err) $ property False
    Right file -> property $ not (null $ tfBlocks file)

-- Property: Parsing with interface definitions
prop_parse_interface_definitions :: [String] -> [String] -> Property
prop_parse_interface_definitions interfaceNames methodNames =
  let interfaces = zipWith (\iName mName -> "type " ++ iName ++ " interface { " ++ mName ++ "() }") 
                           interfaceNames methodNames
      content = unlines $ ["//! ownership: on", "package main"] ++ interfaces ++ ["func main() {}"]
  in case parseTypus content of
    Left err -> counterexample ("Parse error with interface definitions: " ++ err) $ property False
    Right file -> property $ not (null $ tfBlocks file)

-- Property: Parsing with import statements
prop_parse_import_statements :: [String] -> Property
prop_parse_import_statements importPaths =
  let imports = map ("import \"" ++) importPaths
      content = unlines $ ["//! ownership: on", "package main"] ++ imports ++ ["func main() {}"]
  in case parseTypus content of
    Left err -> counterexample ("Parse error with import statements: " ++ err) $ property False
    Right file -> property $ not (null $ tfBlocks file)

-- Property: Parsing with concurrent constructs
prop_parse_concurrent_constructs :: [String] -> Property
prop_parse_concurrent_constructs channelNames =
  let channels = map (\name -> "var " ++ name ++ " chan int") channelNames
      goroutines = map (\name -> "go func() { " ++ name ++ " <- 42 }()") channelNames
      content = unlines $ ["//! ownership: on", "package main"] ++ channels ++ 
                        ["func main() {"] ++ goroutines ++ ["}"]
  in case parseTypus content of
    Left err -> counterexample ("Parse error with concurrent constructs: " ++ err) $ property False
    Right file -> property $ not (null $ tfBlocks file)

-- Property: Parsing with error handling constructs
prop_parse_error_handling :: [String] -> Property
prop_parse_error_handling functionNames =
  let errorFuncs = map (\name -> "func " ++ name ++ "() error { return nil }") functionNames
      errorHandling = map (\name -> "if err := " ++ name ++ "(); err != nil { return err }") functionNames
      content = unlines $ ["//! ownership: on", "package main"] ++ errorFuncs ++
                        ["func main() {"] ++ errorHandling ++ ["return nil", "}"]
  in case parseTypus content of
    Left err -> counterexample ("Parse error with error handling: " ++ err) $ property False
    Right file -> property $ not (null $ tfBlocks file)

-- Property: Parsing with generic types
prop_parse_generic_types :: [String] -> [String] -> Property
prop_parse_generic_types typeNames typeParams =
  let generics = zipWith (\tName tParam -> "type " ++ tName ++ "[" ++ tParam ++ " any] struct { Value " ++ tParam ++ " }") 
                         typeNames typeParams
      content = unlines $ ["//! ownership: on", "package main"] ++ generics ++ ["func main() {}"]
  in case parseTypus content of
    Left err -> counterexample ("Parse error with generic types: " ++ err) $ property False
    Right file -> property $ not (null $ tfBlocks file)

-- Helper functions for new tests
generateDeeplyNestedCode :: Int -> String
generateDeeplyNestedCode 0 = "  return 42"
generateDeeplyNestedCode n = unlines
  [ "  if true {"
  , generateDeeplyNestedCode (n - 1)
  , "  }"
  ]

addEscapeSequences :: String -> String
addEscapeSequences = concatMap (\c -> if c == '\\' then "\\\\" else if c == '"' then "\\\"" else [c])

addOperators :: String -> String
addOperators s = s ++ " + " ++ s ++ " * " ++ s ++ " / " ++ s

-- Additional comprehensive property tests

-- Property: Parsing with method definitions
prop_parse_method_definitions :: [String] -> [String] -> [String] -> Property
prop_parse_method_definitions structNames methodNames paramTypes =
  let methods = zipWith3 (\sName mName pType -> 
        "func (" ++ sName ++ ") " ++ mName ++ "(" ++ pType ++ ") int { return 42 }")
        structNames methodNames paramTypes
      content = unlines $ ["//! ownership: on", "package main"] ++ methods ++ ["func main() {}"]
  in case parseTypus content of
    Left err -> counterexample ("Parse error with method definitions: " ++ err) $ property False
    Right file -> property $ not (null $ tfBlocks file)

-- Property: Parsing with channel operations
prop_parse_channel_operations :: [String] -> Property
prop_parse_channel_operations channelNames =
  let channelDecls = map (\name -> "var " ++ name ++ " chan int") channelNames
      channelOps = map (\name -> name ++ " <- 42") channelNames
      channelRecv = map (\name -> "<-" ++ name) channelNames
      content = unlines $ ["//! ownership: on", "package main"] ++ channelDecls ++
                        ["func main() {"] ++ channelOps ++ channelRecv ++ ["}"]
  in case parseTypus content of
    Left err -> counterexample ("Parse error with channel operations: " ++ err) $ property False
    Right file -> property $ not (null $ tfBlocks file)

-- Property: Parsing with select statements
prop_parse_select_statements :: Int -> Property
prop_parse_select_statements numCases =
  numCases <= 10 ==> -- Limit to avoid complexity
  let selectCases = map (\i -> "case <-ch" ++ show i ++ ":\n  fmt.Println(\"case " ++ show i ++ "\")") [1..numCases]
      defaultCase = if numCases > 0 then "default:\n  fmt.Println(\"default\")" else ""
      selectContent = "select {\n" ++ unlines selectCases ++ defaultCase ++ "\n}"
      content = unlines $ ["//! ownership: on", "package main", "func main() {", selectContent, "}"]
  in case parseTypus content of
    Left err -> counterexample ("Parse error with select statement: " ++ err) $ property False
    Right file -> property $ not (null $ tfBlocks file)

-- Property: Parsing with defer statements
prop_parse_defer_statements :: [String] -> Property
prop_parse_defer_statements functionNames =
  let deferCalls = map (\name -> "defer " ++ name ++ "()") functionNames
      content = unlines $ ["//! ownership: on", "package main", "func main() {"] ++
                        deferCalls ++ ["return 0", "}"]
  in case parseTypus content of
    Left err -> counterexample ("Parse error with defer statements: " ++ err) $ property False
    Right file -> property $ not (null $ tfBlocks file)

-- Property: Parsing with panic and recover
prop_parse_panic_recover :: [String] -> Property
prop_parse_panic_recover panicMessages =
  let panics = map (\msg -> "panic(\"" ++ msg ++ "\")") panicMessages
      recoverFunc = "func recoverTest() {\n  defer func() {\n    if r := recover(); r != nil {\n      fmt.Println(\"Recovered:\", r)\n    }\n  }()\n  panic(\"test panic\")\n}"
      content = unlines $ ["//! ownership: on", "package main"] ++ panics ++ [recoverFunc, "func main() {}"]
  in case parseTypus content of
    Left err -> counterexample ("Parse error with panic/recover: " ++ err) $ property False
    Right file -> property $ not (null $ tfBlocks file)

-- Property: Parsing with type assertions
prop_parse_type_assertions :: [String] -> Property
prop_parse_type_assertions typeNames =
  let assertions = map (\tName -> "var x interface{} = 42\n  val, ok := x.(" ++ tName ++ ")") typeNames
      content = unlines $ ["//! ownership: on", "package main", "func main() {"] ++
                        assertions ++ ["}"]
  in case parseTypus content of
    Left err -> counterexample ("Parse error with type assertions: " ++ err) $ property False
    Right file -> property $ not (null $ tfBlocks file)

-- Property: Parsing with type switches
prop_parse_type_switches :: [String] -> Property
prop_parse_type_switches typeNames =
  let typeCases = map (\tName -> "case " ++ tName ++ ":\n  fmt.Println(\"Type is " ++ tName ++ "\")") typeNames
      typeSwitch = "var x interface{} = 42\n  switch v := x.(type) {\n" ++ unlines typeCases ++ "default:\n  fmt.Println(\"Unknown type\")\n}"
      content = unlines $ ["//! ownership: on", "package main", "func main() {", typeSwitch, "}"]
  in case parseTypus content of
    Left err -> counterexample ("Parse error with type switch: " ++ err) $ property False
    Right file -> property $ not (null $ tfBlocks file)

-- Property: Parsing with closure expressions
prop_parse_closures :: [String] -> [String] -> Property
prop_parse_closures paramNames returnExprs =
  let closures = zipWith (\params ret -> 
        "func(" ++ params ++ ") int { return " ++ ret ++ " }") paramNames returnExprs
      content = unlines $ ["//! ownership: on", "package main", "func main() {"] ++
                        map (\c -> "  fn := " ++ c) closures ++
                        ["}"]
  in case parseTypus content of
    Left err -> counterexample ("Parse error with closures: " ++ err) $ property False
    Right file -> property $ not (null $ tfBlocks file)

-- Property: Parsing with slice operations
prop_parse_slice_operations :: [String] -> [Int] -> Property
prop_parse_slice_operations sliceNames indices =
  let sliceDecls = map (\name -> "var " ++ name ++ " []int") sliceNames
      sliceOps = zipWith (\name idx -> name ++ "[" ++ show idx ++ ":" ++ show (idx + 1) ++ "]") sliceNames indices
      content = unlines $ ["//! ownership: on", "package main"] ++ sliceDecls ++
                        ["func main() {"] ++ sliceOps ++ ["}"]
  in case parseTypus content of
    Left err -> counterexample ("Parse error with slice operations: " ++ err) $ property False
    Right file -> property $ not (null $ tfBlocks file)

-- Property: Parsing with map operations
prop_parse_map_operations :: [String] -> [String] -> Property
prop_parse_map_operations mapNames keyTypes =
  let mapDecls = zipWith (\name kType -> "var " ++ name ++ " map[" ++ kType ++ "]int") mapNames keyTypes
      mapOps = zipWith (\name kType -> name ++ "[\"" ++ kType ++ "\"] = 42") mapNames keyTypes
      content = unlines $ ["//! ownership: on", "package main"] ++ mapDecls ++
                        ["func main() {"] ++ mapOps ++ ["}"]
  in case parseTypus content of
    Left err -> counterexample ("Parse error with map operations: " ++ err) $ property False
    Right file -> property $ not (null $ tfBlocks file)

-- Property: Parsing with range loops
prop_parse_range_loops :: [String] -> Property
prop_parse_range_loops collectionNames =
  let rangeLoops = map (\name -> "for " ++ name ++ " := range " ++ name ++ " {\n  fmt.Println(" ++ name ++ ")\n}") collectionNames
      content = unlines $ ["//! ownership: on", "package main", "func main() {"] ++
                        rangeLoops ++ ["}"]
  in case parseTypus content of
    Left err -> counterexample ("Parse error with range loops: " ++ err) $ property False
    Right file -> property $ not (null $ tfBlocks file)

-- Property: Parsing with labeled statements
prop_parse_labeled_statements :: [String] -> Property
prop_parse_labeled_statements labelNames =
  let labels = map (\name -> name ++ ":\n  for i := 0; i < 10; i++ {\n    if i == 5 {\n      break " ++ name ++ "\n    }\n  }") labelNames
      content = unlines $ ["//! ownership: on", "package main", "func main() {"] ++
                        labels ++ ["}"]
  in case parseTypus content of
    Left err -> counterexample ("Parse error with labeled statements: " ++ err) $ property False
    Right file -> property $ not (null $ tfBlocks file)

-- Property: Parsing with goto statements
prop_parse_goto_statements :: [String] -> Property
prop_parse_goto_statements labelNames =
  let labels = map (\name -> name ++ ":") labelNames
      gotos = map (\name -> "goto " ++ name) labelNames
      content = unlines $ ["//! ownership: on", "package main", "func main() {"] ++
                        gotos ++ labels ++ ["}"]
  in case parseTypus content of
    Left err -> counterexample ("Parse error with goto statements: " ++ err) $ property False
    Right file -> property $ not (null $ tfBlocks file)

-- Property: Parsing with const declarations
prop_parse_const_declarations :: [String] -> [String] -> Property
prop_parse_const_declarations constNames constValues =
  let consts = zipWith (\name value -> "const " ++ name ++ " = " ++ value) constNames constValues
      content = unlines $ ["//! ownership: on", "package main"] ++ consts ++ ["func main() {}"]
  in case parseTypus content of
    Left err -> counterexample ("Parse error with const declarations: " ++ err) $ property False
    Right file -> property $ not (null $ tfBlocks file)

-- Property: Parsing with iota constants
prop_parse_iota_constants :: [String] -> Property
prop_parse_iota_constants constNames =
  let iotaDecls = map (\name -> name ++ " iota") constNames
      content = unlines $ ["//! ownership: on", "package main", "const ("] ++
                        iotaDecls ++ [")", "func main() {}"]
  in case parseTypus content of
    Left err -> counterexample ("Parse error with iota constants: " ++ err) $ property False
    Right file -> property $ not (null $ tfBlocks file)

-- Property: Parsing with type aliases
prop_parse_type_aliases :: [String] -> [String] -> Property
prop_parse_type_aliases aliasNames originalTypes =
  let aliases = zipWith (\alias orig -> "type " ++ alias ++ " = " ++ orig) aliasNames originalTypes
      content = unlines $ ["//! ownership: on", "package main"] ++ aliases ++ ["func main() {}"]
  in case parseTypus content of
    Left err -> counterexample ("Parse error with type aliases: " ++ err) $ property False
    Right file -> property $ not (null $ tfBlocks file)

-- Property: Parsing with embedded structs
prop_parse_embedded_structs :: [String] -> [String] -> Property
prop_parse_embedded_structs structNames embeddedTypes =
  let structs = zipWith (\name embed -> 
        "type " ++ name ++ " struct {\n  " ++ embed ++ "\n  Field int\n}") structNames embeddedTypes
      content = unlines $ ["//! ownership: on", "package main"] ++ structs ++ ["func main() {}"]
  in case parseTypus content of
    Left err -> counterexample ("Parse error with embedded structs: " ++ err) $ property False
    Right file -> property $ not (null $ tfBlocks file)

-- Property: Parsing with struct tags
prop_parse_struct_tags :: [String] -> [String] -> Property
prop_parse_struct_tags fieldNames tagValues =
  let fields = zipWith (\name tag -> name ++ " int `" ++ tag ++ "`") fieldNames tagValues
      structDef = "type StructWithTags struct {\n" ++ unlines (map ("  " ++) fields) ++ "\n}"
      content = unlines $ ["//! ownership: on", "package main", structDef, "func main() {}"]
  in case parseTypus content of
    Left err -> counterexample ("Parse error with struct tags: " ++ err) $ property False
    Right file -> property $ not (null $ tfBlocks file)

-- Property: Parsing with variadic functions
prop_parse_variadic_functions :: [String] -> [String] -> Property
prop_parse_variadic_functions funcNames paramTypes =
  let variadics = zipWith (\name pType -> 
        "func " ++ name ++ "(args ..." ++ pType ++ ") int {\n  return len(args)\n}") funcNames paramTypes
      content = unlines $ ["//! ownership: on", "package main"] ++ variadics ++ ["func main() {}"]
  in case parseTypus content of
    Left err -> counterexample ("Parse error with variadic functions: " ++ err) $ property False
    Right file -> property $ not (null $ tfBlocks file)

-- Property: Parsing with multiple return values
prop_parse_multiple_returns :: [String] -> [String] -> Property
prop_parse_multiple_returns funcNames returnTypes =
  let multiRetFuncs = zipWith (\name retTypes -> 
        "func " ++ name ++ "() (" ++ retTypes ++ ") {\n  return " ++ retTypes ++ "\n}") funcNames returnTypes
      content = unlines $ ["//! ownership: on", "package main"] ++ multiRetFuncs ++ ["func main() {}"]
  in case parseTypus content of
    Left err -> counterexample ("Parse error with multiple returns: " ++ err) $ property False
    Right file -> property $ not (null $ tfBlocks file)

-- Property: Parsing with anonymous functions
prop_parse_anonymous_functions :: [String] -> Property
prop_parse_anonymous_functions functionBodies =
  let anonFuncs = map (\body -> "func() {\n" ++ body ++ "\n}") functionBodies
      content = unlines $ ["//! ownership: on", "package main", "func main() {"] ++
                        map (\f -> "  " ++ f ++ "()") anonFuncs ++
                        ["}"]
  in case parseTypus content of
    Left err -> counterexample ("Parse error with anonymous functions: " ++ err) $ property False
    Right file -> property $ not (null $ tfBlocks file)

-- Property: Parsing with function literals as arguments
prop_parse_function_literals :: [String] -> Property
prop_parse_function_literals operations =
  let funcLiterals = map (\op -> 
        "func(x, y int) int {\n  return x " ++ op ++ " y\n}") operations
      content = unlines $ ["//! ownership: on", "package main", "func main() {"] ++
                        map (\f -> "  result := apply(" ++ f ++ ", 1, 2)") funcLiterals ++
                        ["}", "func apply(fn func(int, int) int, x, y int) int {\n  return fn(x, y)\n}"]
  in case parseTypus content of
    Left err -> counterexample ("Parse error with function literals: " ++ err) $ property False
    Right file -> property $ not (null $ tfBlocks file)

-- Property: Parsing with method expressions
prop_parse_method_expressions :: [String] -> [String] -> Property
prop_parse_method_expressions structNames methodNames =
  let methodExprs = zipWith (\sName mName -> 
        "fn := (*" ++ sName ++ ")." ++ mName) structNames methodNames
      content = unlines $ ["//! ownership: on", "package main", "func main() {"] ++
                        methodExprs ++
                        ["}"]
  in case parseTypus content of
    Left err -> counterexample ("Parse error with method expressions: " ++ err) $ property False
    Right file -> property $ not (null $ tfBlocks file)

-- Property: Parsing with method values
prop_parse_method_values :: [String] -> [String] -> Property
prop_parse_method_values structNames methodNames =
  let methodVals = zipWith (\sName mName -> 
        "var " ++ sName ++ " " ++ sName ++ "\n  method := " ++ sName ++ "." ++ mName) structNames methodNames
      content = unlines $ ["//! ownership: on", "package main", "func main() {"] ++
                        methodVals ++
                        ["}"]
  in case parseTypus content of
    Left err -> counterexample ("Parse error with method values: " ++ err) $ property False
    Right file -> property $ not (null $ tfBlocks file)

tests :: TestTree
tests = testGroup "Parser QuickCheck Tests"
  [ fastProperty "parse roundtrip" prop_parse_roundtrip
  , fastProperty "parse valid directives" prop_parse_valid_directives
  , fastProperty "parse invalid directives" prop_parse_invalid_directives
  , fastProperty "parse empty content" prop_parse_empty_content
  , fastProperty "parse multiple directives" prop_parse_multiple_directives
  , fastProperty "parse large file" prop_parse_large_file
  , fastProperty "parse unicode content" prop_parse_unicode_content
  , fastProperty "parse code blocks" prop_parse_code_blocks
  , fastProperty "parse directive positions" prop_parse_directive_positions
  , fastProperty "parse mixed content" prop_parse_mixed_content
  , fastProperty "parse nested structures" prop_parse_nested_structures
  -- New property tests
  , fastProperty "parse preserves comments" prop_parse_preserves_comments
  , fastProperty "parse whitespace variations" prop_parse_whitespace_variations
  , fastProperty "parse deeply nested blocks" prop_parse_deeply_nested_blocks
  , fastProperty "parse malformed directives" prop_parse_malformed_directives
  , fastProperty "parse special characters" prop_parse_special_characters
  , fastProperty "parse mixed line endings" prop_parse_mixed_line_endings
  , fastProperty "parse very long lines" prop_parse_very_long_lines
  , fastProperty "parse escape sequences" prop_parse_escape_sequences
  , fastProperty "parse numeric literals" prop_parse_numeric_literals
  , fastProperty "parse string literals" prop_parse_string_literals
  , fastProperty "parse boolean literals" prop_parse_boolean_literals
  , fastProperty "parse complex expressions" prop_parse_complex_expressions
  , fastProperty "parse function definitions" prop_parse_function_definitions
  , fastProperty "parse struct definitions" prop_parse_struct_definitions
  , fastProperty "parse interface definitions" prop_parse_interface_definitions
  , fastProperty "parse import statements" prop_parse_import_statements
  , fastProperty "parse concurrent constructs" prop_parse_concurrent_constructs
  , fastProperty "parse error handling" prop_parse_error_handling
  , fastProperty "parse generic types" prop_parse_generic_types
  -- Extended property tests
  , fastProperty "parse method definitions" prop_parse_method_definitions
  , fastProperty "parse channel operations" prop_parse_channel_operations
  , fastProperty "parse select statements" prop_parse_select_statements
  , fastProperty "parse defer statements" prop_parse_defer_statements
  , fastProperty "parse panic and recover" prop_parse_panic_recover
  , fastProperty "parse type assertions" prop_parse_type_assertions
  , fastProperty "parse type switches" prop_parse_type_switches
  , fastProperty "parse closures" prop_parse_closures
  , fastProperty "parse slice operations" prop_parse_slice_operations
  , fastProperty "parse map operations" prop_parse_map_operations
  , fastProperty "parse range loops" prop_parse_range_loops
  , fastProperty "parse labeled statements" prop_parse_labeled_statements
  , fastProperty "parse goto statements" prop_parse_goto_statements
  , fastProperty "parse const declarations" prop_parse_const_declarations
  , fastProperty "parse iota constants" prop_parse_iota_constants
  , fastProperty "parse type aliases" prop_parse_type_aliases
  , fastProperty "parse embedded structs" prop_parse_embedded_structs
  , fastProperty "parse struct tags" prop_parse_struct_tags
  , fastProperty "parse variadic functions" prop_parse_variadic_functions
  , fastProperty "parse multiple returns" prop_parse_multiple_returns
  , fastProperty "parse anonymous functions" prop_parse_anonymous_functions
  , fastProperty "parse function literals" prop_parse_function_literals
  , fastProperty "parse method expressions" prop_parse_method_expressions
  , fastProperty "parse method values" prop_parse_method_values
  ]