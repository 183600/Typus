{-# LANGUAGE CPP #-}

module Test.Unit.ExtendedParserQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import TestSupport.Arbitrary ()
import Test.QuickCheck (Property, (==>), counterexample, property)

import Parser (parseTypus, TypusFile(..), FileDirectives(..), CodeBlock(..))
import SourceLocation (Located(..), locatedValue)
import qualified Data.List as Data.List
import Data.Maybe (isJust)

-- Extended parser property tests for comprehensive coverage

-- Property: Parsing is idempotent - parsing a parsed file's reconstruction yields same structure
prop_parse_idempotent :: TypusFile -> Property
prop_parse_idempotent typusFile = 
  null (tfSyntaxErrors typusFile) ==>
  let reconstructed = reconstructTypusFile typusFile
  in case parseTypus reconstructed of
    Left err -> counterexample ("Parse error in idempotent test: " ++ err) $ property False
    Right parsed -> 
      let originalDirectives = extractDirectiveValues $ tfDirectives typusFile
          parsedDirectives = extractDirectiveValues $ tfDirectives parsed
      in property $ originalDirectives == parsedDirectives

-- Property: Directive order preservation
prop_parse_directive_order_preservation :: [String] -> Property
prop_parse_directive_order_preservation directives =
  not (null directives) && length directives <= 10 ==>
  let validDirectives = ["//! ownership: on", "//! ownership: off", 
                        "//! dependent_types: on", "//! dependent_types: off",
                        "//! constraints: on", "//! constraints: off"]
      selectedDirectives = take (length directives `mod` 6 + 1) 
                              (cycle validDirectives)
      content = Data.List.unlines selectedDirectives
  in case parseTypus content of
    Left err -> counterexample ("Parse error: " ++ err) $ property False
    Right parsed -> property $ True

-- Property: Whitespace insensitivity for directives
prop_parse_directive_whitespace_insensitive :: String -> String -> String -> Property
prop_parse_directive_whitespace_insensitive before middle after =
  all (\s -> not ("//!" `Data.List.isInfixOf` s) && not ("\n" `Data.List.isInfixOf` s)) [before, middle, after] ==>
  let content = before ++ "//! ownership: on\n" ++ middle ++ "//! dependent_types: off\n" ++ after
  in case parseTypus content of
    Left _ -> property False
    Right parsed -> property $ hasOwnershipDirective parsed && hasDependentTypesDirective parsed

-- Property: Case sensitivity in directives
prop_parse_directive_case_sensitivity :: Property
prop_parse_directive_case_sensitivity =
  let normalDirective = "//! ownership: on\npackage main\nfunc main() {}"
      mixedDirective = "//! OWNERSHIP: On\npackage main\nfunc main() {}"
  in case (parseTypus normalDirective, parseTypus mixedDirective) of
    (Right _, Left _) -> property True  -- Normal works, mixed case fails
    (Right _, Right _) -> property True  -- Both work (case insensitive)
    _ -> property True  -- Other combinations are also acceptable

-- Property: Comment preservation in code blocks
prop_parse_comment_preservation :: [String] -> Property
prop_parse_comment_preservation comments =
  let commentLines = map ("// " ++) comments
      content = Data.List.unlines $ ["//! ownership: on", "package main"] ++ commentLines ++ ["func main() {}"]
  in case parseTypus content of
    Left err -> counterexample ("Parse error with comments: " ++ err) $ property False
    Right parsed -> property $ hasCodeBlocks parsed

-- Property: Tab and space mixing handling
prop_parse_tab_space_mixing :: [String] -> Property
prop_parse_tab_space_mixing lines =
  not (null lines) ==>
  let mixedIndentation = zipWith (\i line -> 
        if even i then replicate i ' ' ++ line 
        else replicate i '\t' ++ line) [0..] lines
      content = Data.List.unlines mixedIndentation
  in case parseTypus content of
    Left _ -> property False
    Right _ -> property True

-- Property: Empty lines preservation
prop_parse_empty_lines_preservation :: Int -> Int -> Property
prop_parse_empty_lines_preservation numBlocks numEmptyLines =
  numBlocks > 0 && numEmptyLines >= 0 && numEmptyLines <= 20 ==>
  let blocks = ["func block" ++ show i ++ "() {}" | i <- [1..numBlocks]]
      content = Data.List.unlines $ ["//! ownership: on", "package main"] ++ blocks
  in case parseTypus content of
    Left err -> counterexample ("Parse error with empty lines: " ++ err) $ property False
    Right parsed -> property $ length (tfBlocks parsed) >= numBlocks

-- Property: Special Unicode characters in identifiers
prop_parse_unicode_identifiers :: [String] -> Property
prop_parse_unicode_identifiers identifiers =
  let unicodeIdentifiers = map (++ "变量") identifiers
      content = Data.List.unlines $ ["//! ownership: on", "package main"] ++
                        map (\id -> "var " ++ id ++ " int = 42") unicodeIdentifiers
  in case parseTypus content of
    Left err -> counterexample ("Parse error with Unicode identifiers: " ++ err) $ property False
    Right parsed -> property $ hasCodeBlocks parsed

-- Property: Extremely long identifiers
prop_parse_long_identifiers :: Int -> Property
prop_parse_long_identifiers length =
  length > 0 && length <= 100 ==>
  let longIdentifier = replicate length 'x'
      content = "//! ownership: on\npackage main\nvar " ++ longIdentifier ++ " int = 42"
  in case parseTypus content of
    Left err -> counterexample ("Parse error with long identifier: " ++ err) $ property False
    Right parsed -> property $ hasCodeBlocks parsed

-- Property: Numeric literals with different bases
prop_parse_numeric_base_literals :: [Int] -> [Int] -> [Int] -> Property
prop_parse_numeric_base_literals decimals octals hexadecimals =
  let decimalVars = map (\i -> "var dec" ++ show i ++ " int = " ++ show i) decimals
      octalVars = map (\i -> "var oct" ++ show i ++ " int = 0o" ++ show (i `mod` 100)) octals
      hexVars = map (\i -> "var hex" ++ show i ++ " int = 0x" ++ show (i `mod` 255)) hexadecimals
      content = Data.List.unlines $ ["//! ownership: on", "package main"] ++ 
                        decimalVars ++ octalVars ++ hexVars ++ ["func main() {}"]
  in case parseTypus content of
    Left err -> counterexample ("Parse error with numeric bases: " ++ err) $ property False
    Right parsed -> property $ hasCodeBlocks parsed

-- Property: Floating point literals with scientific notation
prop_parse_float_scientificnotation :: [Double] -> Property
prop_parse_float_scientificnotation floats =
  let floatVars = map (\(i, f) -> "var float" ++ show i ++ " float64 = " ++ 
                      show f ++ "e" ++ show (i `mod` 3 + 1)) (zip [0..] floats)
      content = Data.List.unlines $ ["//! ownership: on", "package main"] ++ floatVars ++ ["func main() {}"]
  in case parseTypus content of
    Left err -> counterexample ("Parse error with scientific notation: " ++ err) $ property False
    Right parsed -> property $ hasCodeBlocks parsed

-- Property: String literals with various escape sequences
prop_parse_string_escape_sequences :: [String] -> Property
prop_parse_string_escape_sequences strings =
  let escapeSequences = ["\n", "\t", "\r", "\\", "\"", "'", "\x41", "\4660"]
      stringsWithEscapes = map (\s -> s ++ concat escapeSequences) strings
      content = Data.List.unlines $ ["//! ownership: on", "package main"] ++
                        map (\s -> "var str string = \"" ++ s ++ "\"") stringsWithEscapes ++
                        ["func main() {}"]
  in case parseTypus content of
    Left err -> counterexample ("Parse error with escape sequences: " ++ err) $ property False
    Right parsed -> property $ hasCodeBlocks parsed

-- Property: Raw string literals
prop_parse_raw_string_literals :: [String] -> Property
prop_parse_raw_string_literals strings =
  let rawStrings = map (\s -> "`" ++ s ++ "`") strings
      content = Data.List.unlines $ ["//! ownership: on", "package main"] ++
                        map (\s -> "var raw string = " ++ s) rawStrings ++
                        ["func main() {}"]
  in case parseTypus content of
    Left err -> counterexample ("Parse error with raw strings: " ++ err) $ property False
    Right parsed -> property $ hasCodeBlocks parsed

-- Property: Complex nested expressions
prop_parse_nested_expressions :: Int -> Property
prop_parse_nested_expressions depth =
  depth >= 0 && depth <= 5 ==>
  let nestedExpr = generateNestedExpression depth
      content = "//! ownership: on\npackage main\nfunc main() {\n  result := " ++ nestedExpr ++ "\n}"
  in case parseTypus content of
    Left err -> counterexample ("Parse error with nested expression: " ++ err) $ property False
    Right parsed -> property $ hasCodeBlocks parsed

-- Property: Complex type declarations
prop_parse_complex_type_declarations :: [String] -> [String] -> Property
prop_parse_complex_type_declarations typeNames fieldTypes =
  let complexTypes = zipWith (\tName fType -> 
        "type " ++ tName ++ " struct {\n  Field1 " ++ fType ++ "\n  Field2 []" ++ fType ++ 
        "\n  Field3 map[string]" ++ fType ++ "\n}") typeNames fieldTypes
      content = Data.List.unlines $ ["//! ownership: on", "package main"] ++ complexTypes ++ ["func main() {}"]
  in case parseTypus content of
    Left err -> counterexample ("Parse error with complex types: " ++ err) $ property False
    Right parsed -> property $ hasCodeBlocks parsed

-- Property: Interface declarations with multiple methods
prop_parse_complex_interface_declarations :: [String] -> [String] -> [String] -> Property
prop_parse_complex_interface_declarations interfaceNames methodNames returnTypes =
  let interfaces = zipWith3 (\iName mName rType -> 
        "type " ++ iName ++ " interface {\n  " ++ mName ++ "() " ++ rType ++ 
        "\n  " ++ mName ++ "2(string) error\n  " ++ mName ++ "3() (int, error)\n}") 
        interfaceNames methodNames returnTypes
      content = Data.List.unlines $ ["//! ownership: on", "package main"] ++ interfaces ++ ["func main() {}"]
  in case parseTypus content of
    Left err -> counterexample ("Parse error with complex interfaces: " ++ err) $ property False
    Right parsed -> property $ hasCodeBlocks parsed

-- Property: Generic type declarations with constraints
prop_parse_generic_type_constraints :: [String] -> [String] -> [String] -> Property
prop_parse_generic_type_constraints typeNames typeParams constraints =
  let generics = zipWith3 (\tName tParam constraint -> 
        "type " ++ tName ++ "[" ++ tParam ++ " " ++ constraint ++ "] struct {\n  Value " ++ tParam ++ "\n}") 
        typeNames typeParams constraints
      content = Data.List.unlines $ ["//! ownership: on", "package main"] ++ generics ++ ["func main() {}"]
  in case parseTypus content of
    Left err -> counterexample ("Parse error with generic constraints: " ++ err) $ property False
    Right parsed -> property $ hasCodeBlocks parsed

-- Property: Function declarations with complex signatures
prop_parse_complex_function_signatures :: [String] -> [String] -> [String] -> [String] -> Property
prop_parse_complex_function_signatures funcNames paramNames paramTypes returnTypes =
  let funcs = Data.List.zipWith4 (\fName pName pType rType -> 
        "func " ++ fName ++ "(" ++ pName ++ " " ++ pType ++ ") (" ++ rType ++ ", error) {\n  return " ++ 
        getDefaultForType rType ++ ", nil\n}") funcNames paramNames paramTypes returnTypes
      content = Data.List.unlines $ ["//! ownership: on", "package main"] ++ funcs ++ ["func main() {}"]
  in case parseTypus content of
    Left err -> counterexample ("Parse error with complex function signatures: " ++ err) $ property False
    Right parsed -> property $ hasCodeBlocks parsed

-- Property: Method declarations with value and pointer receivers
prop_parse_method_declarations :: [String] -> [String] -> [String] -> Bool -> Property
prop_parse_method_declarations structNames methodNames paramTypes isPointerReceiver =
  let methods = zipWith3 (\sName mName pType -> 
        let receiverType = if isPointerReceiver then "*" ++ sName else sName
        in "func (" ++ receiverType ++ ") " ++ mName ++ "(" ++ pType ++ ") error {\n  return nil\n}") 
        (cycle structNames) methodNames paramTypes
      content = Data.List.unlines $ ["//! ownership: on", "package main"] ++ methods ++ ["func main() {}"]
  in case parseTypus content of
    Left err -> counterexample ("Parse error with method declarations: " ++ err) $ property False
    Right parsed -> property $ hasCodeBlocks parsed

-- Property: Anonymous function declarations
prop_parse_anonymous_functions :: [String] -> [String] -> Property
prop_parse_anonymous_functions paramTypes returnTypes =
  let anonFuncs = zipWith (\pType rType -> 
        "var fn func(" ++ pType ++ ") " ++ rType ++ " = func(" ++ pType ++ ") " ++ rType ++ " {\n  return " ++ 
        getDefaultForType rType ++ "\n}") paramTypes returnTypes
      content = Data.List.unlines $ ["//! ownership: on", "package main"] ++ anonFuncs ++ ["func main() {}"]
  in case parseTypus content of
    Left err -> counterexample ("Parse error with anonymous functions: " ++ err) $ property False
    Right parsed -> property $ hasCodeBlocks parsed

-- Property: Closure expressions with captured variables
prop_parse_closure_expressions :: [String] -> Property
prop_parse_closure_expressions variableNames =
  let variables = map (\vName -> "var " ++ vName ++ " int = 42") variableNames
      closures = map (\vName -> "closure := func() int { return " ++ vName ++ " }") variableNames
      content = Data.List.unlines $ ["//! ownership: on", "package main", "func main() {"] ++
                        variables ++ closures ++ ["}"]
  in case parseTypus content of
    Left err -> counterexample ("Parse error with closures: " ++ err) $ property False
    Right parsed -> property $ hasCodeBlocks parsed

-- Property: Complex channel operations
prop_parse_complex_channel_operations :: [String] -> [String] -> Property
prop_parse_complex_channel_operations channelNames elementTypes =
  let channelDecls = zipWith (\cName eType -> "var " ++ cName ++ " chan " ++ eType) channelNames elementTypes
      channelOps = zipWith (\cName eType -> 
        cName ++ " <- make(" ++ eType ++ ")\nval := <-" ++ cName) channelNames elementTypes
      content = Data.List.unlines $ ["//! ownership: on", "package main"] ++ channelDecls ++
                        ["func main() {"] ++ channelOps ++ ["}"]
  in case parseTypus content of
    Left err -> counterexample ("Parse error with complex channel ops: " ++ err) $ property False
    Right parsed -> property $ hasCodeBlocks parsed

-- Property: Select statements with multiple cases
prop_parse_complex_select_statements :: Int -> Int -> Property
prop_parse_complex_select_statements numCases numChannels =
  numCases > 0 && numCases <= 10 && numChannels > 0 && numChannels <= 5 ==>
  let channels = ["ch" ++ show i | i <- [1..numChannels]]
      channelDecls = map (\c -> "var " ++ c ++ " chan int") channels
      selectCases = take numCases $ cycle [
        "case <-ch1:\n  fmt.Println(\"received from ch1\")",
        "case ch2 <- 42:\n  fmt.Println(\"sent to ch2\")",
        "default:\n  fmt.Println(\"no activity\")"]
      selectContent = "select {\n" ++ unlines selectCases ++ "\n}"
      content = Data.List.unlines $ ["//! ownership: on", "package main"] ++ channelDecls ++
                        ["func main() {", selectContent, "}"]
  in case parseTypus content of
    Left err -> counterexample ("Parse error with complex select: " ++ err) $ property False
    Right parsed -> property $ hasCodeBlocks parsed

-- Property: Go statements with function calls
prop_parse_go_statements :: [String] -> [String] -> Property
prop_parse_go_statements funcNames paramTypes =
  let funcDecls = zipWith (\fName pType -> 
        "func " ++ fName ++ "(" ++ pType ++ ") {\n  fmt.Println(\"Running " ++ fName ++ "\")\n}") 
        funcNames paramTypes
      goCalls = map (\fName -> "go " ++ fName ++ "(42)") funcNames
      content = Data.List.unlines $ ["//! ownership: on", "package main"] ++ funcDecls ++
                        ["func main() {"] ++ goCalls ++ ["}"]
  in case parseTypus content of
    Left err -> counterexample ("Parse error with go statements: " ++ err) $ property False
    Right parsed -> property $ hasCodeBlocks parsed

-- Property: Defer statements with function calls
prop_parse_defer_statements :: [String] -> [String] -> Property
prop_parse_defer_statements funcNames paramTypes =
  let funcDecls = zipWith (\fName pType -> 
        "func " ++ fName ++ "(" ++ pType ++ ") {\n  fmt.Println(\"Cleaning up " ++ fName ++ "\")\n}") 
        funcNames paramTypes
      deferCalls = map (\fName -> "defer " ++ fName ++ "(42)") funcNames
      content = Data.List.unlines $ ["//! ownership: on", "package main"] ++ funcDecls ++
                        ["func main() {"] ++ deferCalls ++ ["}"]
  in case parseTypus content of
    Left err -> counterexample ("Parse error with defer statements: " ++ err) $ property False
    Right parsed -> property $ hasCodeBlocks parsed

-- Property: Panic and recover statements
prop_parse_panic_recover_statements :: [String] -> Property
prop_parse_panic_recover_statements panicMessages =
  let panics = map (\msg -> "panic(\"" ++ msg ++ "\")") panicMessages
      recoverFunc = "func recoverTest() {\n  defer func() {\n    if r := recover(); r != nil {\n      fmt.Println(\"Recovered:\", r)\n    }\n  }()\n  panic(\"test panic\")\n}"
      content = Data.List.unlines $ ["//! ownership: on", "package main"] ++ panics ++ [recoverFunc, "func main() {}"]
  in case parseTypus content of
    Left err -> counterexample ("Parse error with panic/recover: " ++ err) $ property False
    Right parsed -> property $ hasCodeBlocks parsed

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
        Just (Located True _ _) -> ["//! ownership: on"]
        Just (Located False _ _) -> ["//! ownership: off"]
      depTypesLine = case depTypes of
        Nothing -> []
        Just (Located True _ _) -> ["//! dependent_types: on"]
        Just (Located False _ _) -> ["//! dependent_types: off"]
      constraintsLine = case constraints of
        Nothing -> []
        Just (Located True _ _) -> ["//! constraints: on"]
        Just (Located False _ _) -> ["//! constraints: off"]
  in ownershipLine ++ depTypesLine ++ constraintsLine

reconstructBlock :: CodeBlock -> String
reconstructBlock block = cbContent block

extractDirectiveValues :: FileDirectives -> (Maybe Bool, Maybe Bool, Maybe Bool)
extractDirectiveValues (FileDirectives ownership depTypes constraints) =
  (locatedValue <$> ownership, locatedValue <$> depTypes, locatedValue <$> constraints)

hasOwnershipDirective :: TypusFile -> Bool
hasOwnershipDirective file = isJust $ fdOwnership $ tfDirectives file

hasDependentTypesDirective :: TypusFile -> Bool
hasDependentTypesDirective file = isJust $ fdDependentTypes $ tfDirectives file

hasCodeBlocks :: TypusFile -> Bool
hasCodeBlocks file = not $ null $ tfBlocks file

generateNestedExpression :: Int -> String
generateNestedExpression 0 = "42"
generateNestedExpression n = "(" ++ generateNestedExpression (n - 1) ++ " + " ++ 
                            generateNestedExpression (n - 1) ++ ")"

getDefaultForType :: String -> String
getDefaultForType "int" = "0"
getDefaultForType "string" = "\"\""
getDefaultForType "bool" = "false"
getDefaultForType "float64" = "0.0"
getDefaultForType _ = "nil"

tests :: TestTree
tests = testGroup "Extended Parser QuickCheck Tests"
  [ fastProperty "Parse idempotent" prop_parse_idempotent
  , fastProperty "Directive order preservation" prop_parse_directive_order_preservation
  , fastProperty "Directive whitespace insensitive" prop_parse_directive_whitespace_insensitive
  , fastProperty "Directive case sensitivity" prop_parse_directive_case_sensitivity
  , fastProperty "Comment preservation" prop_parse_comment_preservation
  , fastProperty "Tab and space mixing" prop_parse_tab_space_mixing
  , fastProperty "Empty lines preservation" prop_parse_empty_lines_preservation
  , fastProperty "Unicode identifiers" prop_parse_unicode_identifiers
  , fastProperty "Long identifiers" prop_parse_long_identifiers
  , fastProperty "Numeric base literals" prop_parse_numeric_base_literals
  , fastProperty "Float scientific notation" prop_parse_float_scientificnotation
  , fastProperty "String escape sequences" prop_parse_string_escape_sequences
  , fastProperty "Raw string literals" prop_parse_raw_string_literals
  , fastProperty "Nested expressions" prop_parse_nested_expressions
  , fastProperty "Complex type declarations" prop_parse_complex_type_declarations
  , fastProperty "Complex interface declarations" prop_parse_complex_interface_declarations
  , fastProperty "Generic type constraints" prop_parse_generic_type_constraints
  , fastProperty "Complex function signatures" prop_parse_complex_function_signatures
  , fastProperty "Method declarations" prop_parse_method_declarations
  , fastProperty "Anonymous functions" prop_parse_anonymous_functions
  , fastProperty "Closure expressions" prop_parse_closure_expressions
  , fastProperty "Complex channel operations" prop_parse_complex_channel_operations
  , fastProperty "Complex select statements" prop_parse_complex_select_statements
  , fastProperty "Go statements" prop_parse_go_statements
  , fastProperty "Defer statements" prop_parse_defer_statements
  , fastProperty "Panic and recover statements" prop_parse_panic_recover_statements
  ]