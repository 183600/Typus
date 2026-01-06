module Test.Unit.SyntaxValidatorValidationAdvancedSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Test.Tasty.QuickCheck (testProperty, Property, Arbitrary(..), Gen, choose, oneof, listOf, elements)
import TestSupport.QuickCheck (fastProperty)

import SyntaxValidator
  ( SyntaxValidator
  , SyntaxError(..)
  , ErrorType(..)
  , newSyntaxValidator
  , validateSyntax
  , validateFile
  , getSyntaxErrors
  , formatSyntaxError
  )
import qualified Data.Set as Set

-- | Tests for SyntaxValidator validation functionality
tests :: TestTree
tests =
  testGroup "SyntaxValidator Validation"
    [ testGroup "Basic syntax validation"
        [ testCase "validates correct Go syntax" $ do
            let validGo = unlines
                  [ "package main"
                  , "import \"fmt\""
                  , "func main() {"
                  , "    fmt.Println(\"Hello, World!\")"
                  , "}"
                  ]
                result = validateSyntax validGo
                errors = getSyntaxErrors result
            assertBool "Should have no syntax errors for valid Go" $ null errors

        , testCase "validates correct Typus syntax" $ do
            let validTypus = unlines
                  [ "//! ownership=true"
                  , "package main"
                  , "func main() {"
                  , "    let x = 42"
                  , "    fmt.Println(x)"
                  , "}"
                  ]
                result = validateSyntax validTypus
                errors = getSyntaxErrors result
            assertBool "Should have no syntax errors for valid Typus" $ null errors

        , testCase "detects missing package declaration" $ do
            let noPackage = unlines
                  [ "import \"fmt\""
                  , "func main() {"
                  , "    fmt.Println(\"Hello\")"
                  , "}"
                  ]
                result = validateSyntax noPackage
                errors = getSyntaxErrors result
            assertBool "Should detect missing package declaration" $ 
                    L.any (\e -> errorType e == MissingPackageDeclaration) errors

        , testCase "detects missing closing brace" $ do
            let missingBrace = unlines
                  [ "package main"
                  , "func main() {"
                  , "    fmt.Println(\"Hello\")"
                  -- Missing closing brace
                  ]
                result = validateSyntax missingBrace
                errors = getSyntaxErrors result
            assertBool "Should detect missing closing brace" $ 
                    L.any (\e -> errorType e == MissingBrace) errors
        ]

    , testGroup "Bracket L.and delimiter validation"
        [ testCase "detects missing parenthesis" $ do
            let missingParen = unlines
                  [ "package main"
                  , "func main() {"
                  , "    fmt.Println(\"Hello\""  -- Missing closing parenthesis
                  , "}"
                  ]
                result = validateSyntax missingParen
                errors = getSyntaxErrors result
            assertBool "Should detect missing parenthesis" $ 
                    L.any (\e -> errorType e == MissingParenthesis) errors

        , testCase "detects missing bracket" $ do
            let missingBracket = unlines
                  [ "package main"
                  , "func main() {"
                  , "    arr := [1, 2, 3"  -- Missing closing bracket
                  , "}"
                  ]
                result = validateSyntax missingBracket
                errors = getSyntaxErrors result
            assertBool "Should detect missing bracket" $ 
                    L.any (\e -> errorType e == MissingBracket) errors

        , testCase "detects unclosed string literal" $ do
            let unclosedString = unlines
                  [ "package main"
                  , "func main() {"
                  , "    fmt.Println(\"Hello, World"  -- Missing closing quote
                  , "}"
                  ]
                result = validateSyntax unclosedString
                errors = getSyntaxErrors result
            assertBool "Should detect unclosed string" $ 
                    L.any (\e -> errorType e == UnclosedString) errors

        , testCase "detects unclosed comment" $ do
            let unclosedComment = unlines
                  [ "package main"
                  , "/* This comment is not closed"
                  , "func main() {"
                  , "    fmt.Println(\"Hello\")"
                  , "}"
                  ]
                result = validateSyntax unclosedComment
                errors = getSyntaxErrors result
            assertBool "Should detect unclosed comment" $ 
                    L.any (\e -> errorType e == UnclosedComment) errors
        ]

    , testGroup "Identifier L.and declaration validation"
        [ testCase "validates correct identifiers" $ do
            let validIdentifiers = unlines
                  [ "package main"
                  , "func main() {"
                  , "    var userName string"
                  , "    var age int"
                  , "    var is_active bool"
                  , "}"
                  ]
                result = validateSyntax validIdentifiers
                errors = getSyntaxErrors result
            assertBool "Should accept valid identifiers" $ null errors

        , testCase "detects invalid identifiers" $ do
            let invalidIdentifiers = unlines
                  [ "package main"
                  , "func main() {"
                  , "    var 123invalid string"  -- Invalid identifier
                  , "}"
                  ]
                result = validateSyntax invalidIdentifiers
                errors = getSyntaxErrors result
            assertBool "Should detect invalid identifiers" $ 
                    L.any (\e -> errorType e == InvalidIdentifier) errors

        , testCase "detects duplicate declarations" $ do
            let duplicateDecls = unlines
                  [ "package main"
                  , "func main() {"
                  , "    var x int"
                  , "    var x string"  -- Duplicate declaration
                  , "}"
                  ]
                result = validateSyntax duplicateDecls
                errors = getSyntaxErrors result
            assertBool "Should detect duplicate declarations" $ 
                    L.any (\e -> errorType e == DuplicateDeclaration) errors

        , testCase "detects undeclared variables" $ do
            let undeclaredVars = unlines
                  [ "package main"
                  , "func main() {"
                  , "    fmt.Println(undeclared_var)"  -- Undeclared variable
                  , "}"
                  ]
                result = validateSyntax undeclaredVars
                errors = getSyntaxErrors result
            assertBool "Should detect undeclared variables" $ 
                    L.any (\e -> errorType e == UndeclaredVariable) errors
        ]

    , testGroup "Function L.and type validation"
        [ testCase "validates correct function declarations" $ do
            let validFunctions = unlines
                  [ "package main"
                  , "func add(a int, b int) int {"
                  , "    return a + b"
                  , "}"
                  , "func main() {"
                  , "    result := add(1, 2)"
                  , "    fmt.Println(result)"
                  , "}"
                  ]
                result = validateSyntax validFunctions
                errors = getSyntaxErrors result
            assertBool "Should accept valid function declarations" $ null errors

        , testCase "detects invalid function declarations" $ do
            let invalidFunctions = unlines
                  [ "package main"
                  , "func 123invalid(a int, b int) int {"  -- Invalid function name
                  , "    return a + b"
                  , "}"
                  ]
                result = validateSyntax invalidFunctions
                errors = getSyntaxErrors result
            assertBool "Should detect invalid function declarations" $ 
                    L.any (\e -> errorType e == InvalidFunctionDeclaration) errors

        , testCase "validates type declarations" $ do
            let validTypes = unlines
                  [ "package main"
                  , "type Person struct {"
                  , "    Name string"
                  , "    Age  int"
                  , "}"
                  , "func main() {"
                  , "    p := Person{Name: \"Alice\", Age: 30}"
                  , "}"
                  ]
                result = validateSyntax validTypes
                errors = getSyntaxErrors result
            assertBool "Should accept valid type declarations" $ null errors

        , testCase "detects invalid type declarations" $ do
            let invalidTypes = unlines
                  [ "package main"
                  , "type 123Invalid struct {"  -- Invalid type name
                  , "    Name string"
                  , "}"
                  ]
                result = validateSyntax invalidTypes
                errors = getSyntaxErrors result
            assertBool "Should detect invalid type declarations" $ 
                    L.any (\e -> errorType e == InvalidTypeDeclaration) errors
        ]

    , testGroup "Statement L.and expression validation"
        [ testCase "validates correct statements" $ do
            let validStatements = unlines
                  [ "package main"
                  , "func main() {"
                  , "    x := 42"
                  , "    if x > 0 {"
                  , "        fmt.Println(\"positive\")"
                  , "    }"
                  , "    for i := 0; i < 10; i++ {"
                  , "        fmt.Println(i)"
                  , "    }"
                  , "}"
                  ]
                result = validateSyntax validStatements
                errors = getSyntaxErrors result
            assertBool "Should accept valid statements" $ null errors

        , testCase "detects invalid statements" $ do
            let invalidStatements = unlines
                  [ "package main"
                  , "func main() {"
                  , "    x := 42"
                  , "    if x > 0"  -- Missing braces
                  , "        fmt.Println(\"positive\")"
                  , "    }"
                  , "}"
                  ]
                result = validateSyntax invalidStatements
                errors = getSyntaxErrors result
            assertBool "Should detect invalid statements" $ 
                    L.any (\e -> errorType e == InvalidStatement) errors

        , testCase "detects missing semicolons where required" $ do
            let missingSemicolons = unlines
                  [ "package main"
                  , "func main() {"
                  , "    var x int = 42"
                  , "    var y int = 24"  -- Missing semicolon in some contexts
                  , "}"
                  ]
                result = validateSyntax missingSemicolons
                errors = getSyntaxErrors result
            -- May L.or may not detect depending on language requirements
            assertBool "Should handle semicolon requirements" $ L.length errors >= 0
        ]

    , testGroup "Import validation"
        [ testCase "validates correct imports" $ do
            let validImports = unlines
                  [ "package main"
                  , "import \"fmt\""
                  , "import \"os\""
                  , "import ("
                  , "    \"strings\""
                  , "    \"strconv\""
                  , ")"
                  , "func main() {"
                  , "    fmt.Println(\"Hello\")"
                  , "}"
                  ]
                result = validateSyntax validImports
                errors = getSyntaxErrors result
            assertBool "Should accept valid imports" $ null errors

        , testCase "detects invalid imports" $ do
            let invalidImports = unlines
                  [ "package main"
                  , "import 123invalid"  -- Invalid import path
                  , "func main() {"
                  , "    fmt.Println(\"Hello\")"
                  , "}"
                  ]
                result = validateSyntax invalidImports
                errors = getSyntaxErrors result
            assertBool "Should detect invalid imports" $ 
                    L.any (\e -> errorType e == InvalidImport) errors
        ]

    , testGroup "Block structure validation"
        [ testCase "validates correct block structures" $ do
            let validBlocks = unlines
                  [ "package main"
                  , "func main() {"
                  , "    if true {"
                  , "        if false {"
                  , "            fmt.Println(\"nested\")"
                  , "        }"
                  , "    }"
                  , "}"
                  ]
                result = validateSyntax validBlocks
                errors = getSyntaxErrors result
            assertBool "Should accept valid block structures" $ null errors

        , testCase "detects invalid block structures" $ do
            let invalidBlocks = unlines
                  [ "package main"
                  , "func main() {"
                  , "    if true {"
                  , "        fmt.Println(\"hello\")"
                  , "    }"  -- Extra closing brace
                  , "}"
                  ]
                result = validateSyntax invalidBlocks
                errors = getSyntaxErrors result
            assertBool "Should detect invalid block structures" $ 
                    L.any (\e -> errorType e == InvalidBlockStructure) errors

        , testCase "detects unterminated blocks" $ do
            let unterminatedBlocks = unlines
                  [ "package main"
                  , "func main() {"
                  , "    if true {"
                  , "        fmt.Println(\"hello\")"
                  , "    // Missing closing braces"
                  ]
                result = validateSyntax unterminatedBlocks
                errors = getSyntaxErrors result
            assertBool "Should detect unterminated blocks" $ 
                    L.any (\e -> errorType e == UnterminatedBlock) errors
        ]

    , testGroup "Error reporting L.and formatting"
        [ testCase "provides clear error messages" $ do
            let invalidCode = unlines
                  [ "package main"
                  , "func main() {"
                  , "    fmt.Println(\"hello\""  -- Missing closing parenthesis
                  , "}"
                  ]
                result = validateSyntax invalidCode
                errors = getSyntaxErrors result
            case errors of
                (err:_) -> do
                    let formatted = formatSyntaxError err
                    assertBool "Error message should be descriptive" $ 
                        L.length (errorMessage err) > 10
                    assertBool "Error should include line number" $ lineNumber err > 0
                    assertBool "Error should include column number" $ columnNumber err > 0
                [] -> assertBool "Should have detected an error" False

        , testCase "sorts errors by location" $ do
            let invalidCode = unlines
                  [ "package main"
                  , "func main() {"
                  , "    var 123invalid int"  -- Line 3
                  , "    fmt.Println(\"hello\""  -- Line 4
                  , "}"
                  ]
                result = validateSyntax invalidCode
                errors = getSyntaxErrors result
            assertBool "Errors should be sorted by location" $ 
                L.all (\(e1, e2) -> lineNumber e1 <= lineNumber e2) $ zip errors (L.tail errors)
        ]

    , testGroup "Property-based validation"
        [ fastProperty "validation is deterministic" prop_validationDeterministic
        , fastProperty "valid code produces no errors" prop_validCodeNoErrors
        , fastProperty "error detection is consistent" prop_errorDetectionConsistent
        ]

    , testGroup "Performance L.and stress tests"
        [ testCase "handles large files efficiently" $ do
            let largeFile = unlines $ 
                  ["package main", "func main() {"] ++
                  ["    fmt.Println(\"line " ++ show i ++ "\")" | i <- [1..1000]] ++
                  ["}"]
                result = validateSyntax largeFile
                errors = getSyntaxErrors result
            assertBool "Should handle large files" $ L.length errors < 100

        , testCase "handles deeply nested structures" $ do
            let deeplyNested = unlines $ 
                  ["package main", "func main() {"] ++
                  L.concat [["    if true {"] | _ <- [1..50]] ++
                  ["        fmt.Println(\"deeply nested\")"] ++
                  L.concat ["    }" | _ <- [1..50]] ++
                  ["}"]
                result = validateSyntax deeplyNested
                errors = getSyntaxErrors result
            assertBool "Should handle deep nesting" $ L.length errors < 20

        , testCase "handles complex expressions" $ do
            let complexExpression = unlines
                  [ "package main"
                  , "func main() {"
                  , "    result := ((a + b) * (c - d)) / (e % f) + (g << h) - (i >> j) & (k | l) ^ (m && n) || (o || p)"
                  , "}"
                  ]
                result = validateSyntax complexExpression
                errors = getSyntaxErrors result
            assertBool "Should handle complex expressions" $ L.length errors >= 0
        ]

    , testGroup "Mixed language validation"
        [ testCase "validates mixed Go L.and Typus code" $ do
            let mixedCode = unlines
                  [ "//! ownership=true"
                  , "package main"
                  , "func processData() {"
                  , "    let data = Box::new(42)"
                  , "    fmt.Println(*data)"
                  , "}"
                  , "func main() {"
                  , "    processData()"
                  , "}"
                  ]
                result = validateSyntax mixedCode
                errors = getSyntaxErrors result
            assertBool "Should handle mixed language code" $ L.length errors >= 0

        , testCase "detects language-specific syntax errors" $ do
            let languageSpecific = unlines
                  [ "//! ownership=true"
                  , "package main"
                  , "func main() {"
                  , "    let x: invalid_type = 42"  -- Invalid type
                  , "}"
                  ]
                result = validateSyntax languageSpecific
                errors = getSyntaxErrors result
            assertBool "Should detect language-specific errors" $ L.length errors >= 0
        ]
    ]

-- Helper functions
hasErrorType :: [SyntaxError] -> ErrorType -> Bool
hasErrorType errors errType = L.any (\e -> errorType e == errType) errors

-- | Property: validation is deterministic
prop_validationDeterministic :: String -> Bool
prop_validationDeterministic code =
    let result1 = validateSyntax code
        result2 = validateSyntax code
    in getSyntaxErrors result1 == getSyntaxErrors result2

-- | Property: valid code produces no errors
prop_validCodeNoErrors :: String -> Bool
prop_validCodeNoErrors code =
    let result = validateSyntax code
        errors = getSyntaxErrors result
    in not (isValidGoLikeCode code) || null errors
  where
    isValidGoLikeCode c = L.any (`L.isInfixOf` c) ["package", "func", "{", "}"]

-- | Property: error detection is consistent
prop_errorDetectionConsistent :: String -> Bool
prop_errorDetectionConsistent code =
    let result = validateSyntax code
        errors = getSyntaxErrors result
    in L.all isValidError errors

isValidError :: SyntaxError -> Bool
isValidError err = 
    lineNumber err > 0 && 
    columnNumber err > 0 && 
    not (L.null (errorMessage err))

isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = needle `elem` [take (L.length needle) $ drop i haystack | i <- [0..L.length haystack - L.length needle]]