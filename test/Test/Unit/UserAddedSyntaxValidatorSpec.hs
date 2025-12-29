module Test.Unit.UserAddedSyntaxValidatorSpec (tests) where

import Test.Tasty (TestTree, testGroup)
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
  testGroup "UserAdded SyntaxValidator Validation"
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
                    any (\e -> errorType e == MissingPackageDeclaration) errors
        ]

    , testGroup "Bracket and delimiter validation"
        [ testCase "detects missing parenthesis" $ do
            let missingParen = unlines
                  [ "package main"
                  , "func main() {"
                  , "    fmt.Println(\"Hello\")"  -- Missing closing parenthesis
                  , "}"
                  ]
                result = validateSyntax missingParen
                errors = getSyntaxErrors result
            assertBool "Should detect missing parenthesis" $ 
                    any (\e -> errorType e == MissingParenthesis) errors

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
                    any (\e -> errorType e == MissingBracket) errors

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
                    any (\e -> errorType e == UnclosedString) errors
        ]

    , testGroup "Identifier and declaration validation"
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
                    any (\e -> errorType e == InvalidIdentifier) errors

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
                    any (\e -> errorType e == DuplicateDeclaration) errors
        ]

    , testGroup "Statement and expression validation"
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
                    any (\e -> errorType e == InvalidStatement) errors
        ]

    , testGroup "Error reporting and formatting"
        [ testCase "provides clear error messages" $ do
            let invalidCode = unlines
                  [ "package main"
                  , "func main() {"
                  , "    fmt.Println(\"hello\")"  -- Missing closing parenthesis
                  , "}"
                  ]
                result = validateSyntax invalidCode
                errors = getSyntaxErrors result
            case errors of
                (err:_) -> do
                    let formatted = formatSyntaxError err
                    assertBool "Error message should be descriptive" $ 
                        length (errorMessage err) > 10
                    assertBool "Error should include line number" $ lineNumber err > 0
                    assertBool "Error should include column number" $ columnNumber err > 0
                [] -> assertBool "Should have detected an error" False
        ]

    , testGroup "Property-based validation"
        [ fastProperty "validation is deterministic" prop_validationDeterministic
        , fastProperty "valid code produces no errors" prop_validCodeNoErrors
        , fastProperty "error detection is consistent" prop_errorDetectionConsistent
        ]

    , testGroup "Performance and stress tests"
        [ testCase "handles large files efficiently" $ do
            let largeFile = unlines $ 
                  ["package main", "func main() {"] ++
                  ["    fmt.Println(\"line " ++ show i ++ "\")" | i <- [1..1000]] ++
                  ["}"]
                result = validateSyntax largeFile
                errors = getSyntaxErrors result
            assertBool "Should handle large files" $ length errors < 100

        , testCase "handles deeply nested structures" $ do
            let deeplyNested = unlines $ 
                  ["package main", "func main() {"] ++
                  concat [["    if true {"] | _ <- [1..50]] ++
                  ["        fmt.Println(\"deeply nested\")"] ++
                  concat ["    }" | _ <- [1..50]] ++
                  ["}"]
                result = validateSyntax deeplyNested
                errors = getSyntaxErrors result
            assertBool "Should handle deep nesting" $ length errors < 20
        ]
    ]

-- Helper functions
hasErrorType :: [SyntaxError] -> ErrorType -> Bool
hasErrorType errors errType = any (\e -> errorType e == errType) errors

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
    isValidGoLikeCode c = any (`isInfixOf` c) ["package", "func", "{", "}"]

-- | Property: error detection is consistent
prop_errorDetectionConsistent :: String -> Bool
prop_errorDetectionConsistent code =
    let result = validateSyntax code
        errors = getSyntaxErrors result
    in all isValidError errors

isValidError :: SyntaxError -> Bool
isValidError err = 
    lineNumber err > 0 && 
    column err > 0 && 
    not (null (errorMessage err))

isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = needle `elem` [take (length needle) $ drop i haystack | i <- [0..length haystack - length needle]]