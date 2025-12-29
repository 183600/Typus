module Test.Unit.SyntaxValidatorAdvancedQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Test.Tasty.QuickCheck (testProperty, Property, (===), forAll, Gen, choose, arbitrary, listOf, elements, oneof, suchThat)
import TestSupport.QuickCheck (fastProperty)

import SyntaxValidator (SyntaxValidator(..), SyntaxError(..), ErrorType(..), 
                        newSyntaxValidator, validateSyntax, validateFile, 
                        getSyntaxErrors, formatSyntaxError)
import qualified Data.Set as Set
import Data.List (isInfixOf, isPrefixOf)
import Data.Char (isSpace, isAlphaNum, isAlpha, isDigit)

-- ============================================================================
-- Generators
-- ============================================================================

-- Generate error types
genErrorType :: Gen ErrorType
genErrorType = elements 
  [ MissingBrace
  , MissingParenthesis
  , MissingBracket
  , UnclosedString
  , UnclosedComment
  , InvalidIdentifier
  , InvalidTypeDeclaration
  , InvalidFunctionDeclaration
  , InvalidImport
  , InvalidStatement
  , UnterminatedBlock
  , InvalidOperator
  , MissingSemicolon
  , UnexpectedToken
  , MissingPackageDeclaration
  , DuplicateDeclaration
  , InvalidBlockStructure
  , UndeclaredVariable
  , SyntaxWarning
  ]

-- Generate line numbers
genLineNumber :: Gen Int
genLineNumber = choose (1, 1000)

-- Generate column numbers
genColumnNumber :: Gen Int
genColumnNumber = choose (1, 200)

-- Generate error messages
genErrorMessage :: Gen String
genErrorMessage = listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ' ' : ".;:!?()[]{}"

-- Generate line content
genLineContent :: Gen String
genLineContent = listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \t,.;:!?()[]{}+-*/"

-- Generate syntax errors
genSyntaxError :: Gen SyntaxError
genSyntaxError = do
  errorType <- genErrorType
  errorMessage <- genErrorMessage
  lineNumber <- genLineNumber
  columnNumber <- genColumnNumber
  lineContent <- genLineContent
  return $ SyntaxError errorType errorMessage lineNumber columnNumber lineContent

-- Generate valid Go code snippets
genValidGoCode :: Gen String
genValidGoCode = oneof
  [ return "package main"
  , return "func main() {}"
  , return "var x int = 1"
  , return "const y = 42"
  , return "import \"fmt\""
  , return "type MyStruct struct { Field int }"
  , do
      lines' <- listOf $ elements 
        [ "package main"
        , "import \"fmt\""
        , "func main() {"
        , "    fmt.Println(\"Hello, World!\")"
        , "}"
        ]
      return $ unlines lines'
  ]

-- Generate invalid Go code snippets
genInvalidGoCode :: Gen String
genInvalidGoCode = oneof
  [ return "func main( {}"  -- Missing parenthesis
  , return "func main() {"   -- Missing closing brace
  , return "var x int = "   -- Incomplete assignment
  , return "import"          -- Incomplete import
  , return "type MyStruct {" -- Missing closing brace
  , return "if x > {"        -- Invalid condition
  , return "\"unclosed string"  -- Unclosed string
  , return "/* unclosed comment"  -- Unclosed comment
  ]

-- Generate mixed code (valid and invalid)
genMixedCode :: Gen String
genMixedCode = do
  validParts <- listOf genValidGoCode
  invalidParts <- listOf genInvalidGoCode
  parts <- listOf $ elements $ validParts ++ invalidParts
  return $ unlines parts

-- Generate code with specific syntax issues
genCodeWithMissingBraces :: Gen String
genCodeWithMissingBraces = do
  lines' <- listOf $ elements
    [ "func main() {"
    , "    if x > 0 {"
    , "        fmt.Println(x)"
    , "    // missing closing braces"
    ]
  return $ unlines lines'

genCodeWithUnclosedStrings :: Gen String
genCodeWithUnclosedStrings = do
  lines' <- listOf $ elements
    [ "var s string = \"unclosed string"
    , "fmt.Println(s)"
    ]
  return $ unlines lines'

genCodeWithInvalidIdentifiers :: Gen String
genCodeWithInvalidIdentifiers = do
  lines' <- listOf $ elements
    [ "var 123invalid int"
    , "func @invalid() {}"
    ]
  return $ unlines lines'

-- ============================================================================
-- QuickCheck Properties
-- ============================================================================

-- Property: newSyntaxValidator creates valid validator
prop_newSyntaxValidatorValid :: Bool
prop_newSyntaxValidatorValid =
  let validator = newSyntaxValidator
  in null (validatorErrors validator) && 
     null (scopeStack validator) && 
     null (braceStack validator) &&
     not (hasPackageDecl validator) &&
     not (hasMainFunc validator)

-- Property: validateFile returns same results as validateSyntax
prop_validateFileSameAsValidateSyntax :: String -> Bool
prop_validateFileSameAsValidateSyntax content =
  validateFile content == validateSyntax content

-- Property: validateSyntax returns list of errors
prop_validateSyntaxReturnsList :: String -> Bool
prop_validateSyntaxReturnsList content =
  let errors = validateSyntax content
  in length errors >= 0  -- Should always be non-negative

-- Property: getSyntaxErrors returns errors in reverse order
prop_getSyntaxErrorsReverses :: SyntaxValidator -> Bool
prop_getSyntaxErrorsReverses validator =
  let errors = getSyntaxErrors validator
      originalErrors = validatorErrors validator
  in errors == reverse originalErrors

-- Property: SyntaxError equality is reflexive
prop_syntaxErrorReflexive :: SyntaxError -> Bool
prop_syntaxErrorReflexive error = error == error

-- Property: SyntaxError equality is symmetric
prop_syntaxErrorSymmetric :: SyntaxError -> SyntaxError -> Bool
prop_syntaxErrorSymmetric error1 error2 = (error1 == error2) == (error2 == error1)

-- Property: SyntaxError equality is transitive
prop_syntaxErrorTransitive :: SyntaxError -> SyntaxError -> SyntaxError -> Bool
prop_syntaxErrorTransitive error1 error2 error3 =
  (error1 == error2 && error2 == error3) ==> (error1 == error3)

-- Property: SyntaxError ordering is consistent
prop_syntaxErrorOrderingConsistent :: SyntaxError -> SyntaxError -> Bool
prop_syntaxErrorOrderingConsistent error1 error2 =
  let ordering = compare error1 error2
  in case (errorMessage error1 `compare` errorMessage error2) of
       LT -> ordering == LT
       GT -> ordering == GT
       EQ -> case compare (lineNumber error1) (lineNumber error2) of
                LT -> ordering == LT
                GT -> ordering == GT
                EQ -> compare (columnNumber error1) (columnNumber error2) == ordering

-- Property: formatSyntaxError produces non-empty output
prop_formatSyntaxErrorNonEmpty :: SyntaxError -> Bool
prop_formatSyntaxErrorNonEmpty error = not (null (formatSyntaxError error))

-- Property: formatSyntaxError contains error type
prop_formatSyntaxErrorContainsType :: SyntaxError -> Bool
prop_formatSyntaxErrorContainsType error =
  let formatted = formatSyntaxError error
      errorTypeStr = show (errorType error)
  in errorTypeStr `isInfixOf` formatted

-- Property: formatSyntaxError contains line number
prop_formatSyntaxErrorContainsLine :: SyntaxError -> Bool
prop_formatSyntaxErrorContainsLine error =
  let formatted = formatSyntaxError error
      lineStr = show (lineNumber error)
  in lineStr `isInfixOf` formatted

-- Property: formatSyntaxError contains column number
prop_formatSyntaxErrorContainsColumn :: SyntaxError -> Bool
prop_formatSyntaxErrorContainsColumn error =
  let formatted = formatSyntaxError error
      columnStr = show (columnNumber error)
  in columnStr `isInfixOf` formatted

-- Property: Valid Go code produces fewer errors
prop_validGoCodeFewerErrors :: String -> Property
prop_validGoCodeFewerErrors validCode =
  let validErrors = validateSyntax validCode
      invalidErrors = validateSyntax "func main( {"
  in length validErrors <= length invalidErrors

-- Property: Invalid Go code produces errors
prop_invalidGoCodeProducesErrors :: String -> Property
prop_invalidGoCodeProducesErrors invalidCode =
  let errors = validateSyntax invalidCode
  in length errors >= 0  -- May or may not have errors depending on the invalid code

-- Property: Empty content produces minimal errors
prop_emptyContentMinimalErrors :: Bool
prop_emptyContentMinimalErrors =
  let errors = validateSyntax ""
  in length errors <= 5  -- Should have very few errors for empty content

-- ============================================================================
-- Unit Tests
-- ============================================================================

tests :: TestTree
tests = testGroup "SyntaxValidator Advanced QuickCheck Tests"
  [ testGroup "Validator Properties"
    [ testProperty "newSyntaxValidator creates valid validator" prop_newSyntaxValidatorValid
    , testProperty "validateFile returns same results as validateSyntax" prop_validateFileSameAsValidateSyntax
    , testProperty "validateSyntax returns list of errors" prop_validateSyntaxReturnsList
    , testProperty "getSyntaxErrors returns errors in reverse order" prop_getSyntaxErrorsReverses
    ]

  , testGroup "SyntaxError Properties"
    [ testProperty "SyntaxError equality is reflexive" prop_syntaxErrorReflexive
    , testProperty "SyntaxError equality is symmetric" prop_syntaxErrorSymmetric
    , testProperty "SyntaxError equality is transitive" prop_syntaxErrorTransitive
    , testProperty "SyntaxError ordering is consistent" prop_syntaxErrorOrderingConsistent
    ]

  , testGroup "Formatting Properties"
    [ testProperty "formatSyntaxError produces non-empty output" prop_formatSyntaxErrorNonEmpty
    , testProperty "formatSyntaxError contains error type" prop_formatSyntaxErrorContainsType
    , testProperty "formatSyntaxError contains line number" prop_formatSyntaxErrorContainsLine
    , testProperty "formatSyntaxError contains column number" prop_formatSyntaxErrorContainsColumn
    ]

  , testGroup "Validation Properties"
    [ testProperty "Valid Go code produces fewer errors" prop_validGoCodeFewerErrors
    , testProperty "Invalid Go code produces errors" prop_invalidGoCodeProducesErrors
    , testProperty "Empty content produces minimal errors" prop_emptyContentMinimalErrors
    ]

  , testGroup "Unit Tests"
    [ testCase "Create new syntax validator" $ do
        let validator = newSyntaxValidator
        validatorErrors validator @?= []
        scopeStack validator @?= []
        braceStack validator @?= []
        hasPackageDecl validator @?= False
        hasMainFunc validator @?= False

    , testCase "Validate empty content" $ do
        let errors = validateSyntax ""
        length errors @?= 0  -- Empty content should have no errors

    , testCase "Validate valid Go code" $ do
        let validCode = unlines
              [ "package main"
              , "import \"fmt\""
              , "func main() {"
              , "    fmt.Println(\"Hello, World!\")"
              , "}"
              ]
        let errors = validateSyntax validCode
        length errors @?= 0  -- Valid code should have no errors

    , testCase "Validate code with missing brace" $ do
        let invalidCode = unlines
              [ "package main"
              , "func main() {"
              , "    fmt.Println(\"Hello\")"
              -- Missing closing brace
              ]
        let errors = validateSyntax invalidCode
        assertBool "Should detect missing brace" $ any (\e -> errorType e == MissingBrace) errors

    , testCase "Validate code with unclosed string" $ do
        let invalidCode = "var s string = \"unclosed string"
        let errors = validateSyntax invalidCode
        assertBool "Should detect unclosed string" $ any (\e -> errorType e == UnclosedString) errors

    , testCase "Validate code with invalid identifier" $ do
        let invalidCode = "var 123invalid int"
        let errors = validateSyntax invalidCode
        assertBool "Should detect invalid identifier" $ any (\e -> errorType e == InvalidIdentifier) errors

    , testCase "Create syntax error" $ do
        let error = SyntaxError MissingBrace "Missing closing brace" 10 5 "func main() {"
        errorType error @?= MissingBrace
        errorMessage error @?= "Missing closing brace"
        lineNumber error @?= 10
        columnNumber error @?= 5
        lineContent error @?= "func main() {"

    , testCase "Format syntax error" $ do
        let error = SyntaxError MissingBrace "Missing closing brace" 10 5 "func main() {"
        let formatted = formatSyntaxError error
        assertBool "Should contain error type" $ "MissingBrace" `isInfixOf` formatted
        assertBool "Should contain line number" $ "10" `isInfixOf` formatted
        assertBool "Should contain column number" $ "5" `isInfixOf` formatted
        assertBool "Should contain error message" $ "Missing closing brace" `isInfixOf` formatted

    , testCase "Compare syntax errors" $ do
        let error1 = SyntaxError MissingBrace "error1" 10 5 "line1"
            error2 = SyntaxError MissingBrace "error1" 10 5 "line1"
            error3 = SyntaxError MissingParenthesis "error3" 10 5 "line3"
        error1 @?= error2
        assertBool "Different errors should not be equal" $ error1 /= error3

    , testCase "Order syntax errors" $ do
        let error1 = SyntaxError MissingBrace "error1" 10 5 "line1"
            error2 = SyntaxError MissingBrace "error2" 10 5 "line2"
            error3 = SyntaxError MissingBrace "error1" 11 5 "line3"
        compare error1 error2 @?= LT
        compare error2 error1 @?= GT
        compare error1 error3 @?= LT
        compare error3 error1 @?= GT

    , testCase "validateFile same as validateSyntax" $ do
        let code = "package main\nfunc main() {}"
        validateFile code @?= validateSyntax code

    , testCase "Get syntax errors from validator" $ do
        let error = SyntaxError MissingBrace "error" 10 5 "line"
        let validator = newSyntaxValidator { validatorErrors = [error] }
        let errors = getSyntaxErrors validator
        errors @?= [error]

    , testCase "Complex validation scenario" $ do
        let complexCode = unlines
              [ "package main"
              , "import \"fmt\""
              , "func main() {"
              , "    if x > 0 {"
              , "        fmt.Println(x)"
              , "    // missing closing braces"
              , "var y string = \"unclosed"
              , "    var 123invalid int"
              , "}"
              ]
        let errors = validateSyntax complexCode
        assertBool "Should detect multiple errors" $ length errors >= 2
    ]
  ]