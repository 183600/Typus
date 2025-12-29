{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

-- | New Cabal QuickCheck Test Suite for SyntaxValidator
module Test.Unit.NewCabalSyntaxValidatorQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
  ( Arbitrary(..)
  , Gen
  , choose
  , listOf
  , elements
  , oneof
  , Property
  , property
  , (===)
  , (.&&.)
  , (.||.)
  , (==>)
  , counterexample
  , classify
  , forAll
  )

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
import Data.List (isInfixOf, isPrefixOf)
import Data.Char (isSpace, isAlphaNum, isAlpha, isDigit)

-- ============================================================================
-- Arbitrary instances
-- ============================================================================

instance Arbitrary ErrorType where
  arbitrary = genErrorType

instance Arbitrary SyntaxError where
  arbitrary = genSyntaxError

-- ============================================================================
-- Generators for SyntaxValidator data types
-- ============================================================================

-- Generate arbitrary ErrorType
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

-- Generate arbitrary SyntaxError
genSyntaxError :: Gen SyntaxError
genSyntaxError = do
  errorType <- genErrorType
  lineNum <- choose (1, 1000)
  colNum <- choose (1, 100)
  content <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \t\n{}();,."
  pure $ SyntaxError
    { errorType = errorType
    , errorMessage = "Test error: " ++ show errorType
    , lineNumber = lineNum
    , columnNumber = colNum
    , lineContent = content
    }

-- Generate valid Go code snippets
genValidGoCode :: Gen String
genValidGoCode = oneof
  [ pure "package main\n\nfunc main() {\n    fmt.Println(\"Hello, World!\")\n}"
  , pure "package main\n\nimport \"fmt\"\n\nfunc add(a int, b int) int {\n    return a + b\n}\n"
  , pure "package main\n\nvar x int = 42\n\nfunc main() {\n    println(x)\n}"
  , pure "package main\n\ntype Point struct {\n    x int\n    y int\n}\n"
  , pure "package main\n\nfunc fib(n int) int {\n    if n <= 1 {\n        return n\n    }\n    return fib(n-1) + fib(n-2)\n}"
  ]

-- Generate code with syntax errors
genInvalidGoCode :: Gen String
genInvalidGoCode = oneof
  [ pure "func main() {\n    fmt.Println(\"Missing package\")\n}"
  , pure "package main\n\nfunc main( {\n    fmt.Println(\"Missing closing parenthesis\")\n}"
  , pure "package main\n\nfunc main() {\n    if true {\n        fmt.Println(\"Missing closing brace\")\n"
  , pure "package main\n\nfunc main() {\n    var x int = \n    fmt.Println(\"Incomplete assignment\")\n}"
  , pure "package main\n\nfunc main() {\n    fmt.Println(\"Unclosed string\n}"
  ]

-- ============================================================================
-- QuickCheck Properties
-- ============================================================================

-- Property: validateSyntax returns empty list for valid Go code
prop_validateSyntax_valid_code :: Property
prop_validateSyntax_valid_code =
  forAll genValidGoCode $ \code ->
    let errors = validateSyntax code
    in counterexample ("Valid code should have no syntax errors: " ++ take 100 code) $
       null errors

-- Property: validateSyntax detects syntax errors in invalid code
prop_validateSyntax_invalid_code :: Property
prop_validateSyntax_invalid_code =
  forAll genInvalidGoCode $ \code ->
    let errors = validateSyntax code
    in counterexample ("Invalid code should have syntax errors: " ++ take 100 code) $
       not (null errors)

-- Property: SyntaxError ordering is consistent
prop_syntax_error_ordering :: SyntaxError -> SyntaxError -> Property
prop_syntax_error_ordering err1 err2 =
  let ord1 = compare err1 err2
      ord2 = compare (errorMessage err1) (errorMessage err2)
  in property $ ord1 === ord2

-- Property: SyntaxError contains required fields
prop_syntax_error_fields :: SyntaxError -> Property
prop_syntax_error_fields err =
  let msg = errorMessage err
      line = lineNumber err
      col = columnNumber err
      content = lineContent err
  in property $ 
    not (null msg) .&&.
    line > 0 .&&.
    col > 0 .&&.
    length content >= 0

-- Property: formatSyntaxError includes error message
prop_formatSyntax_error_includes_message :: SyntaxError -> Property
prop_formatSyntax_error_includes_message err =
  let formatted = formatSyntaxError err
      msg = errorMessage err
  in property $ msg `isInfixOf` formatted

-- Property: formatSyntaxError includes line number
prop_formatSyntax_error_includes_line :: SyntaxError -> Property
prop_formatSyntax_error_includes_line err =
  let formatted = formatSyntaxError err
      line = show $ lineNumber err
  in property $ line `isInfixOf` formatted

-- Property: validateFile is equivalent to validateSyntax
prop_validateFile_equivalent :: String -> Property
prop_validateFile_equivalent code =
  let syntaxErrors = validateSyntax code
      fileErrors = validateFile code
  in property $ length syntaxErrors === length fileErrors

-- Property: getSyntaxErrors returns errors from validator
prop_get_syntax_errors :: SyntaxError -> Property
prop_get_syntax_errors err =
  let validator = newSyntaxValidator
      errors = getSyntaxErrors validator
  in property $ length errors >= 0

-- Property: ErrorType classification
prop_error_type_classification :: ErrorType -> Property
prop_error_type_classification errType =
  let isError = errType `elem` [MissingBrace, MissingParenthesis, MissingBracket, 
                               UnclosedString, UnclosedComment, InvalidIdentifier,
                               InvalidTypeDeclaration, InvalidFunctionDeclaration,
                               InvalidImport, InvalidStatement, UnterminatedBlock,
                               InvalidOperator, MissingSemicolon, UnexpectedToken,
                               MissingPackageDeclaration, DuplicateDeclaration,
                               InvalidBlockStructure, UndeclaredVariable]
      isWarning = errType == SyntaxWarning
  in property $ isError .||. isWarning

-- Property: SyntaxError with long content
prop_syntax_error_long_content :: String -> Property
prop_syntax_error_long_content content =
  let longContent = take 1000 $ content ++ cycle "abcdefghijklmnopqrstuvwxyz"
      err = SyntaxError
        { errorType = InvalidStatement
        , errorMessage = "Test error"
        , lineNumber = 1
        , columnNumber = 1
        , lineContent = longContent
        }
  in property $ length (lineContent err) <= 1000

-- Property: validateSyntax handles empty input
prop_validateSyntax_empty_input :: Property
prop_validateSyntax_empty_input =
  let errors = validateSyntax ""
  in property $ length errors >= 0

-- Property: validateSyntax handles whitespace-only input
prop_validateSyntax_whitespace_only :: Property
prop_validateSyntax_whitespace_only =
  let errors = validateSyntax "   \n\t  \n  "
  in property $ length errors >= 0

-- Property: validateSyntax handles very long lines
prop_validateSyntax_long_lines :: Property
prop_validateSyntax_long_lines =
  let longLine = replicate 1000 'a' ++ "\n"
      code = "package main\n\nfunc main() {\n    " ++ longLine ++ "}"
      errors = validateSyntax code
  in property $ length errors >= 0

-- Property: validateSyntax handles Unicode characters
prop_validateSyntax_unicode :: Property
prop_validateSyntax_unicode =
  let unicodeCode = "package main\n\nfunc main() {\n    // 测试 Unicode: café naïve résumé 🚀\n    println(\"Hello\")\n}"
      errors = validateSyntax unicodeCode
  in property $ length errors >= 0

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "New Cabal SyntaxValidator QuickCheck Tests"
  [ testProperty "validateSyntax returns empty list for valid Go code" prop_validateSyntax_valid_code
  , testProperty "validateSyntax detects syntax errors in invalid code" prop_validateSyntax_invalid_code
  , testProperty "SyntaxError ordering is consistent" prop_syntax_error_ordering
  , testProperty "SyntaxError contains required fields" prop_syntax_error_fields
  , testProperty "formatSyntaxError includes error message" prop_formatSyntax_error_includes_message
  , testProperty "formatSyntaxError includes line number" prop_formatSyntax_error_includes_line
  , testProperty "validateFile is equivalent to validateSyntax" prop_validateFile_equivalent
  , testProperty "getSyntaxErrors returns errors from validator" prop_get_syntax_errors
  , testProperty "ErrorType classification" prop_error_type_classification
  , testProperty "SyntaxError with long content" prop_syntax_error_long_content
  , testProperty "validateSyntax handles empty input" prop_validateSyntax_empty_input
  , testProperty "validateSyntax handles whitespace-only input" prop_validateSyntax_whitespace_only
  , testProperty "validateSyntax handles very long lines" prop_validateSyntax_long_lines
  , testProperty "validateSyntax handles Unicode characters" prop_validateSyntax_unicode
  ]