{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewSyntaxValidatorQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import qualified Test.QuickCheck as QC

import SyntaxValidator
  ( SyntaxValidator(..)
  , SyntaxError(..)
  , ErrorType(..)
  , Token(..)
  , Scope(..)
  , Language(..)
  , ParseState(..)
  , newSyntaxValidator
  , validateSyntax
  , validateFile
  , getSyntaxErrors
  , formatSyntaxError
  , detectLanguage
  , tokenize
  , createGlobalScope
  )

import Data.Text (Text)
import qualified Data.Text as T
import Data.List (sort, nub, intercalate)
import Data.Maybe (isJust, isNothing, fromMaybe)
import qualified Data.Set as Set
import Data.Set (Set)

-- ============================================================================
-- Arbitrary Instances for QuickCheck Testing
-- ============================================================================

-- Generate arbitrary error type
instance Arbitrary ErrorType where
  arbitrary = QC.elements
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

-- Generate arbitrary syntax error
instance Arbitrary SyntaxError where
  arbitrary = do
    errorType <- QC.arbitrary
    errorMessage <- QC.arbitrary
    lineNumber <- QC.choose (1, 1000)
    columnNumber <- QC.choose (1, 1000)
    lineContent <- QC.arbitrary
    return $ SyntaxError errorType errorMessage lineNumber columnNumber lineContent

-- Generate arbitrary token
instance Arbitrary Token where
  arbitrary = QC.oneof
    [ TString <$> QC.arbitrary <*> QC.choose (1, 1000) <*> QC.choose (1, 1000)
    , TComment <$> QC.arbitrary <*> QC.choose (1, 1000) <*> QC.choose (1, 1000)
    , TIdentifier <$> QC.arbitrary <*> QC.choose (1, 1000) <*> QC.choose (1, 1000)
    , TKeyword <$> QC.arbitrary <*> QC.choose (1, 1000) <*> QC.choose (1, 1000)
    , TOperator <$> QC.arbitrary <*> QC.choose (1, 1000) <*> QC.choose (1, 1000)
    , TDelimiter <$> QC.elements "{}[](),;" <*> QC.choose (1, 1000) <*> QC.choose (1, 1000)
    , TNumber <$> QC.arbitrary <*> QC.choose (1, 1000) <*> QC.choose (1, 1000)
    , TWhitespace <$> QC.choose (1, 1000) <*> QC.choose (1, 1000)
    , TNewline <$> QC.choose (1, 1000)
    , TUnknown <$> QC.arbitrary <*> QC.choose (1, 1000) <*> QC.choose (1, 1000)
    ]

-- Generate arbitrary language
instance Arbitrary Language where
  arbitrary = QC.elements [Go, Typus, GoAndTypus, Unknown]

-- ============================================================================
-- Property Tests for Syntax Validation
-- ============================================================================

-- Property: New syntax validator has no errors
prop_new_validator_no_errors :: Property
prop_new_validator_no_errors =
  let validator = newSyntaxValidator
  in property $ L.null (validatorErrors validator)

-- Property: New syntax validator has global scope
prop_new_validator_global_scope :: Property
prop_new_validator_global_scope =
  let validator = newSyntaxValidator
      scope = currentScope validator
  in property $ scopeName scope === "global" .&&.
             Set.L.null (scopeVariables scope) .&&.
             Set.L.null (scopeFunctions scope) .&&.
             parentScope scope === Nothing

-- Property: Create global scope creates correct scope
prop_create_global_scope :: Property
prop_create_global_scope =
  let scope = createGlobalScope
  in property $ scopeName scope === "global" .&&.
             Set.L.null (scopeVariables scope) .&&.
             Set.L.null (scopeFunctions scope) .&&.
             parentScope scope === Nothing

-- Property: Syntax error preserves L.all fields
prop_syntax_error_preserves_fields :: ErrorType -> String -> Int -> Int -> String -> Property
prop_syntax_error_preserves_fields errorType message line col content =
  let error = SyntaxError errorType message line col content
  in property $ errorType error === errorType .&&.
             errorMessage error === message .&&.
             lineNumber error === line .&&.
             columnNumber error === col .&&.
             lineContent error === content

-- Property: Syntax error ordering works correctly
prop_syntax_error_ordering :: SyntaxError -> SyntaxError -> Property
prop_syntax_error_ordering error1 error2 =
  let ordering = compare error1 error2
      msgOrdering = compare (errorMessage error1) (errorMessage error2)
      lineOrdering = compare (lineNumber error1) (lineNumber error2)
      colOrdering = compare (columnNumber error1) (columnNumber error2)
  in property $ if msgOrdering /= EQ
               then ordering === msgOrdering
               else if lineOrdering /= EQ
                    then ordering === lineOrdering
                    else ordering === colOrdering

-- Property: Token preserves position information
prop_token_preserves_position :: Token -> Property
prop_token_preserves_position token =
  let (line, col) = case token of
        TString _ l c -> (l, c)
        TComment _ l c -> (l, c)
        TIdentifier _ l c -> (l, c)
        TKeyword _ l c -> (l, c)
        TOperator _ l c -> (l, c)
        TDelimiter _ l c -> (l, c)
        TNumber _ l c -> (l, c)
        TWhitespace l c -> (l, c)
        TNewline l -> (l, 0)
        TUnknown _ l c -> (l, c)
  in property $ line > 0 .&&. (col >= 0)

-- Property: Language detection works for Go code
prop_language_detection_go :: Property
prop_language_detection_go =
  let goCode = "package main\nfunc main() {}"
      lang = detectLanguage goCode
  in property $ lang === Go

-- Property: Language detection works for Typus code
prop_language_detection_typus :: Property
prop_language_detection_typus =
  let typusCode = "//! ownership: on\nfunc test() {}"
      lang = detectLanguage typusCode
  in property $ lang === Typus

-- Property: Language detection works for mixed code
prop_language_detection_mixed :: Property
prop_language_detection_mixed =
  let mixedCode = "package main\n//! ownership: on\nfunc main() {}"
      lang = detectLanguage mixedCode
  in property $ lang === GoAndTypus

-- Property: Language detection returns unknown for empty input
prop_language_detection_empty :: Property
prop_language_detection_empty =
  let lang = detectLanguage ""
  in property $ lang === Unknown

-- Property: Tokenization produces at least one token for non-empty input
prop_tokenization_non_empty :: Property
prop_tokenization_non_empty =
  let code = "func main() {}"
      tokens = tokenize code
  in property $ not (null tokens)

-- Property: Tokenization handles empty input
prop_tokenization_empty :: Property
prop_tokenization_empty =
  let tokens = tokenize ""
  in property $ null tokens

-- Property: Tokenization preserves newlines
prop_tokenization_preserves_newlines :: Int -> Property
prop_tokenization_preserves_newlines numLines =
  numLines >= 0 && numLines <= 10 ==>
  let code = unlines (replicate numLines "x")
      tokens = tokenize code
      newlineTokens = L.length $ filter isNewlineToken tokens
  in property $ newlineTokens === numLines
  where
    isNewlineToken (TNewline _) = True
    isNewlineToken _ = False

-- Property: Tokenization handles strings correctly
prop_tokenization_handles_strings :: String -> Property
prop_tokenization_handles_strings content =
  not (L.any (`elem` "\\\"") content) ==>
  let code = "s := \"" ++ content ++ "\""
      tokens = tokenize code
      stringTokens = filter isStringToken tokens
  in property $ not (null stringTokens)
  where
    isStringToken (TString _ _ _) = True
    isStringToken _ = False

-- Property: Tokenization handles comments correctly
prop_tokenization_handles_comments :: String -> Property
prop_tokenization_handles_comments content =
  not (L.any (`elem` "\n\r") content) ==>
  let code = "// " ++ content ++ "\nfunc main() {}"
      tokens = tokenize code
      commentTokens = filter isCommentToken tokens
  in property $ not (null commentTokens)
  where
    isCommentToken (TComment _ _ _) = True
    isCommentToken _ = False

-- Property: Validation accepts valid Go code
prop_validation_accepts_valid_go :: Property
prop_validation_accepts_valid_go =
  let goCode = unlines
        [ "package main"
        , "import \"fmt\""
        , "func main() {"
        , "    fmt.Println(\"Hello, World!\")"
        , "}"
        ]
      errors = validateSyntax goCode
  in property $ null errors

-- Property: Validation accepts valid Typus code
prop_validation_accepts_valid_typus :: Property
prop_validation_accepts_valid_typus =
  let typusCode = unlines
        [ "//! ownership: on"
        , "package main"
        , "func main() {"
        , "    x := 42"
        , "    println(x)"
        , "}"
        ]
      errors = validateSyntax typusCode
  in property $ null errors

-- Property: Validation detects missing braces
prop_validation_detects_missing_braces :: Property
prop_validation_detects_missing_braces =
  let code = "func main() {\n    println(\"test\")\n"  -- Missing closing brace
      errors = validateSyntax code
      hasBraceError = L.any isMissingBraceError errors
  in property $ hasBraceError
  where
    isMissingBraceError (SyntaxError MissingBrace _ _ _ _) = True
    isMissingBraceError _ = False

-- Property: Validation detects missing package declaration
prop_validation_detects_missing_package :: Property
prop_validation_detects_missing_package =
  let code = "func main() {\n    println(\"test\")\n}\n"  -- Missing package declaration
      errors = validateSyntax code
      hasPackageError = L.any isMissingPackageError errors
  in property $ hasPackageError
  where
    isMissingPackageError (SyntaxError MissingPackageDeclaration _ _ _ _) = True
    isMissingPackageError _ = False

-- Property: Validation detects duplicate declarations
prop_validation_detects_duplicate_declarations :: String -> Property
prop_validation_detects_duplicate_declarations name =
  not (null name) ==>
  let code = unlines
        [ "package main"
        , "func " ++ name ++ "() {}"
        , "func " ++ name ++ "() {}"  -- Duplicate function
        ]
      errors = validateSyntax code
      hasDuplicateError = L.any isDuplicateDeclarationError errors
  in property $ hasDuplicateError
  where
    isDuplicateDeclarationError (SyntaxError DuplicateDeclaration _ _ _ _) = True
    isDuplicateDeclarationError _ = False

-- Property: Get syntax errors returns errors in L.reverse order
prop_get_syntax_errors_reverse_order :: SyntaxValidator -> Property
prop_get_syntax_errors_reverse_order validator =
  let errors = getSyntaxErrors validator
      originalErrors = validatorErrors validator
  in property $ errors === L.reverse originalErrors

-- Property: Format syntax error contains expected information
prop_format_syntax_error_contains_info :: SyntaxError -> Property
prop_format_syntax_error_contains_info error =
  let formatted = formatSyntaxError error
      hasType = show (errorType error) `L.isInfixOf` formatted
      hasMessage = errorMessage error `L.isInfixOf` formatted
      hasLocation = lineNumber error > 0 && columnNumber error > 0
  in property $ hasType .&&. hasMessage .&&.
             (if hasLocation then ("Line " ++ show (lineNumber error)) `L.isInfixOf` formatted else property True)

-- Property: Validate file is same as validate syntax
prop_validate_file_same_as_validate_syntax :: String -> Property
prop_validate_file_same_as_validate_syntax content =
  let errors1 = validateFile content
      errors2 = validateSyntax content
  in property $ errors1 === errors2

-- Property: Validation handles empty input
prop_validation_handles_empty_input :: Property
prop_validation_handles_empty_input =
  let errors = validateSyntax ""
  in property $ null errors

-- Property: Validation handles whitespace-only input
prop_validation_handles_whitespace_only :: String -> Property
prop_validation_handles_whitespace_only whitespace =
  L.all (`elem` " \t\n\r") whitespace ==>
  let errors = validateSyntax whitespace
  in property $ null errors

-- Property: Validation detects invalid function declarations
prop_validation_detects_invalid_function :: Property
prop_validation_detects_invalid_function =
  let code = "func\n"  -- Invalid function declaration
      errors = validateSyntax code
      hasFunctionError = L.any isInvalidFunctionError errors
  in property $ hasFunctionError
  where
    isInvalidFunctionError (SyntaxError InvalidFunctionDeclaration _ _ _ _) = True
    isInvalidFunctionError _ = False

-- Property: Validation detects invalid import declarations
prop_validation_detects_invalid_import :: Property
prop_validation_detects_invalid_import =
  let code = "import\n"  -- Invalid import declaration
      errors = validateSyntax code
      hasImportError = L.any isInvalidImportError errors
  in property $ hasImportError
  where
    isInvalidImportError (SyntaxError InvalidImport _ _ _ _) = True
    isInvalidImportError _ = False

-- Property: Validation handles complex nested structures
prop_validation_handles_nested_structures :: Int -> Property
prop_validation_handles_nested_structures depth =
  depth >= 0 && depth <= 5 ==>
  let nestedBraces = replicate depth '{' ++ replicate depth '}'
      code = "func main() " ++ nestedBraces ++ "\n"
      errors = validateSyntax code
  in property $ null errors  -- Balanced braces should not produce errors

-- Property: Tokenization handles operators correctly
prop_tokenization_handles_operators :: Property
prop_tokenization_handles_operators =
  let code = "x := a + b * c / d - e"
      tokens = tokenize code
      operatorTokens = filter isOperatorToken tokens
  in property $ L.length operatorTokens >= 4  -- Should find +, *, -, /
  where
    isOperatorToken (TOperator _ _ _) = True
    isOperatorToken _ = False

-- Property: Tokenization handles delimiters correctly
prop_tokenization_handles_delimiters :: Property
prop_tokenization_handles_delimiters =
  let code = "func main() { return (1 + 2) }"
      tokens = tokenize code
      delimiterTokens = filter isDelimiterToken tokens
  in property $ L.length delimiterTokens >= 6  -- Should find (, ), {, }
  where
    isDelimiterToken (TDelimiter _ _ _) = True
    isDelimiterToken _ = False

-- Property: Tokenization handles identifiers correctly
prop_tokenization_handles_identifiers :: String -> Property
prop_tokenization_handles_identifiers identifier =
  not (null identifier) && L.all (`elem` ('_' : ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'])) identifier ==>
  let code = "var " ++ identifier ++ " int"
      tokens = tokenize code
      identifierTokens = filter isIdentifierToken tokens
  in property $ not (null identifierTokens)
  where
    isIdentifierToken (TIdentifier name _ _) = name == identifier
    isIdentifierToken _ = False

-- Property: Validation handles control flow structures
prop_validation_handles_control_flow :: Property
prop_validation_handles_control_flow =
  let code = unlines
        [ "package main"
        , "func main() {"
        , "    if true {"
        , "        println(\"true\")"
        , "    } else {"
        , "        println(\"false\")"
        , "    }"
        , "}"
        ]
      errors = validateSyntax code
  in property $ null errors

tests :: TestTree
tests =
  testGroup "New Syntax Validator QuickCheck Tests"
    [ fastProperty "New syntax validator has no errors" prop_new_validator_no_errors
    , fastProperty "New syntax validator has global scope" prop_new_validator_global_scope
    , fastProperty "Create global scope creates correct scope" prop_create_global_scope
    , fastProperty "Syntax error preserves L.all fields" prop_syntax_error_preserves_fields
    , fastProperty "Syntax error ordering works correctly" prop_syntax_error_ordering
    , fastProperty "Token preserves position information" prop_token_preserves_position
    , fastProperty "Language detection works for Go code" prop_language_detection_go
    , fastProperty "Language detection works for Typus code" prop_language_detection_typus
    , fastProperty "Language detection works for mixed code" prop_language_detection_mixed
    , fastProperty "Language detection returns unknown for empty input" prop_language_detection_empty
    , fastProperty "Tokenization produces at least one token for non-empty input" prop_tokenization_non_empty
    , fastProperty "Tokenization handles empty input" prop_tokenization_empty
    , fastProperty "Tokenization preserves newlines" prop_tokenization_preserves_newlines
    , fastProperty "Tokenization handles strings correctly" prop_tokenization_handles_strings
    , fastProperty "Tokenization handles comments correctly" prop_tokenization_handles_comments
    , fastProperty "Validation accepts valid Go code" prop_validation_accepts_valid_go
    , fastProperty "Validation accepts valid Typus code" prop_validation_accepts_valid_typus
    , fastProperty "Validation detects missing braces" prop_validation_detects_missing_braces
    , fastProperty "Validation detects missing package declaration" prop_validation_detects_missing_package
    , fastProperty "Validation detects duplicate declarations" prop_validation_detects_duplicate_declarations
    , fastProperty "Get syntax errors returns errors in L.reverse order" prop_get_syntax_errors_reverse_order
    , fastProperty "Format syntax error contains expected information" prop_format_syntax_error_contains_info
    , fastProperty "Validate file is same as validate syntax" prop_validate_file_same_as_validate_syntax
    , fastProperty "Validation handles empty input" prop_validation_handles_empty_input
    , fastProperty "Validation handles whitespace-only input" prop_validation_handles_whitespace_only
    , fastProperty "Validation detects invalid function declarations" prop_validation_detects_invalid_function
    , fastProperty "Validation detects invalid import declarations" prop_validation_detects_invalid_import
    , fastProperty "Validation handles complex nested structures" prop_validation_handles_nested_structures
    , fastProperty "Tokenization handles operators correctly" prop_tokenization_handles_operators
    , fastProperty "Tokenization handles delimiters correctly" prop_tokenization_handles_delimiters
    , fastProperty "Tokenization handles identifiers correctly" prop_tokenization_handles_identifiers
    , fastProperty "Validation handles control flow structures" prop_validation_handles_control_flow
    ]