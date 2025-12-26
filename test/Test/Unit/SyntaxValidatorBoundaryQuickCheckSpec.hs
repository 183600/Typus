{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.SyntaxValidatorBoundaryQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import SyntaxValidator
import Data.Set (Set)
import qualified Data.Set as Set

-- ============================================================================
-- Test Data Generation
-- ============================================================================

-- | Generate error types
instance Arbitrary ErrorType where
  arbitrary = elements 
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

-- | Generate syntax errors
instance Arbitrary SyntaxError where
  arbitrary = do
    errorType <- arbitrary
    message <- arbitraryString
    line <- positive
    column <- positive
    lineContent <- arbitraryString
    return $ SyntaxError errorType message line column lineContent
    where
      positive = getPositive <$> arbitrary

-- | Generate tokens
instance Arbitrary Token where
  arbitrary = oneof
    [ TString <$> arbitraryString <*> positive <*> positive
    , TComment <$> arbitraryString <*> positive <*> positive
    , TIdentifier <$> arbitraryIdentifier <*> positive <*> positive
    , TKeyword <$> arbitraryKeyword <*> positive <*> positive
    , TOperator <$> arbitraryOperator <*> positive <*> positive
    , TDelimiter <$> arbitrary <*> positive <*> positive
    , TNumber <$> arbitraryNumber <*> positive <*> positive
    , TWhitespace <$> positive <*> positive
    , TNewline <$> positive
    , TUnknown <$> arbitraryString <*> positive <*> positive
    ]
    where
      positive = getPositive <$> arbitrary

-- | Generate scopes
instance Arbitrary Scope where
  arbitrary = do
    name <- arbitraryString
    variables <- arbitrarySet arbitraryIdentifier
    functions <- arbitrarySet arbitraryIdentifier
    parent <- arbitraryMaybe arbitrary
    return $ Scope name variables functions parent

-- | Generate languages
instance Arbitrary Language where
  arbitrary = elements [Go, Typus, GoAndTypus, Unknown]

-- | Generate syntax validators
instance Arbitrary SyntaxValidator where
  arbitrary = do
    errors <- listOf arbitrary
    currentScope <- arbitrary
    scopeStack <- listOf arbitrary
    braceStack <- listOf ((,,) <$> arbitrary <*> positive <*> positive)
    language <- arbitrary
    tokens <- listOf arbitrary
    hasPackageDecl <- arbitrary
    hasMainFunc <- arbitrary
    return $ SyntaxValidator errors currentScope scopeStack braceStack language tokens hasPackageDecl hasMainFunc
    where
      positive = getPositive <$> arbitrary

-- | Generate arbitrary strings
arbitraryString :: Gen String
arbitraryString = do
  size <- choose (0, 20)
  vectorOf size $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \n\t;{}()[]"

-- | Generate identifiers
arbitraryIdentifier :: Gen String
arbitraryIdentifier = do
  first <- elements ['a'..'z'] ++ ['A'..'Z'] ++ ['_']
  rest <- vectorOf $ choose (0, 10) $ elements ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['_']
  return $ first : rest

-- | Generate keywords
arbitraryKeyword :: Gen String
arbitraryKeyword = elements
  [ "package", "import", "func", "var", "const", "if", "else", "for", "switch"
  , "case", "default", "break", "continue", "return", "go", "defer", "select"
  , "chan", "struct", "interface", "map", "type", "range"
  ]

-- | Generate operators
arbitraryOperator :: Gen String
arbitraryOperator = elements
  [ "+", "-", "*", "/", "%", "==", "!=", "<", "<=", ">", ">=", "&&", "||", "!"
  , "&", "|", "^", "<<", ">>", "&^", "+=", "-=", "*=", "/=", "%=", "&="
  , "|=", "^=", "<<=", ">>=", "&^=", "++", "--", ":=", "=", "(", ")"
  ]

-- | Generate numbers
arbitraryNumber :: Gen String
arbitraryNumber = do
  intPart <- listOf $ elements ['0'..'9']
  fracPart <- oneof [return "", (:) <$> return '.' <*> listOf (elements ['0'..'9'])]
  return $ intPart ++ fracPart

-- | Generate arbitrary sets
arbitrarySet :: Ord a => Gen a -> Gen (Set a)
arbitrarySet gen = Set.fromList <$> listOf gen

-- | Generate arbitrary maybes
arbitraryMaybe :: Gen a -> Gen (Maybe a)
arbitraryMaybe gen = oneof [return Nothing, Just <$> gen]

-- ============================================================================
-- QuickCheck Properties for Syntax Validator Boundary Conditions
-- ============================================================================

-- | Syntax error should preserve all components
prop_syntax_error_preserves :: ErrorType -> String -> Int -> Int -> String -> Property
prop_syntax_error_preserves errType message line column lineContent =
  let error = SyntaxError errType message line column lineContent
  in errorType error === errType .&&.
     errorMessage error === message .&&.
     lineNumber error === line .&&.
     columnNumber error === column .&&.
     lineContent error === lineContent

-- | Syntax error ordering should be consistent
prop_syntax_error_ordering :: SyntaxError -> SyntaxError -> Property
prop_syntax_error_ordering err1 err2 =
  let ord1 = compare err1 err2
      ord2 = compare err2 err1
  in (ord1 == EQ) ==> (ord2 === EQ) .&&. (ord1 === EQ)

-- | Token should preserve position information
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
  in line > 0 .&&. col >= 0

-- | Token types should be distinguishable
prop_token_types_distinguishable :: Token -> Token -> Property
prop_token_types_distinguishable token1 token2 =
  let sameType = case (token1, token2) of
        (TString {}, TString {}) -> True
        (TComment {}, TComment {}) -> True
        (TIdentifier {}, TIdentifier {}) -> True
        (TKeyword {}, TKeyword {}) -> True
        (TOperator {}, TOperator {}) -> True
        (TDelimiter {}, TDelimiter {}) -> True
        (TNumber {}, TNumber {}) -> True
        (TWhitespace {}, TWhitespace {}) -> True
        (TNewline {}, TNewline {}) -> True
        (TUnknown {}, TUnknown {}) -> True
        _ -> False
  in (token1 == token2) ==> sameType

-- | Scope should preserve declarations
prop_scope_preserves_declarations :: String -> Set.Set String -> Set.Set String -> Maybe Scope -> Property
prop_scope_preserves_declarations name variables functions parent =
  let scope = Scope name variables functions parent
  in scopeName scope === name .&&.
     scopeVariables scope === variables .&&.
     scopeFunctions scope === functions .&&.
     parentScope scope === parent

-- | Global scope should be empty
prop_global_scope_empty :: Property
prop_global_scope_empty =
  let global = createGlobalScope
  in scopeName global === "global" .&&.
     Set.null (scopeVariables global) .&&.
     Set.null (scopeFunctions global) .&&.
     parentScope global === Nothing

-- | Syntax validator should be constructible
prop_syntax_validator_constructible :: Property
prop_syntax_validator_constructible =
  let validator = newSyntaxValidator
  in validator `seq` True

-- | Syntax validator should have correct initial state
prop_syntax_validator_initial_state :: Property
prop_syntax_validator_initial_state =
  let validator = newSyntaxValidator
  in null (validatorErrors validator) .&&.
     scopeName (currentScope validator) === "global" .&&.
     null (scopeStack validator) .&&.
     null (braceStack validator) .&&.
     language validator === Unknown .&&.
     null (tokens validator) .&&.
     not (hasPackageDecl validator) .&&.
     not (hasMainFunc validator)

-- | Language detection should be consistent
prop_language_detection :: String -> Property
prop_language_detection code =
  let lang1 = detectLanguage code
      lang2 = detectLanguage code
  in lang1 === lang2

-- | Empty code should be handled gracefully
prop_empty_code_handling :: Property
prop_empty_code_handling =
  let errors = validateSyntax ""
  in errors `seq` True  -- Should not crash

-- | Large code should be handled
prop_large_code_handling :: Positive Int -> Property
prop_large_code_handling (Positive size) =
  let largeCode = replicate size 'a' ++ "\nfunc main() {}"
      errors = validateSyntax largeCode
  in errors `seq` True

-- | Code with only whitespace should be handled
prop_whitespace_only_handling :: Property
prop_whitespace_only_handling =
  let whitespaceCode = "   \n\t  \n   "
      errors = validateSyntax whitespaceCode
  in errors `seq` True

-- | Code with only comments should be handled
prop_comments_only_handling :: Property
prop_comments_only_handling =
  let commentCode = "// line comment\n/* block comment */\n// another comment"
      errors = validateSyntax commentCode
  in errors `seq` True

-- | Unbalanced braces should be detected
prop_unbalanced_braces :: Property
prop_unbalanced_braces =
  let unbalancedCode = "func main() {\n  if true {\n    println(\"hello\")\n  // missing closing brace"
      errors = validateSyntax unbalancedCode
      hasBraceError = any (\err -> errorType err == MissingBrace) errors
  in hasBraceError === True

-- | Unclosed strings should be detected
prop_unclosed_string :: Property
prop_unclosed_string =
  let unclosedCode = "func main() {\n  println(\"unclosed string\n}"
      errors = validateSyntax unclosedCode
      hasStringError = any (\err -> errorType err == UnclosedString) errors
  in hasStringError === True

-- | Unclosed comments should be detected
prop_unclosed_comment :: Property
prop_unclosed_comment =
  let unclosedCode = "func main() {\n  /* unclosed comment\n}"
      errors = validateSyntax unclosedCode
      hasCommentError = any (\err -> errorType err == UnclosedComment) errors
  in hasCommentError === True

-- | Valid Go code should not have syntax errors
prop_valid_go_code :: Property
prop_valid_go_code =
  let validCode = "package main\n\nimport \"fmt\"\n\nfunc main() {\n  fmt.Println(\"Hello, World!\")\n}"
      errors = validateSyntax validCode
  in null errors

-- | Error formatting should be deterministic
prop_error_formatting_deterministic :: SyntaxError -> Property
prop_error_formatting_deterministic err =
  let formatted1 = formatSyntaxError err
      formatted2 = formatSyntaxError err
  in formatted1 === formatted2

-- | Error formatting should contain line and column
prop_error_formatting_contains_location :: SyntaxError -> Property
prop_error_formatting_contains_location err =
  let formatted = formatSyntaxError err
      lineStr = show $ lineNumber err
      colStr = show $ columnNumber err
  in lineStr `isInfixOf` formatted .&&. colStr `isInfixOf` formatted

-- | Error formatting should contain error type
prop_error_formatting_contains_type :: SyntaxError -> Property
prop_error_formatting_contains_type err =
  let formatted = formatSyntaxError err
      typeStr = show $ errorType err
  in typeStr `isInfixOf` formatted

-- | Scope stack operations should be consistent
prop_scope_stack_consistency :: [Scope] -> Property
prop_scope_stack_consistency scopes =
  let validator = newSyntaxValidator { scopeStack = scopes }
      stackSize = length $ scopeStack validator
  in stackSize === length scopes

-- | Brace stack operations should be consistent
prop_brace_stack_consistency :: [(Char, Int, Int)] -> Property
prop_brace_stack_consistency braces =
  let validator = newSyntaxValidator { braceStack = braces }
      stackSize = length $ braceStack validator
  in stackSize === length braces

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Syntax Validator Boundary QuickCheck Tests"
  [ testProperty "syntax error preserves all components" prop_syntax_error_preserves
  , testProperty "syntax error ordering is consistent" prop_syntax_error_ordering
  , testProperty "token preserves position information" prop_token_preserves_position
  , testProperty "token types are distinguishable" prop_token_types_distinguishable
  , testProperty "scope preserves declarations" prop_scope_preserves_declarations
  , testProperty "global scope is empty" prop_global_scope_empty
  , testProperty "syntax validator is constructible" prop_syntax_validator_constructible
  , testProperty "syntax validator has correct initial state" prop_syntax_validator_initial_state
  , testProperty "language detection is consistent" prop_language_detection
  , testProperty "empty code handled gracefully" prop_empty_code_handling
  , testProperty "large code handled" prop_large_code_handling
  , testProperty "whitespace only handled" prop_whitespace_only_handling
  , testProperty "comments only handled" prop_comments_only_handling
  , testProperty "unbalanced braces detected" prop_unbalanced_braces
  , testProperty "unclosed strings detected" prop_unclosed_string
  , testProperty "unclosed comments detected" prop_unclosed_comment
  , testProperty "valid Go code has no syntax errors" prop_valid_go_code
  , testProperty "error formatting is deterministic" prop_error_formatting_deterministic
  , testProperty "error formatting contains location" prop_error_formatting_contains_location
  , testProperty "error formatting contains type" prop_error_formatting_contains_type
  , testProperty "scope stack operations are consistent" prop_scope_stack_consistency
  , testProperty "brace stack operations are consistent" prop_brace_stack_consistency
  ]